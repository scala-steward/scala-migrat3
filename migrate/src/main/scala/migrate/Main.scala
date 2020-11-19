package migrate

import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import compiler.interfaces.Scala3Compiler
import migrate.internal._
import migrate.utils.FileUtils
import migrate.utils.ScalaExtensions._
import migrate.utils.Timer._
import scalafix.interfaces.Scalafix
import scalafix.interfaces.ScalafixEvaluation

object Main {
  lazy val scalafix: Scalafix               = Scalafix.fetchAndClassloadInstance("2.13")
  lazy val scalafixClassLoader: ClassLoader = scalafix.getClass().getClassLoader()

  def migrate(
    sourceRoot: AbsolutePath,
    workspace: AbsolutePath,
    scala2Classpath: Classpath,
    scala2CompilerOptions: Seq[String],
    toolClasspath: Classpath,
    scala3Classpath: Classpath,
    scala3CompilerOptions: Seq[String],
    scala3ClassDirectory: AbsolutePath
  ): Try[Map[AbsolutePath, FileMigrationState.FinalState]] = {
    val filesToMigrate = FileUtils.listFiles(sourceRoot)
    scribe.info(s"Migrating ${filesToMigrate.toList}")
    for {
      compiler <- setupScala3Compiler(scala3Classpath, scala3ClassDirectory, scala3CompilerOptions)
      initialFileToMigrate <-
        buildMigrationFiles(workspace, filesToMigrate, scala2Classpath, toolClasspath, scala2CompilerOptions)
      _            <- compileInScala3(initialFileToMigrate, compiler)
      migratedFiles = initialFileToMigrate.map(f => (f.source, f.migrate(compiler))).toMap
    } yield migratedFiles
  }

  def previewMigration(
    filetoMigrate: AbsolutePath,
    migratedFiles: Map[AbsolutePath, FileMigrationState.FinalState]
  ): Try[String] =
    migratedFiles.get(filetoMigrate).toTry(new Exception(s"Cannot find $filetoMigrate")).flatMap {
      case FileMigrationState.Failed(_, cause) => Failure(cause)
      case f: FileMigrationState.Succeeded     => f.newFileContent
    }

  private def setupScala3Compiler(
    classpath: Classpath,
    classDirectory: AbsolutePath,
    scala3CompilerOptions: Seq[String]
  ): Try[Scala3Compiler] = {
    // It's easier no to deal with semanticdb option, since we don't need semanticdb files
    val modified           = scala3CompilerOptions.filterNot(_ == "-Ysemanticdb")
    val scala3CompilerArgs = modified.toArray ++ Array("-classpath", classpath.value, "-d", classDirectory.value)
    Try {
      Scala3Compiler.setup(scala3CompilerArgs)
    }
  }

  private def compileInScala3(migrationFiles: Seq[FileMigrationState], compiler: Scala3Compiler): Try[Unit] =
    for {
      compilationUnits <- migrationFiles.map(_.previewAllPatches()).sequence
      _ <- timedMs {
             Try {
               compiler.compile(compilationUnits.toList)
             }
           } match {
             case (success @ Success(_), finiteduration) =>
               scribe.info(s"Compiled ${migrationFiles.size} file(s) successfully after ${finiteduration}")
               success
             case (failure @ Failure(cause), _) =>
               scribe.error(s"Compilation failed", cause)
               failure
           }
    } yield ()

  private def buildMigrationFiles(
    workspace: AbsolutePath,
    sources: Seq[AbsolutePath],
    classpath: Classpath,
    toolClasspath: Classpath,
    compilerOptions: Seq[String]
  ): Try[Seq[FileMigrationState.Initial]] =
    for {
      fileEvaluations <- timedMs {
                           inferTypes(workspace, sources, classpath, toolClasspath, compilerOptions)
                         } match {
                           case (Success(evaluation), finiteDuration) =>
                             val fileEvaluations = evaluation.getFileEvaluations().toSeq
                             val patchesCount    = fileEvaluations.map(_.getPatches().size).sum
                             scribe.info(
                               s"Found $patchesCount patch candidate(s) in ${sources.size} file(s) after $finiteDuration"
                             )
                             Success(fileEvaluations)

                           case (Failure(cause), _) =>
                             scribe.error("Failed inferring types", cause)
                             Failure(cause)
                         }
      fileEvaluationMap <- fileEvaluations
                             .map(e => AbsolutePath.from(e.getEvaluatedFile()).map(file => file -> e))
                             .sequence
                             .map(_.toMap)
      fileToMigrate <- sources.map(src => fileEvaluationMap.get(src).map(FileMigrationState.Initial).toTry).sequence
    } yield fileToMigrate

  private def inferTypes(
    workspace: AbsolutePath,
    sources: Seq[AbsolutePath],
    classpath: Classpath,
    toolClasspath: Classpath,
    compilerOptions: Seq[String]
  ): Try[ScalafixEvaluation] = Try {
    val args = scalafix
      .newArguments()
      .withRules(Seq("MigrationRule", "ExplicitImplicits").asJava)
      .withPaths(sources.map(_.toNio).asJava)
      .withClasspath(classpath.paths.map(_.toNio).asJava)
      .withScalacOptions(compilerOptions.asJava) // not sure which compiler option we need here !!
      .withToolClasspath(toolClasspath.toUrlClassLoader(scalafixClassLoader))
      .withSourceroot(workspace.toNio)
    args.evaluate()
  }
}
