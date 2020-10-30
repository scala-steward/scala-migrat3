package migrate

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin
import java.nio.file.{ Files, Path, Paths }

import migrate.interfaces.Migrate

import scala.collection.JavaConverters._

case class Scala3Inputs(scalacOptions: Seq[String], classpath: Seq[Path], classDirectory: Path)

object ScalaMigratePlugin extends AutoPlugin {
  val classpathAttribute      = AttributeKey[Seq[Path]]("unmanagedClasspath")
  val scalacOptionAttribute   = AttributeKey[Seq[String]]("scalacOptions")
  val classDirectoryAttribute = AttributeKey[Path]("scala3ClassDirectory")

  object autoImport {
    // val scala3CompilerOptions = taskKey[Seq[String]]("scalacOptions for scala 3")
    val scala3Version     = "0.27.0-RC1"
    val checkRequirements = taskKey[Unit]("check requirements")
    val migrate           = taskKey[Unit]("migrate a specific project to scala 3")
    val scala3Inputs      = taskKey[Scala3Inputs]("compute scala 3 classpath and options")
    val storeScala3Inputs = taskKey[StateTransform]("store scala 3 classpath and options in state attributes")
  }
  import autoImport._

  override def requires: Plugins = JvmPlugin

  override def trigger = AllRequirements

  val configSettings: Seq[Setting[_]] =
    Def.settings(
      migrate := migrateImp.value,
      scala3Inputs := {
        val state1 = Command.process(s"++ ${scala3Version}! ", state.value)
        val state2 = Command.process("storeScala3Inputs", state1)
        (for {
          classpath      <- state2.attributes.get(classpathAttribute)
          scalacOptions  <- state2.attributes.get(scalacOptionAttribute)
          classDirectory <- state2.attributes.get(classDirectoryAttribute)
        } yield Scala3Inputs(scalacOptions, classpath, classDirectory)).get

      },
      storeScala3Inputs := {
        val classpath            = (Compile / managedClasspath).value.seq.map(_.data.toPath())
        val soptions             = (Compile / scalacOptions).value
        val scala3ClassDirectory = (Compile / compile / classDirectory).value.toPath
        StateTransform(s =>
          s.put(classpathAttribute, classpath)
            .put(scalacOptionAttribute, soptions)
            .put(classDirectoryAttribute, scala3ClassDirectory)
        )
      }
    )

  override def projectSettings: Seq[Setting[_]] =
    inConfig(Compile)(configSettings) ++
      inConfig(Test)(configSettings)

  lazy val migrateImp = Def.task {
    val log = streams.value.log
    log.info("we are going to migrate your project to scala 3 maybe")

    val input                 = (Compile / sourceDirectory).value
    val workspace             = (ThisBuild / baseDirectory).value
    val scala2Classpath       = (Compile / fullClasspath).value.seq.map(_.data.toPath())
    val scala2CompilerOptions = (Compile / scalacOptions).value
    val semanticdbPath        = (Compile / semanticdbTargetRoot).value

    // computed values
    val scala3InputsValue = scala3Inputs.value
    val scalac3Options    = scala3InputsValue.scalacOptions
    val scala3Classpath   = scala3InputsValue.classpath
    val scala3ClassDir    = scala3InputsValue.classDirectory
    if (!Files.exists(scala3ClassDir)) Files.createDirectory(scala3ClassDir)

    // hardcoded values ...
    val toolClasspath = Seq(
      "/Users/meriamlachkar/scalacenter/scala-migrat3/scalafix/rules/target/scala-2.13/classes",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.3/scala-library-2.13.3.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/scalafix-core_2.13/0.9.23/scalafix-core_2.13-0.9.23.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/scalameta_2.13/4.3.24/scalameta_2.13-4.3.24.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/googlecode/java-diff-utils/diffutils/1.3.0/diffutils-1.3.0.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/geirsson/metaconfig-typesafe-config_2.13/0.9.10/metaconfig-typesafe-config_2.13-0.9.10.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-collection-compat_2.13/2.2.0/scala-collection-compat_2.13-2.2.0.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/parsers_2.13/4.3.24/parsers_2.13-4.3.24.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scalap/2.13.3/scalap-2.13.3.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/geirsson/metaconfig-core_2.13/0.9.10/metaconfig-core_2.13-0.9.10.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/config/1.2.1/config-1.2.1.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/trees_2.13/4.3.24/trees_2.13-4.3.24.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.13.3/scala-compiler-2.13.3.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/paiges-core_2.13/0.3.0/paiges-core_2.13-0.3.0.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/pprint_2.13/0.5.9/pprint_2.13-0.5.9.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/common_2.13/4.3.24/common_2.13-4.3.24.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/scalapb-runtime_2.13/0.10.8/scalapb-runtime_2.13-0.10.8.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/fastparse_2.13/1.0.1/fastparse_2.13-1.0.1.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.3/scala-reflect-2.13.3.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline/3.15.0/jline-3.15.0.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fansi_2.13/0.2.9/fansi_2.13-0.2.9.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/sourcecode_2.13/0.2.1/sourcecode_2.13-0.2.1.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/lenses_2.13/0.10.8/lenses_2.13-0.10.8.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.11.4/protobuf-java-3.11.4.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fastparse_2.13/2.3.0/fastparse_2.13-2.3.0.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/fastparse-utils_2.13/1.0.1/fastparse-utils_2.13-1.0.1.jar",
      "/Users/meriamlachkar/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/geny_2.13/0.6.0/geny_2.13-0.6.0.jar"
    ).map(Paths.get(_))

    val migrateAPI     = Migrate.fetchAndClassloadInstance()
    val migrateService = migrateAPI.getService()

    migrateService.migrate(
      input.toPath(),
      workspace.toPath(),
      scala2Classpath.asJava,
      scala2CompilerOptions.asJava,
      toolClasspath.asJava,
      scala3Classpath.asJava,
      scalac3Options.asJava,
      scala3ClassDir
    )
  }
}
