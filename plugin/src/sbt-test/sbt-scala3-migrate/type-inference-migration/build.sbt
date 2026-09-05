lazy val `type-inference-migration` = project
  .in(file("."))
  .settings(
    scalaVersion      := "3.9.0",
    semanticdbVersion := "4.14.2"
  )
