val scala3Version = "3.3.1"

val targetPlatform = sys.env.get("TARGETPLATFORM").getOrElse("linux/x64")

lazy val root = project
  .in(file("."))
  .settings(
    name := "kube-diagnose",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.3",
      "io.kubernetes" % "client-java" % "19.0.0",
      "com.monovore" %% "decline" % "2.4.1",
      "com.monovore" %% "decline-effect" % "2.4.1"
    ),

    graalVMNativeImageOptions ++= Seq(
      "--static",
      "--no-fallback"
    ),

    dockerUsername := Some("vivivanilla"),

    semanticdbEnabled := true
  )
  .enablePlugins(GraalVMNativeImagePlugin)
  .enablePlugins(DockerPlugin)
