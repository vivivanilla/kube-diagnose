import java.io.File

val scala3Version = "3.3.1"

// val targetPlatform = sys.env.get("TARGETPLATFORM").getOrElse("linux/x64")

name := "kube-diagnose"
version := "0.1.0-SNAPSHOT"
scalaVersion := scala3Version
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.3",
  "io.kubernetes" % "client-java" % "19.0.0",
  "com.monovore" %% "decline" % "2.4.1",
  "com.monovore" %% "decline-effect" % "2.4.1"
  // "org.graalvm.sdk" % "nativeimage" % "23.1.2"
)

fork := true
javaOptions ++= {
  if (isGraal)
    Seq(
      s"-agentlib:native-image-agent=config-merge-dir=${graalVMConfigDir.value}"
    )
  else
    Seq()
}

enablePlugins(GraalVMNativeImagePlugin)
val isGraal = System.getProperty("java.vendor").startsWith("GraalVM")
lazy val graalVMConfigDir = settingKey[File]("GraalVM configuration directory")
graalVMConfigDir := target.value / "graalvm-config"
graalVMNativeImageOptions ++= Seq(
  // "--static",
  "--no-fallback",
  s"-H:ReflectionConfigurationFiles=${graalVMConfigDir.value}/reflect-config.json"
)

semanticdbEnabled := true

enablePlugins(DockerPlugin)
dockerUsername := Some("vivivanilla")
