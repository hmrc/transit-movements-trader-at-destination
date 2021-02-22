import play.sbt.routes.RoutesKeys
import uk.gov.hmrc.SbtArtifactory
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import scoverage.ScoverageKeys

val appName = "transit-movements-trader-at-destination"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .disablePlugins(JUnitXmlReportPlugin)
  .configs(IntegrationTest)
  .settings(inConfig(IntegrationTest)(itSettings): _*)
  .settings(inConfig(IntegrationTest)(scalafmtSettings): _*)
  .settings(inConfig(Test)(testSettings): _*)
  .settings(scalaVersion := "2.12.11")
  .settings(
    majorVersion := 0,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    scalafmtOnCompile in ThisBuild := true,
    useSuperShell in ThisBuild := false,
    javaOptions ++= Seq(
      "-Djdk.xml.maxOccurLimit=10000"
    )
  )
  .settings(headerSettings(IntegrationTest): _*)
  .settings(automateHeaderSettings(IntegrationTest))
  .settings(publishingSettings: _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(PlayKeys.playDefaultPort := 9480)
  .settings(scoverageSettings: _*)
  .settings(
    scalacOptions += "-Ypartial-unification"
  )
  .settings(RoutesKeys.routesImport ++= Seq(
    "models._"
  ))

lazy val scoverageSettings = {
  Seq(
    ScoverageKeys.coverageExcludedPackages := """uk\.gov\.hmrc\.BuildInfo*;.*\.Routes;.*\.RoutesPrefix;.*\.Reverse[^.]*;testonly;controllers.testOnly;config.*;logging""",
    ScoverageKeys.coverageMinimum := 84.00,
    ScoverageKeys.coverageExcludedFiles := "<empty>;.*javascript.*;.*Routes.*;",
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val itSettings = Defaults.itSettings ++ Seq(
  unmanagedSourceDirectories := Seq(
    baseDirectory.value / "it",
    baseDirectory.value / "test" / "generators"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "it" / "resources"
  ),
  parallelExecution := false,
  fork := true,
  javaOptions ++= Seq(
    "-Dconfig.resource=it.application.conf",
    "-Dlogger.resource=it.logback.xml"
  ),
  scalafmtTestOnCompile in ThisBuild := true
)

lazy val testSettings = Seq(
  fork := true,
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf",
    "-Dlogger.resource=logback-test.xml"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "test" / "resources"
  )
)
