import play.sbt.routes.RoutesKeys
import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings

val appName = "transit-movements-trader-at-destination"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .configs(IntegrationTest)
  .settings(DefaultBuildSettings.integrationTestSettings())
  .settings(inConfig(IntegrationTest)(itSettings))
  .settings(inConfig(IntegrationTest)(scalafmtSettings))
  .settings(inConfig(IntegrationTest)(scalafixConfigSettings(IntegrationTest)))
  .settings(inConfig(Test)(testSettings): _*)
  .settings(inConfig(Test)(testSettings))
  .settings(inThisBuild(buildSettings))
  .settings(scoverageSettings)
  .settings(scalacSettings)
  .settings(
    majorVersion := 0,
    scalaVersion := "2.12.14",
    resolvers += Resolver.jcenterRepo,
    PlayKeys.playDefaultPort := 9480,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    javaOptions ++= Seq(
      "-Djdk.xml.maxOccurLimit=10000"
    ),
    // Import models for query string binding in routes file
    RoutesKeys.routesImport ++= Seq(
      "models._",
      "models.Binders._",
      "java.time.OffsetDateTime"
    )
  )

// Settings for the whole build
lazy val buildSettings = Def.settings(
  scalafmtOnCompile := true,
  useSuperShell := false,
  scalafixDependencies ++= Seq(
    "com.github.liancheng" %% "organize-imports" % "0.5.0"
  )
)

// Scalac options
lazy val scalacSettings = Def.settings(
  // Disable fatal warnings and warnings from discarding values
  scalacOptions ~= {
    opts =>
      opts.filterNot(Set("-Xfatal-warnings", "-Ywarn-value-discard"))
  },
  // Disable dead code warning as it is triggered by Mockito any()
  Test / scalacOptions ~= {
    opts =>
      opts.filterNot(Set("-Ywarn-dead-code"))
  },
  // Disable warnings arising from generated routing code
  scalacOptions += "-Wconf:src=routes/.*:silent"
)

lazy val scoverageSettings = Def.settings(
  Test / parallelExecution := false,
  ScoverageKeys.coverageMinimumStmtTotal := 82,
  ScoverageKeys.coverageExcludedFiles := "<empty>;.*javascript.*;.*Routes.*;",
  ScoverageKeys.coverageFailOnMinimum := true,
  ScoverageKeys.coverageHighlighting := true,
  ScoverageKeys.coverageExcludedPackages := Seq(
    """uk\.gov\.hmrc\.BuildInfo*""",
    """.*\.Routes""",
    """.*\.RoutesPrefix""",
    """.*\.Reverse[^.]*""",
    "testonly",
    "controllers.testOnly",
    "config.*",
    "logging"
  ).mkString(";")
)

lazy val itSettings = Def.settings(
  // Must fork so that config system properties are set
  fork := true,
  unmanagedSourceDirectories += (baseDirectory.value / "test" / "generators"),
  unmanagedResourceDirectories += (baseDirectory.value / "it" / "resources"),
  javaOptions ++= Seq(
    "-Dconfig.resource=it.application.conf",
    "-Dlogger.resource=it.logback.xml"
  )
)

lazy val testSettings = Def.settings(
  // Must fork so that config system properties are set
  fork := true,
  unmanagedResourceDirectories += (baseDirectory.value / "test" / "resources"),
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf",
    "-Dlogger.resource=logback-test.xml"
  )
)
