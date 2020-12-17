import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion = "2.1.1"

  val compile: Seq[ModuleID] = Seq(
//    "uk.gov.hmrc"       %% "auth-client"                     % "3.2.0-play-26", // To be removed when bootstrap upgrades dependencies
    "uk.gov.hmrc"       %% "bootstrap-play-26"               % "2.2.0",
    "org.reactivemongo" %% "play2-reactivemongo"             % "0.20.11-play26",
    "com.typesafe.play" %% "play-iteratees"                  % "2.6.1",
    "com.typesafe.play" %% "play-iteratees-reactive-streams" % "2.6.1",
    "org.typelevel"     %% "cats-core"                       % catsVersion,
    "org.json"          % "json"                             % "20200518"
  )

  val test: Seq[ModuleID] = Seq(
    "org.mockito"            % "mockito-core"          % "3.3.3",
    "org.scalatest"          %% "scalatest"            % "3.2.0",
    "com.typesafe.play"      %% "play-test"            % current,
    "org.pegdown"            % "pegdown"               % "1.6.0",
    "org.scalatestplus.play" %% "scalatestplus-play"   % "3.1.3",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.14.3",
    "com.github.tomakehurst" % "wiremock-standalone"   % "2.27.1",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.0.2",
    "org.typelevel"          %% "discipline-scalatest" % "1.0.1",
    "com.vladsch.flexmark"   % "flexmark-all"          % "0.35.10"
  ).map(_ % "test, it")
}
