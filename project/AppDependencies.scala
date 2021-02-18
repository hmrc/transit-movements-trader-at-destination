import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion = "2.1.1"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% "bootstrap-backend-play-27"               % "3.3.0",
    "org.reactivemongo" %% "play2-reactivemongo"             % "0.20.11-play26",
    "org.reactivemongo" %% "reactivemongo-akkastream"        % "0.20.11",
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
    "org.scalatestplus.play" %% "scalatestplus-play"   % "4.0.3",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.14.3",
    "com.github.tomakehurst" % "wiremock-standalone"   % "2.27.1",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.0.2",
    "org.typelevel"          %% "discipline-scalatest" % "1.0.1",
    "com.vladsch.flexmark"   % "flexmark-all"          % "0.35.10",
    "com.typesafe.akka"      %% "akka-stream-testkit"  % "2.5.26"
  ).map(_ % "test, it")
}
