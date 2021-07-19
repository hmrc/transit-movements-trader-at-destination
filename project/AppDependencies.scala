import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion    = "2.5.0"
  private val mongockVersion = "4.3.8"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-27"       % "3.4.0",
    "org.reactivemongo"            %% "play2-reactivemongo"             % "0.20.11-play27",
    "org.reactivemongo"            %% "reactivemongo-akkastream"        % "0.20.11",
    "com.typesafe.play"            %% "play-iteratees"                  % "2.6.1",
    "com.typesafe.play"            %% "play-iteratees-reactive-streams" % "2.6.1",
    "org.typelevel"                %% "cats-core"                       % catsVersion,
    "org.json"                      % "json"                            % "20200518",
    "com.github.cloudyrock.mongock" % "mongock-standalone"              % mongockVersion,
    "com.github.cloudyrock.mongock" % "mongodb-sync-v4-driver"          % mongockVersion,
    "org.mongodb"                   % "mongodb-driver-sync"             % "4.0.5",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-play-27"              % "0.51.0"
  )

  val test: Seq[ModuleID] = Seq(
    "org.mockito"             % "mockito-core"         % "3.3.3",
    "org.scalatest"          %% "scalatest"            % "3.2.8",
    "com.typesafe.play"      %% "play-test"            % current,
    "org.pegdown"             % "pegdown"              % "1.6.0",
    "org.scalatestplus.play" %% "scalatestplus-play"   % "4.0.3",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.14.3",
    "com.github.tomakehurst"  % "wiremock-standalone"  % "2.27.2",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.1.4",
    "org.typelevel"          %% "discipline-scalatest" % "2.1.4",
    "com.vladsch.flexmark"    % "flexmark-all"         % "0.35.10",
    "com.typesafe.akka"      %% "akka-stream-testkit"  % "2.5.31"
  ).map(_ % "test, it")
}
