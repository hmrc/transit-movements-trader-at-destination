import play.core.PlayVersion.current
import play.sbt.PlayImport._
import sbt.Keys.libraryDependencies
import sbt._

object AppDependencies {

  val compile = Seq(
    "org.reactivemongo" %% "play2-reactivemongo" % "0.18.6-play26",
    "uk.gov.hmrc"       %% "bootstrap-play-26"   % "1.4.0",
    "org.typelevel"     %% "cats-core"           % "2.0.0"
  )

  val test = Seq(
    "org.mockito"            % "mockito-all"         % "2.0.2-beta",
    "org.scalatest"          %% "scalatest"          % "3.0.8",
    "com.typesafe.play"      %% "play-test"          % current,
    "org.pegdown"            % "pegdown"             % "1.6.0",
    "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2",
    "org.scalacheck"         %% "scalacheck"         % "1.14.1",
    "com.github.tomakehurst" % "wiremock-standalone" % "2.17.0"
  ).map(_ % "test, it")

  val akkaVersion     = "2.5.23"
  val akkaHttpVersion = "10.0.15"

  val overrides = Seq(
    "com.typesafe.akka" %% "akka-stream"    % akkaVersion,
    "com.typesafe.akka" %% "akka-protobuf"  % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j"     % akkaVersion,
    "com.typesafe.akka" %% "akka-actor"     % akkaVersion,
    "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
  )
}
