resolvers += "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2"

resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(Resolver.ivyStylePatterns)

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "3.15.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "2.2.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.20")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.1")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.16")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.29")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.0")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.20")
