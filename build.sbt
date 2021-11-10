name := "minichain-solution"

scalaVersion := "2.13.7"

val Versions =
  new {
    val catsEffect = "3.2.9"
    val scalatest = "3.1.4"
    val catsEffectTesting = "1.3.0"
  }

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % Versions.catsEffect,
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
  "org.typelevel" %% "cats-effect-testing-scalatest" % Versions.catsEffectTesting % Test
)
