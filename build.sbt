name := "minichain-solution"

scalaVersion := "2.13.7"

val Versions =
  new {
    val catsEffect = "3.2.9"
  }

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % Versions.catsEffect
)
