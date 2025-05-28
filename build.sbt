ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"



libraryDependencies += "io.github.dieproht" %% "matr-bundle" % "0.0.6"

//libraryDependencies ++= Seq(
//  "org.jliszka" %% "probability-monad" % "1.0.3"
//)
//

libraryDependencies ++= Seq(
  "com.github.vagmcs" %% "optimus" % "3.4.5",
  "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5",
  "com.github.vagmcs" %% "optimus-solver-lp" % "3.4.5"
)


lazy val root = (project in file("."))
  .settings(
    name := "election_sim"
  )
