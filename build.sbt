ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"


//libraryDependencies ++= Seq(
//  "org.jliszka" %% "probability-monad" % "1.0.3"
//)
//

lazy val root = (project in file("."))
  .settings(
    name := "election_sim"
  )
