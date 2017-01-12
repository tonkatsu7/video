name := "video"

version := "1.0"

scalaVersion := "2.12.1"

val scalazVersion = "7.2.8"

val scalazVersions = ("1.1.1", "7.2.8", "3.0.1") // matching versions: scalaz-scalatest, scalaz, scalatest

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % scalazVersions._2,
                            "org.scalaz" %% "scalaz-concurrent" % scalazVersions._2,
                            "org.scalactic" %% "scalactic" % scalazVersions._3,
                            "org.scalatest" %% "scalatest" % scalazVersions._3 % "test",
                            "org.typelevel" %% "scalaz-scalatest" % scalazVersions._1 % "test",
                            "org.jcodec" % "jcodec-javase" % "0.1.9")