name := "chesspieces"
version := "0.1"
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % "1.5.13", //enum
  "io.reactivex.rxjava2" % "rxjava" % "2.2.7",
  "org.scalaz" %% "scalaz-core" % "7.2.27", //memo
  "org.scalatest" % "scalatest_2.12" % "3.0.5" % Test
)