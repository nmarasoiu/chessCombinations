name := "chesspieces"
version := "0.1"
scalaVersion := "2.12.8"

//val monixVersion = "2.3.3"
val enumeratumVersion = "1.5.13"
val scalaTestVersion = "3.0.5"
val scalazVersion = "7.2.27"
//val scalaCheckVersion = "1.14.0"
//val enumeratumScalacheckVersion = "1.5.15"
//val scalacheckShapelessGenerators = "1.2.0-1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "io.reactivex.rxjava2" % "rxjava" % "2.2.7",
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalatest" % "scalatest_2.12" % scalaTestVersion % Test
)