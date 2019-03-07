name := "chesspieces"
version := "0.1"
scalaVersion := "2.12.8"

val monixVersion = "2.3.3"
val enumeratumVersion = "1.5.13"
val scalaCheckVersion = "1.14.0"
val enumeratumScalacheckVersion = "1.5.15"
val scalacheckShapelessGenerators = "1.2.0-1"
libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "io.monix" %% "monix-reactive" % monixVersion,
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion /* % Test commented for enumeratum-scalacheck and/or scalacheckShapless-test*/ ,
  "com.beachape" % "enumeratum-scalacheck_2.12" % enumeratumScalacheckVersion % Test,
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % scalacheckShapelessGenerators % Test
)