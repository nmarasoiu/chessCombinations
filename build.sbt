name := "chesspieces"
version := "0.1"
scalaVersion := "2.12.8"

val enumeratumVersion="1.5.13"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)