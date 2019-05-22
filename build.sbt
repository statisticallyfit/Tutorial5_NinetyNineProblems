name := "Tutorial5_NinetyNineProblems"

version := "1.0"

scalaVersion := "2.11.12" //"2.12.0-RC1" // 2.11.11

traceLevel := -1

logLevel := Level.Info

// disable printing timing information, but still print [success]
showTiming := false

//added here when suggested by sbt in command line
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

// disable printing a message indicating the success or failure of running a task
showSuccess := false

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-Xlog-implicits","-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-Ypartial-unification")

// disable updating dynamic revisions (including -SNAPSHOT versions)
offline := true


//For the Kind projector plugin
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")


libraryDependencies ++= Seq(
     //Scala Reflections
     "org.scala-lang" % "scala-reflect" % "2.11.11",
     //Specs2
     "org.specs2" %% "specs2-core" % "4.0.2" % Test,
     //Discipline
     "org.typelevel" %% "discipline" % "0.8",
     //Spire
     "org.typelevel" %% "spire" % "0.14.1",
     //Scalaz
     "org.scalaz"      %% "scalaz-core"    % "7.3.0-M19",
     //Cats
     //"org.typelevel"   %% "cats"           % "1.0.1",
     "org.typelevel"   %% "cats-core"           % "1.0.1",
     "org.typelevel"   %% "cats-macros"           % "1.0.1",
     "org.typelevel"   %% "cats-kernel"           % "1.0.1",
     "org.typelevel"   %% "cats-laws"           % "1.0.1",
     "org.typelevel"   %% "cats-free"           % "1.0.1",
     "org.typelevel"   %% "cats-testkit"           % "1.0.1",
     //Shapeless
     //"com.chuusai"     %% "shapeless"      % "2.3.3",
     //Kind projector plugin
     "org.spire-math" %% "kind-projector" % "0.9.4",
     //Apache Commons (Java)
     "org.apache.commons" % "commons-lang3" % "3.6",
     //Shapeless
     "org.typelevel" %% "shapeless-scalaz" % "0.4"
     //old stuff
     /*"org.specs2" % "specs2-core_2.11" % "3.8.4-20160711064123-77be371" % Test,
     "org.scala-lang" % "scala-reflect" % "2.11.8",,
     "org.typelevel" %% "spire" % "0.14.1"*/
)
