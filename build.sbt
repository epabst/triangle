name := "Triangle"

organization := "com.github.epabst.triangle"

version := "0.6-SNAPSHOT"

scalaVersion := "2.8.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.1"

libraryDependencies += "org.slf4j" % "slf4j-jdk14" % "1.6.1" % "test"

libraryDependencies += "org.mockito" % "mockito-core" % "1.8.5" % "test"

libraryDependencies += "junit" % "junit" % "4.8.2" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

//todo eliminate easymock as a dependency
libraryDependencies += "org.easymock" % "easymock" % "2.5.2" % "test"

