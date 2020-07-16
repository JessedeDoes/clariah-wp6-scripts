name := "CLARIAH_wp6"

version := "0.1"

scalaVersion := "2.12.3"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml_2.12

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

// https://mvnrepository.com/artifact/org.incava/java-diff
libraryDependencies += "org.incava" % "java-diff" % "1.1"


libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.8.0-4"

// https://mvnrepository.com/artifact/org.json4s/json4s-native_2.12
libraryDependencies += "org.json4s" % "json4s-native_2.12" % "3.6.6"


// https://mvnrepository.com/artifact/org.json4s/json4s-xml
libraryDependencies += "org.json4s" %% "json4s-xml" % "3.6.6"
// was 3.5.3

// https://mvnrepository.com/artifact/org.json4s/json4s-jackson
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.6"

// https://mvnrepository.com/artifact/javax.servlet/javax.servlet-api
libraryDependencies += "javax.servlet" % "javax.servlet-api" % "4.0.0" % "provided"

// https://mvnrepository.com/artifact/org.ow2.sat4j/org.ow2.sat4j.core


// https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/WordUtils.html


// https://mvnrepository.com/artifact/org.apache.commons/commons-text
libraryDependencies += "org.apache.commons" % "commons-text" % "1.8"


/*
https://github.com/iovka/shex-java

<dependency>
  	<groupId>fr.inria.lille.shexjava</groupId>
  	<artifactId>shexjava-core</artifactId>
  	<version>1.0</version>
 </dependency>
*/

// https://mvnrepository.com/artifact/fr.inria.lille.shexjava/shexjava-core
// libraryDependencies += "fr.inria.lille.shexjava" % "shexjava-core" % "1.0"

//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.11.1"
//libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.1"


assemblyMergeStrategy in assembly := {
    case x if Assembly.isConfigFile(x) =>
      MergeStrategy.concat
    case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
        case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
          MergeStrategy.discard
        case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.first
      }
    case _ => MergeStrategy.first
  }


        
