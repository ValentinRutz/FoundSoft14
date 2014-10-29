import sbt._
import Keys._

import java.io.File
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import scoverage.ScoverageSbtPlugin._

object ExerciseBuild extends Build {

  val exerciseSettings = Defaults.defaultSettings ++ Seq(
    organization := "fos",
    name         := "fos-project3",
    version      := "1.0",
    scalaVersion := "2.10.4",
    resolvers ++= Seq("Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"),
    scalacOptions ++= List("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.11.5" % "test"))


  val filesToInclude = Seq("src/main/scala/fos/SimplyTyped.scala", "src/main/scala/fos/Terms.scala")

  val packageToMoodle = InputKey[Unit]("package-for-submission", "Package all files necessary for submission, given your surnames. For example 'Plociniczak Jovanovic'")

  val packageToMoodleTask: Setting[InputTask[Unit]] = packageToMoodle <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
    (argTask, baseDirectory, target, packageOptions, cacheDirectory, streams, compile in Compile) map { (args: Seq[String], base, outDir, po, cDir, s, comp) =>

      if (args.length > 3 || args.length < 1)
        println("Invalid number of students in a group. Please provide your last names separated by a whitespace")
      else {
        val suffix = args.map(name => name.slice(0,3)).mkString
        val out = new File(outDir, "submission" + suffix + ".jar")
        gatherSources(out, po, cDir, s.log)
      }

    }
  }

  lazy val root = Project("fos-project2", file("."))
  .settings( (exerciseSettings 
                ++ Seq(packageToMoodleTask) 
                ++ scalariformConfig
                ++ scoverageSettings) : _*)

  private def gatherSources(out: File, po: Seq[PackageOption], cacheDir: File, log: Logger): File = {
    val packagePrefix = "src/fos"
    val mappings = filesToInclude.flatMap{src =>
      val f = new File(src)

      if (f.exists()) {
        Some((f, packagePrefix + "/" + f.getName))
      }  else {
        println("Required file " + src + " does not exist!")
        None
      }
    }

    if (filesToInclude.length != mappings.length) {
      out
    } else {
      // println info for students
      println("Files to be included in the submission: '" + filesToInclude.mkString(",") + "'")
      val config = new Package.Configuration(mappings, out, po)
      Package(config, cacheDir, log)
      out
    }

  }

  def scalariformConfig = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences := FormattingPreferences()
        .setPreference(AlignParameters, true)
        .setPreference(AlignSingleLineCaseStatements, false)
        .setPreference(CompactControlReadability, false)
        .setPreference(CompactStringConcatenation, false)
        .setPreference(DoubleIndentClassDeclaration, true)
        .setPreference(FormatXml, true)
        .setPreference(IndentLocalDefs, false)
        .setPreference(IndentPackageBlocks, true)
        .setPreference(IndentSpaces, 4)
        .setPreference(IndentWithTabs, false)
        .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)
        .setPreference(PreserveDanglingCloseParenthesis, false)
        .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
        .setPreference(PreserveSpaceBeforeArguments, false)
        .setPreference(RewriteArrowSymbols, false)
        .setPreference(SpaceBeforeColon, false)
        .setPreference(SpaceInsideBrackets, false)
        .setPreference(SpaceInsideParentheses, false)
        .setPreference(SpacesWithinPatternBinders, true)
  )

    def scoverageSettings = instrumentSettings ++ Seq(
        ScoverageKeys.highlighting := true
    )
}
