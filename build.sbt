val scalaV = "2.12.8"
val specs2V = "4.3.2"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "io.spray" %% "spray-json" % "1.3.5",
  "org.specs2" %% "specs2-core" % specs2V % "test"
)

scalaVersion := scalaV

// docs

enablePlugins(ParadoxMaterialThemePlugin)

paradoxMaterialTheme in Compile := {
  ParadoxMaterialTheme()
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#changing-the-color-palette
    .withColor("light-green", "amber")
    // choose from https://jonas.github.io/paradox-material-theme/getting-started.html#adding-a-logo
    .withLogoIcon("cloud")
    .withCopyright("Copyleft © Johannes Rudolph")
    .withRepository(uri("https://github.com/jrudolph/xyz"))
    .withSocial(
      uri("https://github.com/jrudolph"),
      uri("https://twitter.com/virtualvoid")
    )
}

paradoxProperties ++= Map(
  "github.base_url" -> (paradoxMaterialTheme in Compile).value.properties.getOrElse("repo", "")
)

fork in run := true