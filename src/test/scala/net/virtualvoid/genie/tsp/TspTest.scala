package net.virtualvoid.genie.tsp

import java.nio.file.Paths

object TspTest extends App with TSP with TspFileLoader {
  lazy val cities: Vector[TspTest.City] = load(Paths.get("/home/maike/tmp/repos/genie/src/test/resources/mu1979.tsp"))
  println(s"Found $numCities cities")
}
