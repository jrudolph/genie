package net.virtualvoid.genie.tsp

import java.io.File
import java.nio.file.Paths

import net.virtualvoid.genie.Sample

import scala.util.Random

object TspTest extends App with TSP with TspFileLoader with TspPlotter {
  lazy val cities: Vector[TspTest.City] = load(Paths.get("/home/maike/tmp/repos/genie/src/test/resources/qa194.tsp"))
  println(s"Found $numCities cities")
  val inOrder = Path(Vector.fill(numCities)(0))
  //inOrder.pathCities.foreach(println)
  println(s"Length when visiting in order: ${inOrder.length}")
  plotPathTo(inOrder, new File("inOrder.png"))
  val rand = new Random(10)
  val sampled = Vector.fill(100)(Sample.create[Path](rand))
  val lengths = sampled.map(_.length)
  println(s"Length when visiting randomly: min: ${sampled.minBy(_.length).length} max: ${sampled.maxBy(_.length).length}")
  plotPathTo(sampled(0), new File("random.png"))
  plotPathTo(sampled.minBy(_.length), new File("bestofrandom.png"))
}
