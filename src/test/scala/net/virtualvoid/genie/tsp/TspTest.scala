package net.virtualvoid.genie.tsp

import java.nio.file.Paths

import net.virtualvoid.genie.{ Crossover, Mutate, Sample }

import scala.annotation.tailrec
import scala.util.Random

object TspTest extends App with TSP with TspFileLoader with TspPlotter {
  val populationSize = 200
  val topN = 2
  val randomNewPerGeneration = 10
  val maxGenerations = 100

  lazy val cities: Vector[TspTest.City] = load(Paths.get("/home/maike/tmp/repos/genie/src/test/resources/qa194.tsp"))
  println(s"Found $numCities cities")
  val inOrder = Path(Vector.fill(numCities)(0))
  //inOrder.pathCities.foreach(println)
  println(s"Length when visiting in order: ${inOrder.length}")
  plotPath(inOrder, "inOrder.png")

  val rand = new Random(0)

  def sample(num: Int, random: Random): Vector[Path] =
    Vector.fill(num)(Sample.create[Path](random))

  @tailrec
  def step(generation: Int, lastBest: Double, population: Vector[Path]): Unit = {
    population.par.foreach(_.length)
    val sorted = population.sortBy(_.length)
    println(f"[$generation%5d] best: ${sorted.head.length}%7.0f worst: ${sorted.last.length}%7.0f improvement: ${(lastBest / sorted.head.length - 1) * 100}%5.2f %%")
    plotPath(sorted.head, f"generational/gen$generation%05d.png")
    if (generation < maxGenerations /*&& sorted.head.length < lastBest*/ ) {
      val top = population.sortBy(_.length).take(topN)

      def randomPair(): (Path, Path) = {
        val fst = rand.nextInt(top.size)
        val snd = Iterator.continually(rand.nextInt(top.size)).find(_ != fst).get
        (top(fst), top(snd))
      }

      val newPopulationRaw =
        Vector.fill((populationSize - topN - randomNewPerGeneration) / 2) {
          val pair = randomPair()
          Crossover.cross[Path](pair._1, pair._2, rand)
        }.flatMap { case (a, b) => Seq(a, b) } ++
          sample(randomNewPerGeneration, rand)
      /*val newPopulationRaw =
        top ++ top ++ top ++
          sample(populationSize - 4 * topN, rand)*/

      val newPopulation = top ++ newPopulationRaw.map(p => Mutate.mutate(p, rand))
      step(generation + 1, sorted.head.length, newPopulation)
    } else {
      val best = sorted.head
      println(s"After $generation generations the best solution was ${best.length}")
      plotPath(best, "best.png")
    }
  }
  step(0, Double.MaxValue, sample(populationSize, rand) /*:+ inOrder*/ )
}
