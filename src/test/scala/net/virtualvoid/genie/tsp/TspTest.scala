package net.virtualvoid.genie.tsp

import java.io.{ File, FileOutputStream }
import java.nio.file.Paths

import net.virtualvoid.genie.{ Crossover, Mutate, Sample }

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object TspTest extends App with TSP with TspFileLoader with TspPlotter {
  val populationSize = 2000
  val topN = 50
  val randomNewPerGeneration = 0
  val maxGenerations = 100

  lazy val cities: Vector[TspTest.City] = load(Paths.get("/home/maike/tmp/repos/genie/src/test/resources/qa194.tsp"))
  println(s"Found $numCities cities")
  val inOrder = Path((0 until numCities).toVector)
  val inOrderY = Path {
    cities.zipWithIndex.sortBy(_._1.position._2).map(_._2)
  }
  //inOrder.pathCities.foreach(println)
  println(s"Length when visiting in order: ${inOrder.length}")
  plotPath(inOrder, "inOrder.png")

  val rand = new Random(0)

  def sample(num: Int, random: Random): Vector[Path] =
    Vector.fill(num)(Sample.create[Path](random))

  def savePath(path: Path): Unit = {
    val target = new File(s"best/${path.length}-${path.hashCode()}.txt")
    if (!target.exists()) {
      val fos = new FileOutputStream(target)
      path.indices.foreach { i =>
        fos.write(i.toString.getBytes("utf8"))
        fos.write('\n')
      }
      fos.close()
    }
  }
  def loadPath(file: File): Path =
    Path(Source.fromFile(file).getLines.map(_.toInt).toVector)
  def loadPaths(): Seq[Path] = {
    val folder = new File("best")
    folder.listFiles()
      .filter(_.getName endsWith ".txt")
      .map(loadPath)
  }

  @tailrec
  def step(generation: Int, lastBest: Double, population: Vector[Path]): Unit = {
    population.par.foreach(_.length)
    val sorted = population.sortBy(_.length)
    println(f"[$generation%5d] best: ${sorted.head.length}%7.0f worst: ${sorted.last.length}%7.0f improvement: ${(lastBest / sorted.head.length - 1) * 100}%5.2f %%")
    plotPath(sorted.head, f"generational/gen$generation%05d.png")
    savePath(sorted.head)
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
  step(0, Double.MaxValue, sample(populationSize, rand) ++ loadPaths() :+ inOrder :+ inOrderY)

  /*val Seq(p1, p2) = sample(2, rand)
  val (n1, n2) = Crossover.cross[Path](p1, p2, rand)

  println(p1.indices.mkString(", "))
  println(p2.indices.mkString(", "))
  println(n1.indices.mkString(", "))
  println(n2.indices.mkString(", "))*/
}
