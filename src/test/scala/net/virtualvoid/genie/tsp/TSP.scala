package net.virtualvoid.genie.tsp

import net.virtualvoid.genie.{ Crossover, IntRange, Sample }

import scala.annotation.tailrec

trait TSP {
  case class City(name: String, position: (Double, Double))

  def distance(city1: City, city2: City): Double = {
    val xDiff = city1.position._1 - city2.position._1 toDouble
    val yDiff = city1.position._2 - city2.position._2 toDouble

    math.sqrt(xDiff * xDiff + yDiff * yDiff)
  }

  def numCities: Int = cities.size
  def cities: Vector[City]

  case class Path(vertexIndices: Vector[Int]) {
    lazy val pathCities: Seq[City] = {
      @tailrec def rec(remainingPath: Vector[Int], remainingCities: Vector[City], reversePath: List[City]): Seq[City] =
        if (remainingPath.isEmpty) {
          require(reversePath.size == numCities)
          reversePath.reverse.toVector
        } else {
          val selected = remainingPath.head
          val newPath = remainingCities(selected) :: reversePath
          val newRemainingPath = remainingPath.tail
          val newRemainingCities = remainingCities.take(selected) ++ remainingCities.drop(selected + 1)
          require(newRemainingPath.size == remainingPath.size - 1)
          rec(newRemainingPath, newRemainingCities, newPath)
        }

      rec(vertexIndices, cities, Nil)
    }
    lazy val length: Double = {
      val fst = pathCities.head
      pathCities.tail.foldLeft((fst, 0d)) { (state, nextCity) =>
        val newDist = state._2 + distance(state._1, nextCity)
        (nextCity, newDist)
      }._2
    }
  }
  implicit val intRanges: Vector[IntRange[Int]] = (numCities to 1 by -1).map(num => IntRange(0, num, identity)).toVector
  implicit def samplePath: Sample[Path] =
    Sample { random =>
      Path(intRanges.map(_.sample(random)))
    }

  implicit val crossoverPath: Crossover[Path] =
    Crossover { (path1, path2, random) =>
      val pivot = random.nextInt(numCities)

      val fstChild = path1.vertexIndices.take(pivot) ++ path2.vertexIndices.drop(pivot)
      val sndChild = path2.vertexIndices.take(pivot) ++ path1.vertexIndices.drop(pivot)

      (Path(fstChild), Path(sndChild))
    }

  //implicit val mutatePath: Mutate[Path] =
}