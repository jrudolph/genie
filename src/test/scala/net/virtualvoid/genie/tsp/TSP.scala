package net.virtualvoid.genie.tsp

import net.virtualvoid.genie.{ Crossover, IntRange, Mutate, Sample }

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

  case class Path(indices: Vector[Int]) {
    //require(indices.toSet.size == numCities, s"Path should have $numCities distinct parts but had only ${indices.toSet.size}: ${indices.mkString(", ")}")

    def pathCities: Seq[City] = indices.map(cities)
    lazy val length: Double = {
      val cities = pathCities
      val fst = cities.head
      cities.tail.foldLeft((fst, 0d)) { (state, nextCity) =>
        val newDist = state._2 + distance(state._1, nextCity)
        (nextCity, newDist)
      }._2
    }
  }

  case class Path2(vertexIndices: Vector[Int]) {
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
  //implicit val intRanges: Vector[IntRange[Int]] = IntRange(0, 1, identity) +: ((numCities - 1) to 1 by -1).map(num => IntRange(0, num, identity)).toVector
  implicit def samplePath: Sample[Path] =
    Sample { random =>
      def select(remainingCities: Seq[Int], currentPath: List[Int]): Path =
        if (remainingCities.isEmpty) Path(currentPath.toVector)
        else {
          val selected = random.nextInt(remainingCities.size)
          val remaining = remainingCities.take(selected) ++ remainingCities.drop(selected + 1)
          select(remaining, remainingCities(selected) :: currentPath)
        }

      select((1 until numCities), 0 :: Nil)
      //Path(intRanges.map(_.sample(random)))
    }

  implicit val noPathCrossover: Crossover[Path] = Crossover.disable[Path]
  val crossoverPath: Crossover[Path] =
    Crossover { (path1, path2, random) =>
      val pivot1 = random.nextInt(numCities)
      val pivot2 = numCities - pivot1

      val new1Head = path1.indices.take(pivot1)
      val new1Remaining =
        path1.indices.drop(pivot1).toSet
      val new1Tail =
        path1.indices.filter(new1Remaining)

      val new2Tail = path2.indices.takeRight(pivot2)
      val new2Remaining = path2.indices.dropRight(pivot2).toSet
      val new2Head = path2.indices.filter(new2Remaining)

      (Path(new1Head ++ new1Tail), Path(new2Head ++ new2Tail))
    }

  def perPathMutationRate: Double = 1
  implicit val mutatePath: Mutate[Path] =
    Mutate { (path, random) =>
      if (random.nextDouble() < perPathMutationRate) {
        val indicesArray = path.indices.toArray
        val a = random.nextInt(numCities)
        val b = random.nextInt(numCities)
        val s = indicesArray(a)
        indicesArray(a) = indicesArray(b)
        indicesArray(b) = s

        Path(indicesArray.toVector)
      } else path
    }

  /*implicit val crossoverPath: Crossover[Path] =
    Crossover { (path1, path2, random) =>
      val pivot = random.nextInt(numCities)

      val fstChild = path1.vertexIndices.take(pivot) ++ path2.vertexIndices.drop(pivot)
      val sndChild = path2.vertexIndices.take(pivot) ++ path1.vertexIndices.drop(pivot)

      (Path(fstChild), Path(sndChild))
    }

  def perElementMutationRate: Double = 0.01
  implicit val mutatePath: Mutate[Path] =
    Mutate { (path, random) =>
      val indices = path.vertexIndices.zipWithIndex.map {
        case (i, idx) =>
          if (random.nextDouble() < perElementMutationRate)
            intRanges(idx).sample(random)
          else
            i
      }
      Path(indices)
    }*/
}