package net.virtualvoid.genie.tsp

import java.io.File
import java.nio.file.{ Path => FPath }

import scala.io.Source

trait TspFileLoader { self: TSP =>
  def load(tspFile: FPath): Vector[City] = {
    println(s"Loading $tspFile")
    Source.fromFile(tspFile.toFile).getLines()
      .dropWhile(_ != "NODE_COORD_SECTION")
      .drop(1)
      .filter(_ != "EOF")
      .map { line =>
        val Array(id, x, y) = line.split(" ")
        City(id, (x.toDouble, y.toDouble))
      }
      .toVector
  }
}
