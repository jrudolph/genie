package net.virtualvoid.genie.tsp

import java.awt.geom.{ Path2D, Rectangle2D }
import java.awt.{ Color, Graphics2D, RenderingHints }
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

trait TspPlotter { self: TSP =>
  def extraSpace = 1.1d
  def plotDimensions = (1000, 1000)
  def citySize = 5d
  lazy val width = plotDimensions._1
  lazy val height = plotDimensions._2

  lazy val minX = cities.minBy(_.position._1).position._1
  lazy val maxX = cities.maxBy(_.position._1).position._1
  lazy val minY = cities.minBy(_.position._2).position._2
  lazy val maxY = cities.maxBy(_.position._2).position._2
  lazy val rawExtents = ((maxX - minX), (maxY - minY))
  lazy val xOrigin = minX
  lazy val yOrigin = minY

  println(s"minX: $minX maxX: $maxX minY: $minY maxY: $maxY rawExtents: $rawExtents")
  println(s"width: $width height: $height xOrigin: $xOrigin yOrigin: $yOrigin")

  def posOf(city: City): (Double, Double) = {
    val x = (city.position._1 - xOrigin) / rawExtents._1 * width.toDouble
    val y = (city.position._2 - yOrigin) / rawExtents._2 * height.toDouble
    (x, y)
  }

  def plotPath(path: Path, fileName: String): Unit = {
    val im = new BufferedImage(plotDimensions._1, plotDimensions._2, BufferedImage.TYPE_INT_RGB)
    val g2d = im.getGraphics.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setBackground(Color.WHITE)
    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, plotDimensions._1, plotDimensions._2)

    g2d.setColor(Color.BLACK)
    g2d.setBackground(Color.BLACK)

    val p = new Path2D.Double()
    var first = true
    path.pathCities.foreach { city =>
      val point = posOf(city)
      g2d.fill(new Rectangle2D.Double(point._1 - citySize / 2, point._2 - citySize / 2, citySize, citySize))
      if (first) {
        first = false
        p.moveTo(point._1, point._2)
      } else
        p.lineTo(point._1, point._2)
    }
    g2d.draw(p)

    g2d.dispose()

    ImageIO.write(im, "png", new File(fileName))
  }
}
