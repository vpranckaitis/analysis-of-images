package lt.vpranckaitis.image.analysis

import boofcv.alg.filter.derivative.GradientSobel
import boofcv.alg.misc.ImageStatistics
import boofcv.core.image.ConvertImage
import boofcv.core.image.border.FactoryImageBorderAlgs
import boofcv.struct.image.{GrayS16, GrayU8, Planar}

object ImageTools {
  implicit class RichPlanar(p: Planar[GrayU8]) {
    def slidingWindow(size: Int, step: Int) = {
      for {
        y <- 0 until (p.height - size) by step
        x <- 0 until (p.width - size) by step
        sub = p.subimage(x, y, x + size, y + size)
      } yield sub
    }

    def averageColor(): Seq[Int] = {
      p.bands map ImageStatistics.mean map { Math.round(_).toInt }
    }

    def histogram(): Seq[Double] = {
      val h = Array.fill(256)(0)
      val gray = ConvertImage.average(p, null)
      ImageStatistics.histogram(gray, h)
      (h grouped 8).toList map { _.sum } map { _ / (p.width*p.height).toDouble }
    }

    def relativeNumberOfEdges(size: Int): Seq[Seq[Double]] = {
      val gray = ConvertImage.average(p, null)
      val derivX = new GrayS16(p.width, p.height)
      val derivY = new GrayS16(p.width, p.height)

      GradientSobel.process(gray, derivX, derivY, FactoryImageBorderAlgs.extend(gray))

      def gradientMagnitude(x: Int, y: Int) = {
        val g1 = derivX.get(x, y)
        val g2 = derivY.get(x, y)
        Math.sqrt(g1*g1 + g2*g2)
      }

      val magnitudes = for {
        y <- 0 until p.height
        xs = 0 until p.width
      } yield xs map { gradientMagnitude(_, y) }

      val MagnitudeThreshold = 1024 / 3
      val edges = magnitudes map { _ map { m => if (m > MagnitudeThreshold) 1 else 0 } }

      val w = (p.width - size) / 2
      val ww = 2*w + 1

      def sumsOfWindows(xs: Seq[Int]) = (xs sliding ww).toSeq take size map { _.sum }

      val horizontalWindows = edges map sumsOfWindows
      val squareWindows = edges.transpose map sumsOfWindows

      squareWindows.transpose map { _ map { _ / (ww * ww).toDouble } }
    }
  }
}