package lt.vpranckaitis.image.analysis

import boofcv.struct.image.{GrayU8, Planar}
import lt.vpranckaitis.image.analysis.ImageTools._

object Utils {
  def knn(features: Seq[Feature], target: Feature, k: Int): Seq[Feature] =
    features.toSeq map { ff => (FeatureDistance(target, ff), ff) } sortBy { _._1 } take k map { _._2 }

  def whitePercentage(p: Planar[GrayU8]) = {
    val whitePixels = for (y <- 0 until p.height; x <- 0 until p.width) yield {
      val brightness = (p.getBands map { _.get(x, y) }).sum / 3
      if (brightness > 230)
        1
      else
        0
    }

    whitePixels.sum.toDouble / (p.width * p.height)
  }

  def imageToFeature(image: Planar[GrayU8], concept: String) =
    Feature(concept, image.averageColor(), image.histogram(), image.relativeNumberOfEdges(2 << 4))

  def simpleString(ss: Seq[String]) = {
    val ordered = (ss groupBy identity mapValues { _.size }).toSeq.sortBy(_._2)(Ordering[Int].reverse)
    ordered map { case (s, n) => s"$n $s" } mkString ", "
  }

  def entropy(xs: Seq[String], base: Int): Double = {
    val counts = xs groupBy identity mapValues { _.size }
    def logn(base: Double)(a: Double) = Math.log(a) / Math.log(base)
    val logBase = logn(base)(_)
    val xn = xs.size.toDouble
    -(counts map { case (s, x) => (x / xn) * logBase(x / xn) }).sum
  }

  def inverseEntropy: (Seq[String], Int) => Double = 1 - entropy(_, _)
}
