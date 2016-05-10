package lt.vpranckaitis.image.analysis

case class Feature(concept: String, color: Seq[Int], histogram: Seq[Double], relativeEdges: Seq[Seq[Double]])

object FeatureDistance {
  private def square(x: Double) = x*x

  private def correlation(a: Seq[Double], b: Seq[Double]): Double = {
    def mean(x: Seq[Double]) = x.sum / x.size

    val meanA = mean(a)
    val meanB = mean(b)

    val varianceA = mean(a map { ai => square(ai - meanA) })
    val varianceB = mean(b map { bi => square(bi - meanB) })

    val covariance = mean(a zip b map { case (ai, bi) => (ai - meanA)*(bi - meanB) })
    covariance / (Math.sqrt(varianceA) * Math.sqrt(varianceB))
  }

  def apply(a: Feature, b: Feature): Double = {
    val colorDiff = (a.color zip b.color map { case (c1, c2) => square(c1 - c2) } ).sum
    val histogramCorrelation = correlation(a.histogram, b.histogram)
    val edgeDiff = (a.relativeEdges.flatten zip b.relativeEdges.flatten map { case (ai, bi) => square(ai - bi) }).sum

    val A = 1
    val B = -1000
    val C = 100000

    //println(s"${A*colorDiff} + ${B*histogramCorrelation} + ${C*edgeDiff}")

    A*colorDiff + B*histogramCorrelation + C*edgeDiff
  }
}
