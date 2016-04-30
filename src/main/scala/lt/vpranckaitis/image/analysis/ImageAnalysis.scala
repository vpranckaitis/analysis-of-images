package lt.vpranckaitis.image.analysis

import java.net.URL

import boofcv.gui.ListDisplayPanel
import boofcv.gui.image.ShowImages
import boofcv.io.image.{ConvertBufferedImage, UtilImageIO}
import boofcv.struct.image._
import lt.vpranckaitis.image.analysis.ImageTools._
import lt.vpranckaitis.image.analysis.Utils._
import org.imgscalr.Scalr

import scala.io.StdIn
import scala.util.{Failure, Try}

object ImageAnalysis extends App {

  val gui = new ListDisplayPanel()

  val concepts = List("apple", "banana", "basket", "blue-grapes",
    "green-grapes", "kiwi", "lemon", "orange", "pineapple", "strawberries")

  println("Loading images of concepts")

  val images = for {
    concept <- concepts
    k <- 1 to 4
    image = Scalr.resize(UtilImageIO.loadImage(s"pictures/$concept/$k.jpg"), Scalr.Method.QUALITY, 500)
  } yield (concept, image)

  val windows = images groupBy { _._1 } mapValues { _.unzip._2 } mapValues { imgs =>
    for {
      img <- imgs
      planar = ConvertBufferedImage.convertFromMulti(img, null, true, classOf[GrayU8])
      k <- 6 to 7
      size = 1 << k
      window <- planar.slidingWindow(size, size / 2)
    } yield window
  }

  val nonBackgroundWindows = windows mapValues { imgs =>
    imgs filter { img => whitePercentage(img) < 0.3 }
  }

  println("Extracting features")

  val features = for {
    (concept, images) <- nonBackgroundWindows
    image <- images
  } yield imageToFeature(image, concept)

  ShowImages.showWindow(gui,"Color Planar Image Examples",true)

  println("Input URL of the image:")

  val NumberOfNeighbours = 10

  def analyseImage(url: String): Unit = {
    val image = Scalr.resize(UtilImageIO.loadImage(new URL(url)), Scalr.Method.QUALITY, 500)
    val planar = ConvertBufferedImage.convertFromMulti(image, null, true, classOf[GrayU8])
    val neighbours = for {
      k <- (6 to 7).toStream
      size = 1 << k
      window <- planar.slidingWindow(size, size / 2) filter {
        whitePercentage(_) < 0.3
      }
      neigh = knn(features.toSeq, imageToFeature(window, ""), NumberOfNeighbours)
    } yield (window, neigh map { _.concept })

    gui.reset()
    neighbours foreach { case (w, ns) => gui.addImage(w, simpleString(ns)) }

    val predictions = neighbours map {
      case (_, ns) =>
        val confidence = inverseEntropy(ns, NumberOfNeighbours)
        val prediction = (ns groupBy identity mapValues { _.size }).maxBy(_._2)._1
        (prediction, confidence)
    }

    val safePredictions = predictions groupBy { _._1 } mapValues { _.unzip._2.max } withDefaultValue 0.0

    concepts foreach { c => println(s"$c: ${safePredictions(c)}") }
  }

  for (_ <- 1 to Int.MaxValue) {
    val url = StdIn.readLine()
    Try(analyseImage(url)) match {
      case Failure(ex) => println("failed to analyse image")
      case _ =>
    }
  }
}
