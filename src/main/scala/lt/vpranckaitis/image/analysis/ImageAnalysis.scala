package lt.vpranckaitis.image.analysis

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension}
import javax.swing.{ImageIcon, JFrame, JLabel}

import boofcv.alg.filter.derivative.GradientSobel
import boofcv.alg.misc.GPixelMath
import boofcv.core.image.border.FactoryImageBorderAlgs
import boofcv.gui.image.VisualizeImageData
import boofcv.io.image.{ConvertBufferedImage, UtilImageIO}
import boofcv.struct.image._
import org.imgscalr.Scalr

object ImageAnalysis extends App {

  val image = Scalr.resize(UtilImageIO.loadImage("pictures/orange/3.jpg"), Scalr.Method.QUALITY, 500)
  val out = ConvertBufferedImage.convertFromMulti(image, null, true, classOf[GrayU8])

  val derivX = new GrayS16(out.width, out.height)
  val derivY = new GrayS16(out.width, out.height)

  val gray = new GrayU8(out.width, out.height)

  GPixelMath.averageBand(out, gray)

  GradientSobel.process(gray, derivX, derivY, FactoryImageBorderAlgs.extend(gray));

  val outputImage = VisualizeImageData.colorizeGradient(derivX, derivY, -1);

  val f = new JFrame("Image")

  f.addWindowListener(new WindowAdapter() {
    override def windowClosing(e: WindowEvent): Unit = System.exit(0)
  })

  val label = new JLabel(new ImageIcon(outputImage))
  f.add(label, BorderLayout.CENTER)
  f.getContentPane.setPreferredSize(new Dimension(image.getWidth, image.getHeight))

  f.pack()
  f.setVisible(true)
  label.setVisible(true)
}
