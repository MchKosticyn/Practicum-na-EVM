/**
  * Created by Asus on 16.12.2016.
  */
package Blur

import java.awt.{Color, Dimension, Graphics2D}
import java.io.FileInputStream
import Blur._
import scala.swing.{Image => _, _}
import java.awt.image._
import javax.imageio._
import org.scalameter._
import java.io._

object Blur {
  type RGBA = Int
  type Image = Array[Array[RGBA]]
  var Iwidth = 0
  var Iheight = 0

  def RGBA2ARGB(pix: RGBA): Int = {
    (alpha(pix) << 24) | ((pix & 0xffffff00) >>> 8)
  }

  def load(s: String): Image = {
    def ARGB2RGBA(pix: Int): RGBA = {
      red(pix) | ((pix & 0x00ffffff) << 8)
    }

    val stream = new FileInputStream(s)
    val l = ImageIO.read(stream)
    Iheight = l.getHeight
    Iwidth = l.getWidth
    val img: Image = new Image(Iwidth)
    for (i <- 0 until Iwidth)
      img(i) = new Array(Iheight)

    for {
      x <- 0 until Iwidth
      y <- 0 until Iheight
    } img(x)(y) = ARGB2RGBA(l.getRGB(x, y))
    img
  }

  def BlurInf(): (Int, Int, String) = {
    println("Choose number of threads")
    val numOfThreads = scala.io.StdIn.readInt()
    println("Choose radius of Blur")
    val rad = scala.io.StdIn.readInt()
    println("Choose h or v (horizontal or vertical)")
    val blurType = scala.io.StdIn.readLine()
    (numOfThreads, rad, blurType)
  }

  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }


  def Sr(seq: RGBA*): RGBA =
    rgba(seq.map(red).sum / seq.length,
      seq.map(green).sum / seq.length,
      seq.map(blue).sum / seq.length,
      seq.map(alpha).sum / seq.length)


  def exist(x: Int, y: Int): Boolean = (x >= 0) && (x < Iwidth) && (y >= 0) && (y < Iheight)

  def neighbours(I: Image, x: Int, y: Int, rad: Int) = {
    for {
      i <- y - rad to y + rad
      j <- x - rad to x + rad
      if exist(j, i)
    } yield I(j)(i)
  }

  def Sm(L: Image, x: Int, y: Int, rad: Int): RGBA = Sr(neighbours(L, x, y, rad): _*)

  def Sl(L: Image, L1: Image, from: Int, count: Int, rad: Int): Unit = {
    for {x <- 0 until Iwidth
         y <- from until from + count
    } L1(x)(y) = Sm(L, x, y, rad)
  }

  def Slv(L: Image, L1: Image, from: Int, count: Int, rad: Int): Unit = {
    for {y <- 0 until Iheight
         x <- from until from + count
    } L1(x)(y) = Sm(L, x, y, rad)
  }

  def parallel(a: => Unit, b: => Unit) = {
    val left = new Thread {
      override def run(): Unit = {
        b
      }
    }
    left.start()
    a
    left.join()
  }

  @volatile var thread: Int = 1

  def filtH(L: Image, L1: Image, from: Int, count: Int, rad: Int, max: Int): Unit = {
    if ((thread >= max) || (count == 1)) Sl(L, L1, from, count, rad)
    else if ((count % 2) == 1) {
      thread += 1
      parallel(filtH(L, L1, from, count / 2 + 1, rad, max),
        filtH(L, L1, from + 1 + (count / 2), count / 2, rad, max))
      thread -= 1
    }
    else {
      thread += 1
      parallel(filtH(L, L1, from, count / 2, rad, max),
        filtH(L, L1, from + (count / 2), count / 2, rad, max))
      thread -= 1
    }
  }

  def HorizontalFilter(L: Image, L1: Image, max: Int, rad: Int) = filtH(L, L1, 0, Iheight, rad, max)

  def filtV(L: Image, L1: Image, from: Int, count: Int, rad: Int, max: Int): Unit = {
    if ((thread >= max) || (count == 1)) Slv(L, L1, from, count, rad)
    else if ((count % 2) == 1) {
      thread += 1
      parallel(filtV(L, L1, from, count / 2 + 1, rad, max),
        filtV(L, L1, from + 1 + (count / 2), count / 2, rad, max))
      thread -= 1
    }
    else {
      thread += 1
      parallel(filtV(L, L1, from, count / 2, rad, max),
        filtV(L, L1, from + (count / 2), count / 2, rad, max))
      thread -= 1
    }
  }

  def VerticalFilter(L: Image, L1: Image, max: Int, rad: Int) = filtV(L, L1, 0, Iwidth, rad, max)

}

class DataPanel(width: Int, height: Int, image: Array[Array[Int]],
                BGColor: Color) extends Panel {
  override def paintComponent(gcan: Graphics2D) = {
    val bufferedImage = new BufferedImage(Iwidth, Iheight, BufferedImage.TYPE_INT_ARGB)
    for (x <- 0 until width; y <- 0 until height) bufferedImage.setRGB(x, y, image(x)(y))

    gcan.drawImage(bufferedImage, 0, 0, null)
  }
}

object renderer extends SimpleSwingApplication {

  def DeepCopy(I: Image, L: Image): Unit = {
    for {
      x <- 0 until Iwidth
      y <- 0 until Iheight
    } I(x)(y) = L(x)(y)
  }

  var loadImage = load("src/main/scala/pic1.bmp")

  val ExtraImage: Image = {
    var L1: Image = new Image(Iwidth)
    for (i <- 0 until Iwidth)
      L1(i) = new Array(Iheight)
    DeepCopy(L1, loadImage)
    L1
  }

  def top = new MainFrame {

    val colorImageB = ExtraImage.map(_.map(RGBA2ARGB))

    val Inf = BlurInf()
    Inf match {
      case (numOfThreads, rad, "h") =>
        HorizontalFilter(loadImage, ExtraImage, numOfThreads, rad)

      case (numOfThreads, rad, "v") =>
        VerticalFilter(loadImage, ExtraImage, numOfThreads, rad)
    }
    DeepCopy(loadImage, ExtraImage)

    val colorImageA = loadImage.map(_.map(RGBA2ARGB))

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new DataPanel(Iwidth, Iheight, colorImageB, Color.WHITE) {
        preferredSize = new Dimension(Iwidth, Iheight)
      }
      contents += new DataPanel(Iwidth, Iheight, colorImageA, Color.WHITE) {
        preferredSize = new Dimension(Iwidth, Iheight)
      }
    }
    val l = Array(1, 2, 4, 8, 16, 32, 64)
    for (i <- l) {
      println("scalameter > Horizontal filter on " + i + " threads with radius 3 takes " + BluraMeter.HorizontalFilterTime(i, 3, renderer.loadImage, renderer.ExtraImage))
      println("scalameter > Vertical filter on " + i + " threads with radius 3 takes " + BluraMeter.VerticalFilterTime(i, 3, renderer.loadImage, renderer.ExtraImage))
    }
  }
}

object BluraMeter extends Bench.LocalTime {

  val myConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15
  ) withWarmer new Warmer.Default

  def HorizontalFilterTime(max: Int, rad: Int, dst: Image, dst1: Image): Quantity[Double] = {
    val time = myConfig measure {
      HorizontalFilter(dst, dst1, max, rad)
    }
    time
  }

  def VerticalFilterTime(max: Int, rad: Int, dst: Image, dst1: Image): Quantity[Double] = {
    val time = myConfig measure {
      VerticalFilter(dst, dst1, max, rad)
    }
    time
  }
}
