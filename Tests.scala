package Blur

/**
  * Created by Asus on 21.12.2016.
  */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Blur._

@RunWith(classOf[JUnitRunner])
class Tests extends FunSuite {
  test("Color fragmentation test") {
    val testColor: RGBA = 0x04010203
    assert(red(testColor) == 0x04, "red test")
    assert(green(testColor) == 0x01, "green test")
    assert(blue(testColor) == 0x02, "blue test")
    assert(alpha(testColor) == 0x03, "alpha test")
  }

  test("Collecting color test") {
    val r = 0xfa
    val g = 0xfb
    val b = 0xfc
    val a = 0xfd
    assert(rgba(r, g, b, a) == 0xfafbfcfd)
  }
  def Equal(I:Image, I1:Image): Boolean =
    (for {
    x <- 0 until 20
    y <- 0 until 20
  } yield I1(x)(y) == I(x)(y)).forall(identity)

  test("Blur test") {
    val I : Image = new Array(20)
    for (i <- 0 until 20)
      I(i) = new Array(20)
    for {
      x <- 0 until 20
      y <- 0 until 20
    } I(x)(y) = 0x00ff00ff

    val I1 : Image = new Array(20)
    for (i <- 0 until 20)
      I1(i) = new Array(20)

    for {
      x <- 0 until 20
      y <- 0 until 20
    } I1(x)(y) = I(x)(y)

    HorizontalFilter(I,I1,8,0)
    assert(Equal(I,I1), "zero blur radius")

    HorizontalFilter(I,I1,8,3)
    assert(Equal(I,I1), "nonzero blur radius")
  }
}
