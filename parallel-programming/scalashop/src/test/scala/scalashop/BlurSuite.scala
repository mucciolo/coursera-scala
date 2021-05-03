package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test
  def test() = {

    val radius = 1
    val width = 32
    val height = 64
    val src = new Img(width, height)
    val dst = new Img(width, height)

    VerticalBoxBlur.parBlur(src, dst, 32, radius)
  }
}
