package observatory

import scala.collection.concurrent.TrieMap
import java.awt.Point
import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.Colors
import observatory.Interaction.tileLocation

import scala.math.BigDecimal.RoundingMode

trait InteractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _
  private val MaxLat = 85.0511287798066
  private val MaxLon = 180.0

  test("tileLocation zoom 0") {
    val actual = tileLocation(Tile(0, 0, 0))
    val expected = Location(MaxLat, -MaxLon)
    assertEquals(actual, expected)
  }

  test("tileLocation zoom 1") {
    val zoom = 1
    val tiles = for y <- 0 to 1; x <- 0 to 1 yield Tile(x, y, zoom)
    val actualTiles = tiles.map(tileLocation)
    val expectedTiles = Seq(
      Location(MaxLat, -MaxLon),
      Location(MaxLat, 0.0),
      Location(0.0, -MaxLon),
      Location(0.0, -0.0)
    )
    assert(actualTiles == expectedTiles)
  }

