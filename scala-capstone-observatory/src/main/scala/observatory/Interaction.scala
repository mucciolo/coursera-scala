package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import observatory.Visualization.*

import scala.collection.parallel.CollectionConverters.{IterableIsParallelizable, given}
import scala.math.*

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    val n = 1 << tile.zoom
    Location(
      lat = atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / n))).toDegrees,
      lon = tile.x.toDouble / n * 360.0 - 180.0,
    )

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)],
           colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage = {

    val zoom = 8
    val width = 1 << zoom
    val height = 1 << zoom
    val zoomedOffsetX = tile.x * width
    val zoomedOffsetY = tile.y * height
    val pixels =
      for {
        y <- 0 until height
        x <- 0 until width
        zoomedTile = Tile(x + zoomedOffsetX, y + zoomedOffsetY, tile.zoom + 8)
        location = tileLocation(zoomedTile)
        temperature = predictTemperature(temperatures, location)
        color = interpolateColor(colors, temperature)
      } yield Pixel(x, y, color.red, color.green, color.blue, 127)

    ImmutableImage.create(width, height, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData.par
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
      tile = Tile(x, y, zoom)
    } generateImage(year, tile, data)
  }

