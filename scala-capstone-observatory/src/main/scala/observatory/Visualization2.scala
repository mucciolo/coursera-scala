package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import observatory.Interaction.tileLocation
import observatory.Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {

    val x = point.x
    val y = point.y

    d00 * (1 - x) * (1 - y) +
      d01 * (1 - x) * y +
      d10 * x * (1 - y) +
      d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage = {

    val zoom = 8
    val width = 1 << zoom
    val height = 1 << zoom
    val zoomedOffsetX = tile.x * width
    val zoomedOffsetY = tile.y * height

    val pixels =
      for {
        y <- 0 until height
        x <- 0 until width
        zoomedTile = Tile(x + zoomedOffsetX, y + zoomedOffsetY, tile.zoom + zoom)
        location = tileLocation(zoomedTile)
        xG = location.lon
        yG = location.lat
        cell = CellPoint(xG - xG.floor, yG - yG.floor)
        d00 = grid(GridLocation(yG.floor.toInt, xG.floor.toInt))
        d01 = grid(GridLocation(yG.ceil.toInt, xG.floor.toInt))
        d10 = grid(GridLocation(yG.floor.toInt, xG.ceil.toInt))
        d11 = grid(GridLocation(yG.ceil.toInt, xG.ceil.toInt))
        temperature = bilinearInterpolation(cell, d00, d01, d10, d11)
        color = interpolateColor(colors, temperature)
      } yield Pixel(x, y, color.red, color.green, color.blue, 127)

    ImmutableImage.create(width, height, pixels.toArray)
  }

