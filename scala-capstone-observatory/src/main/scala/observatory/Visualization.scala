package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given

import scala.annotation.tailrec
import scala.collection.View
import scala.collection.parallel.CollectionConverters.given
import scala.math.*

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  private val EarthRadius: Double = 6371.009 // km
  private val WeightPowerParam = 2.0
  private val MinDist = 1.0
  private val ImageWidth = 360
  private val ImageHeight = 180

  def areAntipodalPoints(p: Location, q: Location): Boolean =
    p.lat + q.lat == 0 && abs(p.lon - q.lon) == 180

  def centralAngle(p: Location, q: Location): Double =
    if p == q then 0
    else if areAntipodalPoints(p, q) then Pi
    else acos(
      sin(p.lat.toRadians) * sin(q.lat.toRadians) +
        cos(p.lat.toRadians) * cos(q.lat.toRadians) * cos(abs(p.lon - q.lon).toRadians)
    )

  def earthSurfaceDist(p: Location, q: Location): Double =
    EarthRadius * centralAngle(p, q)

  private def weight(p: Location, q: Location): Double =
    1.0 / pow(earthSurfaceDist(p, q), WeightPowerParam)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    temperatures.view.collect {
      case pair @ (loc, _) if earthSurfaceDist(loc, location) < MinDist =>
        (pair, earthSurfaceDist(loc, location))
    }.minByOption { case (_, dist) => dist }
      .map { case ((_, temp), _) => temp }
      .getOrElse {

        val (numeratorSum, denominatorSum) = temperatures.view.map {
          case (loc_i, temp_i) =>
            val weight_i = weight(location, loc_i)
            (weight_i * temp_i, weight_i)
        }.fold((0.0, 0.0)) {
          case ((num1, denom1), (num2, denom2)) => (num1 + num2, denom1 + denom2)
        }

        numeratorSum / denominatorSum
      }
  }

  private def linearInterp(y1: Int, y2: Int, x: Double): Int = round(y1 + x * (y2 - y1)).toInt

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    @tailrec
    def findBounds(points: Iterator[(Temperature, Color)],
                   lower: Option[(Temperature, Color)] = None,
                   upper: Option[(Temperature, Color)] = None
                  ): (Option[(Temperature, Color)], Option[(Temperature, Color)]) = {

      points.nextOption() match
        case pt @ Some((tempPt, _)) =>
          if (tempPt == value) {
            (pt, pt)
          } else if (tempPt < value) {
            lower match
              case Some((tempLow, _)) =>
                findBounds(points, if (tempPt > tempLow) pt else lower, upper)
              case None =>
                findBounds(points, pt, upper)
          } else {
            upper match
              case Some((tempUp, _)) =>
                findBounds(points, lower, if (tempPt < tempUp) pt else upper)
              case None =>
                findBounds(points, lower, pt)
          }
        case None =>
          (lower, upper)
    }

    findBounds(points.iterator) match {
      case (Some((t1, c1 @ Color(r1, g1, b1))), Some((t2, Color(r2, g2, b2)))) =>
        if (t1 == t2) {
          c1
        } else {
          val factor = (value - t1) / (t2 - t1)
          Color(
            red = linearInterp(r1, r2, factor),
            green = linearInterp(g1, g2, factor),
            blue = linearInterp(b1, b2, factor)
          )
        }

      case (None, Some((_, upperColor))) =>
        upperColor

      case (Some((_, lowerColor)), None) =>
        lowerColor

      case _ =>
        throw new IllegalArgumentException("empty points")
    }
  }

  case class Coords(x: Int, y: Int) {
    def toLocation: Location = Location(lat = ImageHeight / 2 - y, lon = x - ImageWidth / 2)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): ImmutableImage = {

    def getColor(coords: Coords): Color = {
      val temp = predictTemperature(temperatures, coords.toLocation)
      interpolateColor(colors, temp)
    }

    val coordsSeq = for y <- 0 until ImageHeight; x <- 0 until ImageWidth yield Coords(x, y)

    val pixelsArray = coordsSeq.view
      .map {
        case coords @ Coords(x, y) =>
          val Color(r, g, b) = getColor(coords)
          Pixel(x, y, r, g, b, 255)
      }.toArray

    ImmutableImage.create(ImageWidth, ImageHeight, pixelsArray)
  }
