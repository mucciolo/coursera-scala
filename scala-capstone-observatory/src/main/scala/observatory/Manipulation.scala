package observatory

import observatory.Visualization.predictTemperature

import scala.collection.parallel.CollectionConverters.{IterableIsParallelizable, given}
import scala.collection.parallel.ParIterable

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  private val GridLocations: Seq[GridLocation] = for {
    lat <- -89 to 90
    long <- -180 to 179
  } yield GridLocation(lat, long)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    tempByGridLoc(temperatures).apply
  }


  private def tempByGridLoc(
    temperatures: Iterable[(Location, Temperature)]
  ): Map[GridLocation, Temperature] = {

    val gridLocToTemp = for {
      gridLoc <- GridLocations
      location = Location(gridLoc.lat, gridLoc.lon)
      temperature = predictTemperature(temperatures, location)
    } yield gridLoc -> temperature

    gridLocToTemp.toMap
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                     is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {

    val grids = for (temperatures <- temperaturess.par) yield makeGrid(temperatures)
    val gridLocToAvgTemp = for {
      gridLoc <- GridLocations
      temperatures = grids.map(grid => grid(gridLoc))
      avgTemp = temperatures.sum / temperatures.size
    } yield gridLoc -> avgTemp
    val avgTempByGridLoc = gridLocToAvgTemp.toMap

    avgTempByGridLoc.apply
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(
    temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature
  ): GridLocation => Temperature = {

    val tempByGridLoc = Manipulation.tempByGridLoc(temperatures)
    val deviations = tempByGridLoc.map {
      case (gridLoc, temp) => (gridLoc, temp - normals(gridLoc))
    }

    deviations.apply
  }
