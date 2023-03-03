package observatory

import cats.Eq
import cats.effect.{Blocker, IO}
import fs2.*

import java.nio.file.Paths
import java.time.LocalDate
import scala.io.{BufferedSource, Source => SourceFile}
import scala.util.{Try, Using}
import akka.stream.scaladsl.*

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:

  private val MISSING_TEMP: Double = 9999.9

  case class StationKey(stn: String, wban: String)

  def loadStationsLocation(stationsFile: String): Map[StationKey, Location] =
    Using(SourceFile.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8")) { file =>
      file.getLines()
        .map(_.split(','))
        .collect {
          case Array(stn, wban, lat, lon) if lat.nonEmpty && lon.nonEmpty =>
            StationKey(stn, wban) -> Location(lat.toDouble, lon.toDouble)
        }.toMap
    }.get

  private def fahrenheitToCelsius(f: Double): Double = (f - 32.0) * (5.0 / 9.0)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val stationLocation: Map[StationKey, Location] = loadStationsLocation(stationsFile)

    SourceFile.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8")
      .getLines()
      .map(_.split(','))
      .map {
        case Array(stn, wban, month, day, temp) =>
          val maybeLocation: Option[Location] = stationLocation.get(StationKey(stn, wban))
          val date = LocalDate.of(year, month.toInt, day.toInt)
          (maybeLocation, date, temp.toDouble)
      }
      .collect {
        case (maybeLocation, date, temp) if maybeLocation.isDefined && temp != MISSING_TEMP =>
          (date, maybeLocation.get, fahrenheitToCelsius(temp))
      }.to(Iterable)
  }

  implicit val eqLocation: Eq[Location] = Eq.fromUniversalEquals

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    records.groupMapReduce {
      case (_, location, _) => location
    } {
      case (_, _, temperature) => (temperature, 1)
    } {
      case ((tempA, countA), (tempB, countB)) => (tempA + tempB, countA + countB)
    }.view.mapValues {
      case (tempSum, tempCount) => tempSum / tempCount
    }
