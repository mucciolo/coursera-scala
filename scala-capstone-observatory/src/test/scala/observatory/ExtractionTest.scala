package observatory

import observatory.Extraction.StationKey

import java.time.LocalDate

trait ExtractionTest extends MilestoneSuite:

  private val milestoneTest = namedMilestoneTest("data extraction", 1) _
  private val stationsFile: String = "/stations.csv"
  private val tempsFile: String = "/temps.csv"

  test("stations with no gps coordinates should be ignored") {
    val actual: Map[StationKey, Location] = Extraction.loadStationsLocation(stationsFile)
    val expected = Map(
      StationKey("", "") -> Location(0, 0),
      StationKey("1", "") -> Location(1, 0),
      StationKey("2", "2") -> Location(2, 2)
    )
    assertEquals(actual.toList, expected.toList)
  }

  test("locateTemperatures should ignore record with no matching station or temperature = 9999.9") {
    val actual = Extraction.locateTemperatures(2023, stationsFile, tempsFile)
    val expected = Iterable(
      (LocalDate.of(2023, 1, 1), Location(0, 0), 0.0),
      (LocalDate.of(2023, 1, 1), Location(1, 0), 10.0),
      (LocalDate.of(2023, 1, 1), Location(2, 2), 20.0),
      (LocalDate.of(2023, 1, 2), Location(0, 0), 10.0)
    )
    assertEquals(actual.toList, expected.toList)
  }

  test("locationYearlyAverageRecords should average temperature by location") {
    val temps = Extraction.locateTemperatures(2023, stationsFile, tempsFile)
    val actual = Extraction.locationYearlyAverageRecords(temps)
    val expected = Iterable(
      (Location(0, 0), 5.0),
      (Location(1, 0), 10.0),
      (Location(2, 2), 20.0)
    )
    assertEquals(actual.toList, expected.toList)
  }