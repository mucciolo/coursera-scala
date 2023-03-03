package observatory

trait VisualizationTest extends MilestoneSuite:

  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  test("predicted temperature at location z should be closer to known temperature at location x" +
    " than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {

    val actual = Visualization.predictTemperature(
      temperatures = Iterable(
        (Location(0, 0), 0.0),
        (Location(1, 1), 1.0),
        (Location(2, 2), 2.0),
      ),
      Location(1.001, 1.001)
    )
    val expected = 1.0

    assertEqualsDouble(actual, expected, 0.05)
  }

  test("predicted temperature should return existing point") {

    val actual = Visualization.predictTemperature(
      temperatures = Iterable(
        (Location(0, 0), 0.0),
        (Location(1, 1), 1.0),
        (Location(2, 2), 2.0),
      ),
      Location(2, 2)
    )
    val expected = 2.0

    assertEquals(actual, expected)
  }

  test("predicted temperature should return weighted average of points") {

    val actual = Visualization.predictTemperature(
      temperatures = Iterable(
        (Location(0, 0), 0.0),
        (Location(1, 1), 1.0),
        (Location(2, 2), 2.0),
      ),
      Location(3.0, 3.0)
    )
    val expected = 1.65

    assertEqualsDouble(actual, expected, 0.05)
  }

  test("exceeding the greatest value of a color scale should return the color associated with the greatest value") {

    val actual = Visualization.interpolateColor(
      points = List((0.0, Color(255, 0, 0)), (2.0, Color(0, 0, 255))),
      value = 3.0
    )
    val expected = Color(0, 0, 255)

    assertEquals(actual, expected)

  }

  test("temperature smaller than the lowest value of a color scale should return the color associated with the lowest value") {

    val actual = Visualization.interpolateColor(
      points = List((0.0, Color(255, 0, 0)), (2.0, Color(0, 0, 255))),
      value = -1.0
    )
    val expected = Color(255, 0, 0)

    assertEquals(actual, expected)

  }

  test("temperature between two values of the color scale should return the linear color interpolation") {

    val actual = Visualization.interpolateColor(
      points = List((0.0, Color(255, 0, 0)), (2.0, Color(0, 0, 255))),
      value = 1.0
    )
    val expected = Color(128, 0, 128)

    assertEquals(actual, expected)

  }
