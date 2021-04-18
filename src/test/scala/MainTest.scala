import Reader.{readFlightRecord, readPassengerRecord}
import dataTypes.FlightRecord
import org.scalatest.FunSuite

class MainTest extends FunSuite {
  val flightInfo: Iterator[List[String]] = io.Source.fromResource("testFlightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData: List[FlightRecord] = flightInfo.map(readFlightRecord).toList

  test("Main.numFlightsEachMonths") {
    assert(1 === 0)
  }

  test("Main.mostFrequentFlyers") {
    assert(1 === 0)
  }

  test("Main.countriesPassengerNotUK") {
    assert(1 === 0)
  }

  test("Main.flownTogether") {
    assert(1 === 0)
  }

  test("Main.extendedFlownTogether") {
    assert(1 === 0)
  }
}
