import Reader.{readFlightRecord, readPassengerRecord}
import dataTypes.FlightRecord
import org.scalatest.FunSuite

class ServicesTest extends FunSuite {
  val flightInfo: Iterator[List[String]] = io.Source.fromResource("testFlightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData: List[FlightRecord] = flightInfo.map(readFlightRecord).toList

  test("Services.namesFromId") {
    assert(1 === 0)
  }

  test("Services.numFlightsEachMonths") {
    assert(1 === 0)
  }

  test("Services.mostFrequentFlyers") {
    assert(1 === 0)
  }

  test("Services.countriesPassengerNotUK") {
    assert(1 === 0)
  }

  test("Services.flownTogether") {
    assert(1 === 0)
  }

  test("Services.extendedFlownTogether") {
    assert(1 === 0)
  }
}
