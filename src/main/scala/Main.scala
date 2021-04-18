import Reader._
import dataTypes.FlightRecord

object Main extends App {

  val flightInfo = io.Source.fromResource("flightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData = flightInfo.map(readFlightRecord).toList

  val passengerInfo = io.Source.fromResource("passengers.csv").getLines().map(_.split(",").map(_.trim).toList)
  val passengerHead = List(passengerInfo.next())
  val passengerData = passengerInfo.map(readPassengerRecord).toList

  // Question 1
  // Question 2
  // Question 3
  // Question 4
  // Question 5
}
