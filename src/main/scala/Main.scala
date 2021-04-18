import Reader._
import dataTypes.FlightRecord

object Main extends App {

  val flightInfo = io.Source.fromResource("flightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData = flightInfo.map(readFlightRecord).toList

  val passengerInfo = io.Source.fromResource("passengers.csv").getLines().map(_.split(",").map(_.trim).toList)
  val passengerHead = List(passengerInfo.next())
  val passengerData = passengerInfo.map(readPassengerRecord).toList

  //  Question 1
  def numFlightsEachMonth() = {
    val distinctFlights = flightData.groupBy(_.flightId).map(_._2.head)
    val flightsEachMonth = distinctFlights.groupBy(x => x.date.getMonth + 1)
    flightsEachMonth.map(x => (x._1, x._2.toList.length))
  }

  //  Question 2
  def mostFrequentFlyers() = {
    val flightsPerPassenger = flightData.groupBy(_.passengerId)
    val lenFlightsPerPassenger = flightsPerPassenger.map(x => (x._1, x._2.length))
    lenFlightsPerPassenger.toList.sortBy(_._2)(Ordering[Int].reverse).take(100)

    //    TODO: find and return names of these people
  }

  //  Question 3
  def countriesPassengerNotUK(passengerId: Int): Int = {
    def longestStretchNotUK(flightRecords: List[FlightRecord]): Int = {
      val orderedFlights = flightRecords.sortBy(_.flightId)

      // fold with accumulator (currentLength, maxLength)
      val currentAndMaxStretch = orderedFlights.foldLeft((0, 0)) { (acc, flightRecord) =>
        val currentLength = acc._1
        val maxLength = acc._2
        val newLength = flightRecord.to match {
          case "uk" => 0
          case _ => currentLength + 1
        }
        val newMax = newLength.max(maxLength)
        (newLength, newMax)
      }

      currentAndMaxStretch._2
    }

    val flightsPerPassenger = flightData.groupBy(_.passengerId)
    longestStretchNotUK(flightsPerPassenger(passengerId))
  }

  //  Question 4
  def flownTogether() = {
    val flightPairs = flightData.groupBy(_.flightId).map(_._2.map(_.passengerId)).flatMap(_.combinations(2).map(_.toSet))
    val flightsTogether = flightPairs.groupMapReduce(identity)(_ => 1)((acc, b) => acc + b)
    flightsTogether.filter(x => x._2 >= 3)
  }

  //  Question 5
  def extendedFlownTogether(N: Int, from: String, to: String) = {
    val filteredFlightData = flightData.filter(x => x.date.after(stringToDate(from)) && x.date.before(stringToDate(to)))

    val flightPairs = filteredFlightData.groupBy(_.flightId).map(_._2.map(_.passengerId)).flatMap(_.combinations(2).map(_.toSet))
    val flightsTogether = flightPairs.groupMapReduce(identity)(_ => 1)((acc, b) => acc + b)
    flightsTogether.filter(x => x._2 >= N)
  }
}
