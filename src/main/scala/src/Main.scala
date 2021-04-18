package src

import FlightRecord._
import PassengerRecord._
import Reader._

object Main extends App {

  val flightInfo = io.Source.fromResource("flightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData = flightInfo.map(readFlightRecord).toList

  val passengerInfo = io.Source.fromResource("passengers.csv").getLines().map(_.split(",").map(_.trim).toList)
  val passengerHead = List(passengerInfo.next())
  val passengerData = passengerInfo.map(readPassengerRecord).toList

//  Question 1
  val distinctFlights = flightData.groupBy(_.flightId).map(_._2.head)
  val flightsEachMonth = distinctFlights.groupBy(x => x.date.getMonth + 1)
  val numFlightsEachMonth = flightsEachMonth.map(x => (x._1, x._2.toList.length))

  numFlightsEachMonth.foreach(println)

}


///*
//Submission for Quantexa Assignment.
//*/
//
//import scala.collection.immutable.HashMap
//
//
//// Date setup and parsing
//val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
//def stringToDate(dateString: String): java.util.Date = dateFormat.parse(dateString)
//
//  // Define data types for the two records
//  case class FlightRecord(val passengerId: Int, val flightId: Int, val from: String, val to: String, val date: java.util.Date)
//  // eg. 48,0,cg,ir,2017-01-01
//  def readFlightRecord(row: List[String]): FlightRecord = row match {
//  case List(passengerId, flightId, from, to, date) =>
//  FlightRecord(passengerId.toInt, flightId.toInt, from, to, stringToDate(date))
//  case _ => throw new Exception("invalid input for FlightRecord")
//  }
//  case class PassengerRecord(passengerId: Int, firstName: String, lastName: String)
//  // eg. 14751,Napoleon,Gaylene
//  def readPassengerRecord(row: List[String]): PassengerRecord = row match {
//  case List(passengerId, firstName, lastName) =>
//  PassengerRecord(passengerId.toInt, firstName, lastName)
//  case _ => throw new Exception("invalid input for PassengerRecord")
//  }
//
//
//  // read in flightData
//  val flightInfo = io.Source.fromFile("flightData.csv").getLines().map(_.split(",").map(_.trim).toList)
//  val flightHead = List(flightInfo.next())
//  val flightData = flightInfo.map(readFlightRecord).toList
//
//  // read in passengerData
//  val passengerInfo = io.Source.fromFile("passengers.csv").getLines().map(_.split(",").map(_.trim).toList)
//  val passengerHead = List(passengerInfo.next())
//  val passengerData = passengerInfo.map(readPassengerRecord).toList
//
//
//
//  // ######################################################################
//  // #####  Question 1: Find the total number of flights each month.  #####
//  // ######################################################################
//
//  // TODO: update this to use non-depreciated functions
//  // val distinctFlights = flightData.groupBy(_.flightId).map(_._2.head)
//  // val flightsEachMonth = distinctFlights.groupBy(x => (x.date.getMonth + 1))
//  // val numFlightsEachMonth = flightsEachMonth.map(x => (x._1, x._2.toList.length))
//
//  // numFlightsEachMonth.foreach(println)
//
//
//
//  // #########################################################################
//  // #####  Question 2: Find the names of the 100 most frequent flyers.  #####
//  // #########################################################################
//
//  // val flightsPerPassenger = flightData.groupBy(_.passengerId)
//  // val lenFlightsPerPassenger = flightsPerPassenger.map(x => (x._1, x._2.toList.length))
//  // val topFlyers = lenFlightsPerPassenger.toList.sortBy(_._2)(Ordering[Int].reverse).take(100)
//
//  // topFlyers.foreach(println)
//
//
//
//  // ##################################################################################################
//  // Question 3: Find the greatest number of countries a passenger has been in without being in the UK.
//  // For example, if the countries a passenger was in were: UK -> FR -> US -> CN -> UK -> DE -> UK,
//  // the correct answer would be 3 countries.
//  // ##################################################################################################
//
//  def stretchNotUK(flightRecords: List[FlightRecord]): Int = {
//  // TODO: make sure flightRecords are distinct
//  val orderedFlights = flightRecords.sortBy(_.flightId)
//
//  // fold with accumulator (currentLength, maxLength)
//  val currentAndMaxStretch = orderedFlights.foldLeft( (0, 0) ) { (acc, flightRecord) =>
//  val currentLength = acc._1
//  val maxLength = acc._2
//  val newLength = flightRecord.to match {
//  case "uk" => 0
//  case _    => currentLength + 1
//  }
//  val newMax = newLength.max(maxLength)
//  (newLength, newMax)
//  }
//  currentAndMaxStretch._2
//  }
//
//  def countriesPassengerNotUK(passengerId: Int): Int = {
//  val flightsPerPassenger = flightData.groupBy(_.passengerId)
//  stretchNotUK(flightsPerPassenger(passengerId))
//  }
//
//  // val index = 1000
//  // val testList = flightsPerPassenger(index)
//  // testList.map(_.to).foreach(println)
//  // println(stretchNotUK(testList))
//  // println(countriesPassengerNotUK(index))
//
//
//
//  // ########################################################################################################################
//  // #####  Question 4: Find the passengers who have been on more than 3 flights together.                              #####
//  // ########################################################################################################################
//  // Complextity to find pairs: n! / 2 * (n-2)!, or roughly == n**2 / 2 == O(n^2)
//  // TODO: only compute combinations for passengers with >= N flights
//
//  val flightPairs = flightData.groupBy(_.flightId).map(_._2.map(_.passengerId)).map(_.combinations(2).map(_.toSet)).flatten
//  val flightsTogether = flightPairs.groupMapReduce(identity)(_ => 1)((acc, b) => acc + b)
//  flightsTogether.filter(x => x._2 > 2).foreach(println)
//
//
//
//// ########################################################################################################################
//// #####  Question 5: Find the passengers who have been on more than N flights together within the range (from, to).  #####
//// ########################################################################################################################
//
//// val N = 0
//// val from: java.util.Date = stringToDate("2017-01-01")
//// val to:   java.util.Date = stringToDate("2017-01-20")
//// val filteredFlightData = flightData.filter(x => x.date.after(from) && x.date.before(to))
//
//// val flightPairs = filteredFlightData.groupBy(_.flightId).map(_._2.map(_.passengerId)).map(_.combinations(2).map(_.toSet)).flatten
//// val flightsTogether = flightPairs.groupMapReduce(identity)(_ => 1)((acc, b) => acc + b)
//// flightsTogether.filter(x => x._2 >= N).foreach(println)
//
//
//
//
//// ###################
//// #####  TESTS  #####
//// ###################