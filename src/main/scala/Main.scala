import Reader._
import Services._

object Main extends App {

  val flightInfo = io.Source.fromResource("flightData.csv").getLines().map(_.split(",").map(_.trim).toList)
  val flightHead = List(flightInfo.next())
  val flightData = flightInfo.map(readFlightRecord).toList

  val passengerInfo = io.Source.fromResource("passengers.csv").getLines().map(_.split(",").map(_.trim).toList)
  val passengerHead = List(passengerInfo.next())
  val passengerData = passengerInfo.map(readPassengerRecord).toList

  // ######################################################################
  // #####  Question 1: Find the total number of flights each month.  #####
  // ######################################################################

  println("Question 1: Total number of flights each month:")
  val numFlightsEachMonthMap = numFlightsEachMonth(flightData)
  for (i <- 1 to 12){
    println(i + ": " + numFlightsEachMonthMap(i))
  }
  println()

  // #########################################################################
  // #####  Question 2: Find the names of the 100 most frequent flyers.  #####
  // #########################################################################

  println("Question 2: Find the names of the 100 most frequent flyers.")
  val topFlyers = mostFrequentFlyers(flightData)
  for (i <- 0 to 99){
    val record = topFlyers(i)
    val name = namesFromId(record._1, passengerData)
//    println(name)
    println(record._1, record._2, name)
  }
  println()

  // ##################################################################################################
  // Question 3: Find the greatest number of countries a passenger has been in without being in the UK.
  // For example, if the countries a passenger was in were: UK -> FR -> US -> CN -> UK -> DE -> UK,
  // the correct answer would be 3 countries.
  // ##################################################################################################

  println("Question 3: Find the greatest number of countries a passenger has been in without being in the UK.")
  val aPassengerId = 3141
  println("PassengerId: " + aPassengerId + ", longestStretchNotUK: " + countriesPassengerNotUK(aPassengerId, flightData))
  println()

  // ############################################################################################
  // #####  Question 4: Find the passengers who have been on more than 3 flights together.  #####
  // ############################################################################################

  println("Question 4: Find the passengers who have been on more than 3 flights together.")
  val flownTogetherMap = flownTogether(flightData)
  flownTogetherMap.foreach(println)
  println()

  // ########################################################################################################################
  // #####  Question 5: Find the passengers who have been on more than N flights together within the range (from, to).  #####
  // ########################################################################################################################

  println("Question 5: Find the passengers who have been on more than N flights together within the range (from, to).")
  val N = 15
  val from = "2017-01-01"
  val to = "2017-06-08"
  val extendedFlownTogetherMap = extendedFlownTogether(N, from, to, flightData)
  extendedFlownTogetherMap.foreach(println)
  println()
}
