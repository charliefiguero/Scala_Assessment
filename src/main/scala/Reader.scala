import dataTypes.{FlightRecord, PassengerRecord}

package object Reader {

  private val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
  //  TODO: Check the from and to are date strings
  def stringToDate(dateString: String): java.util.Date = dateFormat.parse(dateString)

  def readFlightRecord(row: List[String]): FlightRecord = row match {
    case List(passengerId, flightId, from, to, date) =>
      FlightRecord(passengerId.toInt, flightId.toInt, from, to, stringToDate(date))
    case _ => throw new Exception("invalid input for dataTypes.FlightRecord")
  }

  def readPassengerRecord(row: List[String]): PassengerRecord = row match {
    case List(passengerId, firstName, lastName) =>
      PassengerRecord(passengerId.toInt, firstName, lastName)
    case _ => throw new Exception("invalid input for dataTypes.PassengerRecord")
  }

}
