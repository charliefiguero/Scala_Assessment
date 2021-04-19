import Reader.{readFlightRecord, readPassengerRecord, stringToDate}
import Services._
import dataTypes.{FlightRecord, PassengerRecord}
import org.scalatest.FunSuite

class ServicesTest extends FunSuite {

  test("Services.namesFromId") {

    val io = List( PassengerRecord(10760, "Jayna", "Lyle"),
                   PassengerRecord(3187, "Natashia","Quintin"),
                   PassengerRecord(13950, "Caitlyn","Olive"),
                   PassengerRecord(2031, "Emilia","Delphia"),
    )
    assert(namesFromId(10760, io) === ("Jayna", "Lyle"))
    assert(namesFromId(3187, io) === ("Natashia","Quintin"))
    assert(namesFromId(13950, io) === ("Caitlyn","Olive"))
    assert(namesFromId(2031, io) === ("Emilia","Delphia"))
  }

  test("Services.numFlightsEachMonths") {

    // no flights
    val i0: List[FlightRecord] = List.empty
    val o0: Map[Int, Int] = Services.numFlightsEachMonth(i0)
    assert(o0 == Map.empty)

    // one flight in jan
    val i1: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2021-01-01")))
    val o1: Map[Int, Int] = Services.numFlightsEachMonth(i1)
    assert(o1 == Map(1 -> 1))

    // one flight in may
    val i2: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2021-05-23")))
    val o2: Map[Int, Int] = Services.numFlightsEachMonth(i2)
    assert(o2 == Map(5 -> 1))

    // flights in different months, same year
    val i3: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2021-01-01")),
                                      FlightRecord(0, 1, "", "", stringToDate("2021-03-23")),
                                      FlightRecord(0, 2, "", "", stringToDate("2021-12-04")),
                                      FlightRecord(0, 3, "", "", stringToDate("2021-07-22")),
                                      FlightRecord(0, 4, "", "", stringToDate("2021-05-15")))
    val o3: Map[Int, Int] = Services.numFlightsEachMonth(i3)
    assert(o3 == Map(1 -> 1, 3 -> 1, 5 -> 1, 7 -> 1, 12 -> 1))

    // flights in different months, different years
    val i4: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2021-01-01")),
                                      FlightRecord(0, 1, "", "", stringToDate("2017-05-23")),
                                      FlightRecord(0, 2, "", "", stringToDate("2015-12-04")),
                                      FlightRecord(0, 3, "", "", stringToDate("2020-07-22")),
                                      FlightRecord(0, 5, "", "", stringToDate("2013-05-15")),
                                      FlightRecord(0, 6, "", "", stringToDate("2012-06-19")),
                                      FlightRecord(0, 7, "", "", stringToDate("2011-12-22")),
                                      FlightRecord(0, 8, "", "", stringToDate("2021-05-06")),
                                      FlightRecord(0, 9, "", "", stringToDate("2001-03-11")))
    val o4: Map[Int, Int] = Services.numFlightsEachMonth(i4)
    assert(o4 == Map(1 -> 1, 3 -> 1, 5 -> 3, 6 -> 1, 7 -> 1, 12 -> 2))

    // flights with same id should be grouped (distinct)
    val i5: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2001-02-02")),
                                      FlightRecord(0, 0, "", "", stringToDate("2001-02-02")),
                                      FlightRecord(0, 1, "", "", stringToDate("2013-04-10")),
                                      FlightRecord(0, 1, "", "", stringToDate("2013-04-10")),
                                      FlightRecord(0, 1, "", "", stringToDate("2013-04-10")),
                                      FlightRecord(0, 2, "", "", stringToDate("2005-08-18")),
                                      FlightRecord(0, 2, "", "", stringToDate("2005-08-18")),
                                      FlightRecord(0, 2, "", "", stringToDate("2005-08-18")))
    val o5: Map[Int, Int] = Services.numFlightsEachMonth(i5)
    assert(o5 == Map(2 -> 1, 4 -> 1, 8 -> 1))

    // mix-and-match
    val i6: List[FlightRecord] = List(FlightRecord(0, 0, "", "", stringToDate("2012-05-02")),
                                      FlightRecord(0, 1, "", "", stringToDate("2002-08-18")),
                                      FlightRecord(0, 0, "", "", stringToDate("2012-05-02")),
                                      FlightRecord(0, 3, "", "", stringToDate("2014-01-24")),
                                      FlightRecord(0, 5, "", "", stringToDate("2021-09-02")),
                                      FlightRecord(0, 1, "", "", stringToDate("2002-08-18")),
                                      FlightRecord(0, 2, "", "", stringToDate("2029-05-06")),
                                      FlightRecord(0, 2, "", "", stringToDate("2029-05-06")),
                                      FlightRecord(0, 4, "", "", stringToDate("2012-02-15")))
    val o6: Map[Int, Int] = Services.numFlightsEachMonth(i6)
    assert(o6 == Map(1 -> 1, 2 -> 1, 5 -> 2, 8 -> 1, 9 -> 1))
  }

  test("Services.mostFrequentFlyers") {
    // top flyer comes first
    val i0 = List(FlightRecord(0, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(0, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(0, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(1, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(1, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(2, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(1, 0, null, null, stringToDate("2017-06-08")),
                  FlightRecord(0, 0, null, null, stringToDate("2017-06-08"))
                 )
    val o0 = mostFrequentFlyers(i0).head
    assert( o0 === (0, 4) )

    // last flyer comes last
    val o1 = mostFrequentFlyers(i0).last
    assert( o1 === (2, 1) )

    // 0 flyers returns empty list
    val i2 = List()
    val o2 = mostFrequentFlyers(i2)
    assert( o2 === List() )
  }

  test("Services.countriesPassengerNotUK") {
    // 'Find the greatest number of countries a passenger has been in without being in the UK'

    // no flights
    val i0: (Int, List[FlightRecord]) = (0, List.empty)
    val o0: Int = (Services.countriesPassengerNotUK _).tupled(i0)
    assert(o0 == 0)

    // one passenger, one flight ending in UK
    //     (CR -> UK)
    val i1: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "cr", "uk", null)))
    val o1: Int = (Services.countriesPassengerNotUK _).tupled(i1)
    assert(o1 == 1)

    // one passenger, one flight starting in UK
    // (UK -> CR)
    val i2: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "uk", "cr", null)))
    val o2: Int = (Services.countriesPassengerNotUK _).tupled(i2)
    assert(o2 == 1)

    // one passenger, non-uk route (one flight)
    // (GR -> CR)
    val i3: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "gr", "cr", null)))
    val o3: Int = (Services.countriesPassengerNotUK _).tupled(i3)
    assert(o3 == 2)

    // one passenger, non-uk route (multiple flights)
    // (GR -> CR -> PL -> CN)
    val i4: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "gr", "cr", null),
                                                FlightRecord(0, 1, "gr", "pl", null),
                                                FlightRecord(0, 2, "pl", "cn", null)))
    val o4: Int = (Services.countriesPassengerNotUK _).tupled(i4)
    assert(o4 == 4)

    // one passenger, only-uk route (multiple flights)
    // (UK -> UK -> UK)
    val i5: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "uk", "uk", null),
                                                 FlightRecord(0, 1, "uk", "uk", null),
                                                 FlightRecord(0, 2, "uk", "uk", null)))
    val o5: Int = (Services.countriesPassengerNotUK _).tupled(i5)
    assert(o5 == 0)

    // one passenger, non&uk route (multiple flights) (1)
    // (UK -> CN -> PL -> GR -> UK -> PL -> FR)
    val i6: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "uk", "cn", null),
                                                 FlightRecord(0, 1, "cn", "pl", null),
                                                 FlightRecord(0, 2, "pl", "gr", null),
                                                 FlightRecord(0, 3, "gr", "uk", null),
                                                 FlightRecord(0, 4, "uk", "pl", null),
                                                 FlightRecord(0, 5, "pl", "fr", null)))
    val o6: Int = (Services.countriesPassengerNotUK _).tupled(i6)
    assert(o6 == 3)

    // one passenger, non&uk route (multiple flights) (2)
    // (CN -> PL -> JP -> UK -> GR -> UK -> PL -> FR -> UK)
    val i7: (Int, List[FlightRecord]) = (0, List(FlightRecord(0, 0, "cn", "pl", null),
                                                  FlightRecord(0, 1, "pl", "jp", null),
                                                  FlightRecord(0, 2, "jp", "uk", null),
                                                  FlightRecord(0, 3, "uk", "gr", null),
                                                  FlightRecord(0, 4, "gr", "uk", null),
                                                  FlightRecord(0, 5, "uk", "pl", null),
                                                  FlightRecord(0, 6, "pl", "fr", null),
                                                  FlightRecord(0, 7, "fr", "uk", null)))
    val o7: Int = (Services.countriesPassengerNotUK _).tupled(i7)
    assert(o7 == 3)

    // multiple passengers, non-uk routes
    // 0: (FR -> AU -> SA -> JP -> FR -> KR)
    // 1: (PL -> FR -> US -> JP -> FR)
    val i8: List[FlightRecord] = List(FlightRecord(0, 0, "fr", "au", null),
                                       FlightRecord(0, 1, "au", "sa", null),
                                       FlightRecord(0, 2, "sa", "jp", null),
                                       FlightRecord(0, 3, "jp", "fr", null),
                                       FlightRecord(0, 4, "fr", "kr", null),

                                       FlightRecord(1, 5, "pl", "fr", null),
                                       FlightRecord(1, 6, "fr", "us", null),
                                       FlightRecord(1, 2, "us", "jp", null),
                                       FlightRecord(1, 3, "jp", "fr", null))
    val o81: Int = Services.countriesPassengerNotUK(0, i8)
    val o82: Int = Services.countriesPassengerNotUK(1, i8)
    assert(o81 == 6)
    assert(o82 == 5)

    // multiple passengers, uk routes
    // 0: (FR -> UK -> AU -> SA -> JP -> FR -> KR -> UK)
    // 1: (PL -> FR -> US -> UK -> JP -> UK -> FR)
    // 2: (UK -> PL -> FR -> UK -> US -> JP -> FR)
    val i9: List[FlightRecord] = List(FlightRecord(0, 0, "fr", "uk", null),
                                       FlightRecord(0, 1, "uk", "au", null),
                                       FlightRecord(0, 2, "au", "sa", null),
                                       FlightRecord(0, 3, "sa", "jp", null),
                                       FlightRecord(0, 4, "jp", "fr", null),
                                       FlightRecord(0, 5, "fr", "kr", null),
                                       FlightRecord(0, 6, "kr", "uk", null),

                                       FlightRecord(1, 7, "pl", "fr", null),
                                       FlightRecord(1, 8, "fr", "us", null),
                                       FlightRecord(1, 9, "us", "uk", null),
                                       FlightRecord(1, 10, "uk", "jp", null),
                                       FlightRecord(1, 11, "jp", "uk", null),
                                       FlightRecord(1, 12, "uk", "fr", null),

                                       FlightRecord(2, 13, "uk", "pl", null),
                                       FlightRecord(2, 14, "pl", "fr", null),
                                       FlightRecord(2, 15, "fr", "uk", null),
                                       FlightRecord(2, 16, "uk", "us", null),
                                       FlightRecord(2, 17, "us", "jp", null),
                                       FlightRecord(2, 18, "jp", "fr", null))
    val o91: Int = Services.countriesPassengerNotUK(0, i9)
    val o92: Int = Services.countriesPassengerNotUK(1, i9)
    val o93: Int = Services.countriesPassengerNotUK(2, i9)
    assert(o91 == 5)
    assert(o92 == 3)
    assert(o93 == 3)
  }

  test("Services.flownTogether") {
    // Find the passengers who have been on more than 3 flights together.

    // no flight records
    val i0: List[FlightRecord] = List.empty
    val o0: Map[Set[Int], Int] = Services.flownTogether(i0)
    assert(o0 == Map.empty)

    // one flight record
    val i1: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null))
    val o1: Map[Set[Int], Int] = Services.flownTogether(i1)
    assert(o1 == Map.empty)

    // passenger flown with self 4 times
    val i2: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null),
                                      FlightRecord(0, 1, null, null, null),
                                      FlightRecord(0, 2, null, null, null),
                                      FlightRecord(0, 3, null, null, null))
    val o2: Map[Set[Int], Int] = Services.flownTogether(i2)
    assert(o2 == Map.empty)

    // passenger flown with one other 2 times
    val i3: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null),
                                      FlightRecord(0, 1, null, null, null),
                                      FlightRecord(1, 0, null, null, null),
                                      FlightRecord(1, 1, null, null, null))
    val o3: Map[Set[Int], Int] = Services.flownTogether(i3)
    assert(o3 == Map.empty)

    // passenger flown with one other 3 times
    val i4: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null),
                                      FlightRecord(0, 1, null, null, null),
                                      FlightRecord(0, 2, null, null, null),
                                      FlightRecord(1, 0, null, null, null),
                                      FlightRecord(1, 1, null, null, null),
                                      FlightRecord(1, 2, null, null, null))
    val o4: Map[Set[Int], Int] = Services.flownTogether(i4)
    assert(o4 == Map(Set(0, 1) -> 3))

    // passenger flown with one other 5 times
    val i5: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null),
                                      FlightRecord(0, 1, null, null, null),
                                      FlightRecord(1, 1, null, null, null),
                                      FlightRecord(1, 2, null, null, null),
                                      FlightRecord(0, 4, null, null, null),
                                      FlightRecord(0, 3, null, null, null),
                                      FlightRecord(1, 3, null, null, null),
                                      FlightRecord(0, 2, null, null, null),
                                      FlightRecord(1, 0, null, null, null),
                                      FlightRecord(1, 4, null, null, null))
    val o5: Map[Set[Int], Int] = Services.flownTogether(i5)
    assert(o5 == Map(Set(0, 1) -> 5))

    // passenger [0] flown with [1] 3 times, [0] with [2] 2 times, [1] with [2] 5 times, [3] with [0] 1 time
    val i6: List[FlightRecord] = List(FlightRecord(0, 0, null, null, null),
                                      FlightRecord(0, 1, null, null, null),
                                      FlightRecord(0, 2, null, null, null),

                                      FlightRecord(1, 0, null, null, null),
                                      FlightRecord(1, 1, null, null, null),
                                      FlightRecord(1, 2, null, null, null),
                                      FlightRecord(1, 3, null, null, null),
                                      FlightRecord(1, 4, null, null, null),
                                      FlightRecord(1, 5, null, null, null),

                                      FlightRecord(2, 2, null, null, null),
                                      FlightRecord(2, 0, null, null, null),
                                      FlightRecord(2, 3, null, null, null),
                                      FlightRecord(2, 4, null, null, null),
                                      FlightRecord(2, 5, null, null, null),

                                      FlightRecord(3, 1, null, null, null),
    )
    val o6: Map[Set[Int], Int] = Services.flownTogether(i6)
    assert(o6 == Map(Set(0, 1) -> 3, Set(1, 2) -> 5))
  }

  test("Services.extendedFlownTogether") {
    // passenger [0] flown with [1] 3 times, [0] with [2] 2 times, [1] with [2] 5 times, [3] with [0] 1 time
    val i0: List[FlightRecord] = List(FlightRecord(0, 0, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(0, 1, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(0, 2, null, null, stringToDate("2017-06-08")),

                                      FlightRecord(1, 0, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(1, 1, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(1, 2, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(1, 3, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(1, 4, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(1, 5, null, null, stringToDate("2017-06-08")),

                                      FlightRecord(2, 2, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(2, 0, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(2, 3, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(2, 4, null, null, stringToDate("2017-06-08")),
                                      FlightRecord(2, 5, null, null, stringToDate("2017-06-08")),

                                      FlightRecord(3, 1, null, null, stringToDate("2017-06-08")),
    )
    val o1: Map[Set[Int], Int] = Services.extendedFlownTogether(3, "2017-06-07", "2017-06-09", i0)
    assert(o1 == Map(Set(0, 1) -> 3, Set(1, 2) -> 5))
  }
}
