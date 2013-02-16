package com.plexq.csv

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class CSVParserSpec extends FunSuite with ShouldMatchers {
  test("Parsing a simple CSV with values") {
    val k = new CSVParser().apply("A,B,C,D")

    k should be (List("A","B","C","D"))
  }

  test("Parsing simply doubled quoted CSV values") {
    val k = new CSVParser().apply("\"Apple\",\"Bottle\",\"Cat\",\"Dharma\"")

    k should be (List("Apple", "Bottle", "Cat", "Dharma"))
  }

  test("Parsing a CSV with mixed quoted and unquoted values") {
    // Firstly starting unquoted
    val k = new CSVParser().apply("Apple,\"Bottle\",Cat,\"Dharma\",Exceed")

    k should be (List("Apple", "Bottle", "Cat", "Dharma", "Exceed"))

    // secondly starting quoted
    val kj = new CSVParser().apply("\"Apple\",\"Bottle\",Cat,\"Dharma\",Exceed")

    kj should be (List("Apple", "Bottle", "Cat", "Dharma", "Exceed"))
  }

  test("Parsing a CSV with blank fields") {
    val k = new CSVParser().apply("\"Apply\",,\"Bottle\",,,\"Cat\"")

    k should be (List("Apply","","Bottle","","","Cat"))
  }

  test("Parsing a CSV with comma quoted fields") {
    val k = new CSVParser().apply("\"Alpha\",\"Gamma\",\"Delta, Epsilon\",,\"Lambda\"")

    k should be (List("Alpha", "Gamma", "Delta, Epsilon", "", "Lambda"))
  }

  test("Parsing a CSV with a blank last field") {
    val k = new CSVParser().apply("\"Alpha\",\"Beta\",")

    k should be (List("Alpha","Beta",""))
  }

  test("Parsing a CSV with a blank first field") {
    val k = CSVParser(",Alpha,Beta")

    k should be (List("","Alpha","Beta"))
  }

  test("Parsing a CSV with a blank first and last field") {
    val k = CSVParser(",Alpha,Beta,")

    k should be (List("","Alpha","Beta",""))
  }

  test("Complex CSV line") {
    val l = """WP0001052328,"Tier 1",Joe,Bloggs,joe@joeandlaura.com,"Joe Bloggs",joe@joeandlaura.com,85.00,"June 8th, 2012",,Joe,"Joe, Bloggs",No,,"21 or over (may choose to drink alcohol)",,,,"1234 Matteson, Los Angeles, Ca, 90066",joe@joeandlaura.com,3104805170,"I participate year round!","Arbol Ardentro","""
    val k = CSVParser(l)

    println(k)

    k(0) should be ("WP0001052328")
    k(1) should be ("Tier 1")
    k(2) should be ("Joe")
    k(3) should be ("Bloggs")
  }

  test("CSV with newlines in quoted fields (ugh - I know)") {
    val l = "One,Two,'Holy\nCow',Four\nSeven,Eight\n"
    val k = CSVParser.multi(l)(0)

    k(0) should be ("One")
    k(1) should be ("Two")
    k(2) should be ("Holy\nCow")
    k(3) should be ("Four")
  }


  test("CSV with CRLFs") {
    val l = "One,Two\r\nThree,Four\r\n"
    val k = CSVParser.multi(l)

    k(0)(0) should be ("One")
    k(0)(1) should be ("Two")
    k(1)(0) should be ("Three")
    k(1)(1) should be ("Four")
  }

  test("CSV with trailining commas") {
    val l = "A,B,,,\n"
    val k = CSVParser.multi(l)
    k.size should be (1)
    k(0).size should be (5)
    k(0)(0) should be ("A")
    k(0)(1) should be ("B")
    k(0)(2) should be ("")
    k(0)(3) should be ("")
    k(0)(4) should be ("")
  }

  test("CSV with double doubled double quotes") {
    val l = "A,B,\"Mike \"\"Fast Fingers\"\" Joe\",Bar\n"
    val k = CSVParser.multi(l)
    k should have size (1)
    k(0) should have size  (4)
    k(0)(0) should be ("A")
    k(0)(1) should be ("B")
    k(0)(2) should be ("Mike \"Fast Fingers\" Joe")
    k(0)(3) should be ("Bar")
  }

  test("CSV From Volunteers") {
    val l = "FIRST NAME,LAST NAME,PLAYA NAME,PHONE #,E-MAIL,POSITION,WAIVE V TIX,RV,TICKET LEVEL,TICKET LINK SENT,READY TO SEND NEXT ROUND,PAID\r\nJane,Greene,,1 (818) 427-3023,jane226@gmail.com,CIRCONAUTS,No,No,$40,,,\r\nAna,Jones,,337-555-5279,jane@mac.com,CIRCONAUTS,No,No,$40,,,\r\n";
    val k = CSVParser.multi(l)

    k(1)(0) should be ("Jane")
  }
}
