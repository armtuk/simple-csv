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
}
