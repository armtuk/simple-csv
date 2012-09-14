package com.plexq.csv

import util.parsing.combinator.RegexParsers
import collection.JavaConversions._

class CSVParser extends RegexParsers {
  def parseFile(f: java.io.File): Array[Array[java.lang.String]] = applyFile(f).map(_.toArray).toArray
  def parseString(s: java.lang.String): Array[java.lang.String] = apply(s).toArray
  def parseMulti(s: java.lang.String): Array[Array[java.lang.String]] = multi(s).map(_.toArray).toArray

  def applyFile(f: java.io.File) = apply(f)
  def apply(f: java.io.File): List[List[String]] = parseAll(fromFile, io.Source.fromFile(f).mkString) match {
    case Success(result, _) => {
      result
    }
    case failure: NoSuccess => {throw new Exception("Parse failed " + failure.toString)}
  }
  def apply(s: String): List[String] = parseAll(fromCsv, s) match {
    case Success(result, _) => {
      result
    }
    case failure: NoSuccess => {throw new Exception("Parse Failed " + failure.toString)}
  }

  def multi(s: String): List[List[String]] = parseAll(fromFile, s) match {
    case Success(result, _) => {
      result
    }
    case failure: NoSuccess => {throw new Exception("Parse Failed " + failure.toString)}
  }

  def fromFile:Parser[List[List[String]]] = rep1(fromCsv <~ "\n") ^^ {case x => x}
  def fromCsv:Parser[List[String]] = rep1(mainToken <~ ",") ~ mainToken ^^ {case x ~ y => x :+ y}
  def mainToken = (doubleQuotedTerm | singleQuotedTerm | unquotedTerm | "") ^^ {case "," => ""; case a => a}
  def doubleQuotedTerm: Parser[String] = "\"" ~> "[^\"]+".r <~ "\"" ^^ {case a => (""/:a)(_+_)}
  def singleQuotedTerm = "'" ~> "[^']+".r <~ "'" ^^ {case a => (""/:a)(_+_)}
  def unquotedTerm = "[^,\\n\\r]+".r ^^ {case a => (""/:a)(_+_)}

  override def skipWhitespace = false
}

object CSVParser {
  def apply(f: java.io.File) = new CSVParser().apply(f)
  def apply(s: String) = new CSVParser().apply(s)
  def multi(s: String) = new CSVParser().multi(s)
}
