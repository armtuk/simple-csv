package com.plexq.csv

import util.parsing.combinator.RegexParsers
import collection.JavaConversions._

class CSVParser extends RegexParsers {
  def parseFile(f: java.io.File): Array[Array[java.lang.String]] = applyFile(f).map(_.toArray).toArray
  def parseString(s: java.lang.String): Array[java.lang.String] = apply(s).toArray

  def applyFile(f: java.io.File) = apply(f)
  def apply(f: java.io.File): Iterator[List[String]] = io.Source.fromFile(f).getLines().map(apply(_))
  def apply(s: String): List[String] = parseAll(fromCsv, s) match {
    case Success(result, _) => {
      result
    }
    case failure: NoSuccess => {throw new Exception("Parse Failed " + failure.toString)}
  }

  def fromCsv:Parser[List[String]] = rep1(mainToken <~ ",") ~ mainToken ^^ {case x ~ y => x :+ y}
  def mainToken = (doubleQuotedTerm | singleQuotedTerm | unquotedTerm | "") ^^ {case "," => ""; case a => a}
  def doubleQuotedTerm: Parser[String] = "\"" ~> "[^\"]+".r <~ "\"" ^^ {case a => (""/:a)(_+_)}
  def singleQuotedTerm = "'" ~> "[^']+".r <~ "'" ^^ {case a => (""/:a)(_+_)}
  def unquotedTerm = "[^,]+".r ^^ {case a => (""/:a)(_+_)}

  override def skipWhitespace = false
}
