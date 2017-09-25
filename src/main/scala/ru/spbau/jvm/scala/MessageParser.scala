package ru.spbau.jvm.scala

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val wordParser: Parser[String] = raw"\S+".r
  val intParser: Parser[Int] = "[1-9][0-9]*".r ^^ {
    _.toInt
  }

  val getWeather : Parser[GetWeather] =
    "[Ww]heather in".r ~> wordParser ^^ {
      case word => GetWeather(word)
    }

  val getStatistic : Parser[GetStatistic] =
    "[Ss]how last".r ~> intParser ~> "queries".r ^^ {
      case qNumber => GetStatistic(qNumber)
    }

  val userMessage : Parser[UserMessage] =
    getWeather | getStatistic
}

object MessageParser extends MessageParser {
  def parse(text: String) : UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}