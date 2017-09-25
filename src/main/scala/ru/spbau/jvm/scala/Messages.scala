package ru.spbau.jvm.scala

trait UserMessage

case class GetWeather(word: String) extends UserMessage

case class GetStatistic(qNumber: Int) extends UserMessage

case object WrongMessage extends UserMessage

