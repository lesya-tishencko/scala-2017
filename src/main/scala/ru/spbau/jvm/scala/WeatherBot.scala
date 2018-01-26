package ru.spbau.jvm.scala

import scala.util.Success
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration.DurationInt
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.Message
import ru.spbau.jvm.scala.WeatherActor.{GetStatisticById, GetWeatherByName}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AskActor(bot: WeatherBot) extends Actor {
  override def receive = {
    case _ => bot.askUsers()
  }
}

class WeatherBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {
  def askUsers(): Unit = {}

  val map: mutable.HashMap[Long, String] = mutable.HashMap.empty

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        if (map.contains(message.chat.id)) {
          if (text == map(message.chat.id)) {
            reply("All right")
          } else {
            reply("Something went wrong")
          }
          map.remove(message.chat.id)
        } else
          MessageParser.parse(text) match {
            case GetWeather(city) => handleWeatherAnswer(message, city)
            case GetStatistic(recordCount) => handleStatistic(message, recordCount)
            case WrongMessage => handleWrongMessage(message)
          }
      }
  }

  private def handleWeatherAnswer(msg: Message, city: String): Unit = {
    implicit val message = msg
    implicit val timeout: Timeout = Timeout(10.second)
    (database ? GetWeatherByName(msg.chat.id, city)).onComplete {
      case Success((time, json)) =>
        reply(s"Weather: ${json.asInstanceOf[String]}")
      case evt =>
        reply("error :(")
    }
  }

  private def handleStatistic(msg: Message, recordCount: Int): Unit = {
    implicit val message = msg
    implicit val timeout: Timeout = Timeout(10.second)
    (database ? GetStatisticById(message.chat.id)).onComplete {
      case Success(buffer) =>
        var result: String = buffer.asInstanceOf[ArrayBuffer[String]].map { case (json) => s"$json" }.take(recordCount).mkString("\n")
        result = if (result.isEmpty) "<history is empty>" else result
        reply(result)
      case _ =>
        reply("Database error:(")
    }
  }

  private def handleWrongMessage(msg: Message): Unit = {
    implicit val message = msg
    reply("Uncorrect command:(")
  }

}