package ru.spbau.jvm.scala

import scala.util.Success
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration.DurationInt
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.WeatherActor.{AddWeather, GetStatisticById, GetWeatherByName}

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
            case GetWeather(city) =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? GetWeatherByName(city)).onComplete {
                case Success((0, "")) =>
                  reply(AddWeather(message.chat.id, city).toString())
                case Success((time, json)) => {
                  if (System.currentTimeMillis() - time.asInstanceOf[Long] > 600000)
                    reply(AddWeather(message.chat.id, city).toString())
                  else
                    reply(json.asInstanceOf[String])
                }
                case _ =>
                  reply("Database error:(")
              }
            case GetStatistic(qNumbers) =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? GetStatisticById(message.chat.id)).onComplete {
                case Success(buffer) =>
                  reply(buffer.asInstanceOf[ArrayBuffer[String]].map {
                    case (json) => s"$json"
                  }.mkString("\n"))
                case _ =>
                  reply("Database error:(")
              }
            case WrongMessage =>
              reply("Uncorrect command:(")
          }
      }
  }
}