package ru.spbau.jvm.scala

import akka.actor.Status.Success
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration.DurationInt
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.WeatherActor.AddWeather

import scala.collection.mutable

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
              (database ? GetWeather(city)).onComplete {
                case Success((time, json)) => {
                  if (System.currentTimeMillis() - time > 600000)
                    reply(AddWeather(message.char.id, city))
                  else
                    reply(json)
                }
                case Success(None) =>
                  reply(AddWeather(message.char.id, city))
                case _ =>
                  reply("Database error:(")
              }
            case GetStatistic(qNumbers) =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? GetStatistic(message.chat.id)).onComplete {
                case Success(buffer) =>
                  reply(buffer.map {
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