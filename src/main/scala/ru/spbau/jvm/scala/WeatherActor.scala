package ru.spbau.jvm.scala

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import akka.persistence.PersistentActor
import ru.spbau.jvm.scala.WeatherActor._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class WeatherActor extends PersistentActor {
  val queryString = "http://api.openweathermap.org/data/2.5/weather?appid=4ca042807dbef1d736b5a0a79ffee657&q="
  val mapCity: mutable.HashMap[String, (Long, String)] = mutable.HashMap.empty
  val map: mutable.HashMap[Long, ArrayBuffer[String]] = mutable.HashMap.empty

  def receiveEvent(event: Event): String = {
    event match {
      case AddWeather(id, city) => {
        val connection = (new URL(queryString ++ city)).openConnection.asInstanceOf[HttpURLConnection]
        connection.setRequestMethod("GET")
        if (connection.getResponseCode() == 200) {
          val br = new BufferedReader(new InputStreamReader(connection.getInputStream()));

          var line = "";
          val sb = new StringBuilder();
          while ((line = br.readLine()) != null) {
            sb.append(line);
            sb.append("\n");
          }
          br.close();
          mapCity.update(city, (System.currentTimeMillis(), sb.toString()));
          map.getOrElseUpdate(id, ArrayBuffer.empty) += sb.toString();
          return sb.toString();
        }
        return "Answer from service: " + connection.getResponseCode().toString();
      }
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetWeatherByName(city) =>
      sender ! mapCity.getOrElse(city, (0, ""))
    case GetStatisticById(id) =>
      sender ! map.getOrElse(id, ArrayBuffer.empty)
  }

  override def persistenceId = "weather-database"
}

object WeatherActor {

  trait Event

  case class AddWeather(id: Long, city: String) extends Event

  case class GetWeatherByName(city: String)

  case class GetStatisticById(id: Long)

  case class JsonAnsw(buffer: (Long, String))
}