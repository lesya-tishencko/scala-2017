package ru.spbau.jvm.scala

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}

import akka.actor.Status.Success
import akka.persistence.PersistentActor
import ru.spbau.jvm.scala.WeatherActor._


class WeatherActor(val cache: Cache) extends PersistentActor {

  type WeatherRequestResult = (Int, Option[String])

  val QueryString = "http://api.openweathermap.org/data/2.5/weather?appid=4ca042807dbef1d736b5a0a79ffee657&q="
  val SuccessRequestCode = 200
  val DefaultCacheLiveTime = 600000


  override def receiveRecover: Receive = {
    case evt: Event => ""
  }

  override def receiveCommand: Receive = {
    case GetWeatherByName(userID, city) =>
      sender ! Success(System.currentTimeMillis(), getWeather(userID, city))
    case GetStatisticById(id) =>
      sender ! cache.getStatisticForUserByID(id)
  }

  private def getWeather(userID: Long, city: String): String = {
    val cachedWeather = cache.getCachedWeatherForCity(city)
    if (isExpiredCachedData(cachedWeather._1)) {
      val weather = requestWeather(city)
      if (isCorrectWeather(weather))
        updateWeather(userID, city, weather)
    }

    cache.getCachedWeatherForCity(city)._2
  }

  private def updateWeather(userID: Long, city: String, weather: WeatherRequestResult): Unit = {
    cache.updateCacheForCity(city, weather._2.get)
    cache.updateLogForUser(userID, s"city: $city - ${weather._2.get}")
  }

  private def isCorrectWeather(weather: WeatherRequestResult): Boolean =
    weather._1 == SuccessRequestCode && weather._2.isDefined

  private def isExpiredCachedData(cachedTime: Long, liveTime: Long = DefaultCacheLiveTime): Boolean = {
    System.currentTimeMillis() - cachedTime.asInstanceOf[Long] > liveTime
  }

  private def requestWeather(city: String): WeatherRequestResult = {
    val connection = new URL(QueryString ++ city).openConnection.asInstanceOf[HttpURLConnection]

    connection.setRequestMethod("GET")
    if (connection.getResponseCode == SuccessRequestCode) {
      val br = new BufferedReader(new InputStreamReader(connection.getInputStream()))

      val sb = new StringBuilder()
      var line = br.readLine()

      while (line != null) {
        sb.append(line)
        sb.append("\n")
        line = br.readLine()
      }
      br.close()

      return (SuccessRequestCode, Some(sb.toString()))
    }
    return (connection.getResponseCode, None)
  }

  override def persistenceId = "weather-database"
}

object WeatherActor {

  trait Event

  case class GetWeatherByName(userID: Long, city: String)
  case class GetStatisticById(id: Long)
}