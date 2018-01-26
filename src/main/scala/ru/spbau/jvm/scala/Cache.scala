package ru.spbau.jvm.scala

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Cache {
  type City = String
  type LastUpdateTime = Long
  type WeatherInfo = String
  type UserRequestAndAnswer = String
  type UserId = Long

  type WeatherCache = mutable.HashMap[City, (LastUpdateTime, WeatherInfo)]
  type WeatherLog = mutable.HashMap[UserId, ArrayBuffer[UserRequestAndAnswer]]

  private val weatherCache: WeatherCache = mutable.HashMap.empty
  private val weatherLog: WeatherLog = mutable.HashMap.empty

  def getCachedWeatherForCity(city: City)= weatherCache.getOrElse(city, (0l, ""))
  def getStatisticForUserByID(id: UserId) = weatherLog.getOrElse(id, ArrayBuffer.empty)

  def updateCacheForCity(city: City, weather: WeatherInfo) = {
    weatherCache.update(city, (System.currentTimeMillis(), weather))
  }

  def updateLogForUser(userID: UserId, newRecord: UserRequestAndAnswer) = {
    weatherLog.getOrElseUpdate(userID, ArrayBuffer.empty) += newRecord
  }

}
