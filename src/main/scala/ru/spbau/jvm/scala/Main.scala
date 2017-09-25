package ru.spbau.jvm.scala

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension

object Main extends App {
  val token = "361159336:AAFe5ZaPiAE1ZlGtflDrkEwxlupqKte0_Es"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[WheatherActor]))

  private val bot = new WheatherBot(token, database)
  val actor = system.actorOf(Props(classOf[AskActor], bot))

  scheduler.createSchedule("every minute", None, "	0/1 * * 1/1 * ? *")
  scheduler.schedule("every minute", actor, "Ask")

  bot.run()
}