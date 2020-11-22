package com.example

import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Signal
import akka.actor.typed.PostStop

object StartStopActor1 {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new StartStopActor1(context))
}

class StartStopActor1(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  println(s"${context.self} started")
  context.spawn(StartStopActor2(), "Clooney")
  context.spawn(StartStopActor2(), "Oscar")
  context.spawn(StartStopActor2(), "Alice")
  context.spawn(StartStopActor2(), "Bob")

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "stop" => Behaviors.stopped
    }

  override def onSignal: PartialFunction[Signal, Behavior[String]] = {
    case PostStop =>
      println(s"${context.self} stopped")
      this
  }

}

object StartStopActor2 {
  def apply(): Behavior[String] =
    Behaviors.setup(new StartStopActor2(_))
}

class StartStopActor2(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  println(s"${context.self} started")

  override def onMessage(msg: String): Behavior[String] = {
    // no messages handled by this actor
    Behaviors.unhandled
  }

  override def onSignal: PartialFunction[Signal, Behavior[String]] = {
    case PostStop =>
      println(s"${context.self} stopped")
      this
  }

}


object StartStopMain {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new StartStopMain(context))

}

class StartStopMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "start" =>
        val first = context.spawn(StartStopActor1(), "Charles")
        first ! "stop"
        this
    }
}

object StartStopExample extends App {
  val testSystem = ActorSystem(StartStopMain(), "testSystem")
  testSystem ! "start"
  println(s"ActorSystem: $testSystem")
}


