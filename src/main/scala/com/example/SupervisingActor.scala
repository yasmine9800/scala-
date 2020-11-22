package com.example

import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Signal
import akka.actor.typed.PostStop
import akka.actor.typed.PreRestart
import akka.actor.typed.SupervisorStrategy

object SupervisingActor {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new SupervisingActor(context))
}

class SupervisingActor(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  private val child = context.spawn(
    Behaviors.supervise(SupervisedActor()).onFailure(SupervisorStrategy.restart),
    name = "child")
  
  private val child2 = context.spawn(SupervisedActor(), "child2")

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "failChild" =>
        child ! "fail"
        child2 ! "fail"
        this
    }
}

object SupervisedActor {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new SupervisedActor(context))
}

class SupervisedActor(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  context.log.info("supervised actor started")

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "fail" =>
        context.log.info("supervised actor fails now")
        throw new Exception("I failed!")
    }

  override def onSignal: PartialFunction[Signal, Behavior[String]] = {
    case PreRestart =>
      context.log.info("supervised actor will be restarted")
      this
    case PostStop =>
      context.log.info("supervised actor stopped")
      this
  }

}

object SupervisingMain {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new SupervisingMain(context))
}

class SupervisingMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "start" =>
        val supervisingActor = context.spawn(SupervisingActor(), "supervising-actor")
        supervisingActor ! "failChild"
        this
    }
}

object SupervisingActorsExample extends App {
  val testSystem = ActorSystem(SupervisingMain(), "testSystem")
  testSystem ! "start"
}


