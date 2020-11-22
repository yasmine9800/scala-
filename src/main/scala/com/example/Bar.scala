package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext

/*
Bob avise un bar
Bob entre dans le bar
Bob hèle un serveur
le serveur demande à Bob ce qu'il veut boire
Bob dit un café

le serveur note la commande
le serveur prévient le barman
le barman prévient le serveur que la commande est prète
le serveur amène la commande à Bob

le serveur demande à Bob de régler la commande
Bob règle la commande
[ Bob avise son ami Oscar dans la rue
Bob commande deux cafés un autre pour lui et un pour son ami ]

Acteurs :
Bar
Serveurs
Consommateurs
Barmans
…

Pour commencer :
- [X] Créer un bar
- [X] Créer un serveur
- [X] Créer Bob (dans le bar)
- [X] Bob dit bonjour au bar

Pour continuer :
- [ ] Bob s'adresse au bar pour demander l'ActorRef d'un serveur
- [ ] le bar réponds avec un ActorRef de serveur

Pour finir :
- [ ] Bob passe sa commande le serveur la gère
*/

object Serveur {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new Serveur(context))
}

class Serveur(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  context.log.info("Hello !")

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "garçon!" =>
        context.log.info("TODO")
        this
    }
}

object Client {
  def apply(bar: ActorRef[String]): Behavior[String] =
    Behaviors.setup(context => new Client(bar, context))
}

class Client(bar: ActorRef[String], context: ActorContext[String]) extends AbstractBehavior[String](context) {
  context.log.info("Hello !")
  bar ! "Bonjour"

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "bonjour" =>
        context.log.info("TODO")
        this
    }
}


object BarMain {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new BarMain(context))
}

class BarMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  var serveurs = Set.empty[ActorRef[String]]

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "start" =>
        serveurs = Range(1,3).map( 
          (i) => context.spawn(Serveur(), s"serveur-$i")
        ).toSet
        val bob = context.spawn(Client(context.self), "Bob")
        this
      case "Bonjour" =>
        context.log.info("On a un client !")
        this
    }
}

object LeBar extends App {
  val testSystem = ActorSystem(BarMain(), "bar")
  testSystem ! "start"
}
