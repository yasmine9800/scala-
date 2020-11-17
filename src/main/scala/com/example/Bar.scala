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
- [ ] Créer un serveur
- [ ] Créer Bob (dans le bar)
- [ ] Bob dit bonjour au bar

Pour continuer :
- [ ] Bob s'adresse au bar pour demander l'ActorRef d'un serveur
- [ ] le bar réponds avec un ActorRef de serveur

Pour finir :
- [ ] Bob passe sa commande le serveur la gère
*/

object BarMain {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new BarMain(context))
}

class BarMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "start" =>
        context.log.info("TODO")
        this
    }
}

object LeBar extends App {
  val testSystem = ActorSystem(BarMain(), "bar")
  testSystem ! "start"
}
