package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import scala.util.Random
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

Client                  Serveur                           Barman                Bar
  |                        |                               |                     |
  |--------------------------------Bonjour(c)----------------------------------->|
  |<------------------VotreServeur(s)--------------------------------------------|
  |                        |                               |                     |
  |--Commande("café",c)--->|                               |                     |
  |                        |----Commande("café", c, s)---->|                     |
  |                        |<---Livraison("café", c, 150)--|                     |
  |                        |                               |                     |
  |<-Livraison("café",150)-|                               |                     |
  |----------------------Livraison("café", 150)------pour les stats------------->|
  |--Payer(150,c)--------->|                               |                     |


Pour commencer :
- [X] Créer un bar
- [X] Créer un serveur
- [X] Créer Bob (dans le bar)
- [X] Bob dit bonjour au bar

Pour continuer :
- [X] Bob s'adresse au bar pour demander l'ActorRef d'un serveur
- [X] le bar réponds avec un ActorRef de serveur

Pour finir :
- [-] Bob passe sa commande le serveur la gère
*/

object Serveur {
  trait Command
  final case class Order(conso: String, consumer: ActorRef[Client.Command]) extends Command

  def apply(): Behavior[Command] =
    Behaviors.setup(context => new Serveur(context))
}

class Serveur(context: ActorContext[Serveur.Command]) extends AbstractBehavior[Serveur.Command](context) {
  context.log.info("Hello !")
  import Serveur._

  override def onMessage(msg: Command): Behavior[Command] =
    msg match {
      case msg @ Order(conso, client) =>
        context.log.info("TODO: process {} and answer", msg)
        this
    }
}

object Client {
  trait Command
  final case class VotreServeur(serveur: ActorRef[Serveur.Command]) extends Command

  def apply(bar: ActorRef[Bar.Command]): Behavior[Client.Command] =
    Behaviors.setup(context => new Client(bar, context))
}

class Client(bar: ActorRef[Bar.Command], context: ActorContext[Client.Command]) extends AbstractBehavior[Client.Command](context) {
  var monserveur: Option[ActorRef[Serveur.Command]] = None 
  context.log.info("Hello !")
  import Bar.Bonjour
  import Client._
  import Serveur.Order

  bar ! Bonjour(context.self)
 
  override def onMessage(msg: Command): Behavior[Command] =
    msg match {
      case VotreServeur(serveur) =>
        monserveur = Some(serveur)
        serveur ! Order("un café", context.self)
        this
    }
}

object BarMain {
  def apply(): Behavior[String] =
    Behaviors.setup(context => new BarMain(context))
}

class BarMain(context: ActorContext[String]) extends AbstractBehavior[String](context) {
   var serveurs = Set.empty[ActorRef[Serveur.Command]]

  override def onMessage(msg: String): Behavior[String] =
    msg match {
      case "start" =>
        /* créons un bar */
        val bar = context.spawn(Bar(), "LeTurfu")
        bar ! Bar.Start
        /* créons un premier client, Bob et donnons lui l'adresse du Bar */
        val bob = context.spawn(Client(bar), "Bob")
        this
    }
}

object Bar {
  trait Command
  final case object Start extends Command
  final case class Bonjour(replyTo: ActorRef[Client.Command]) extends Command

  def apply(): Behavior[Command] =
    Behaviors.setup(context => new Bar(context))
}

class Bar(context: ActorContext[Bar.Command]) extends AbstractBehavior[Bar.Command](context) {
  var serveurs = Set.empty[ActorRef[Serveur.Command]]
  import Bar._

  override def onMessage(msg: Command): Behavior[Command] =
    msg match {
      case Start =>
        /* créons des serveurs */
        serveurs = Range(1,3).map( 
          (i) => context.spawn(Serveur(), s"serveur-$i")
        ).toSet
        this
      case Bonjour(client) =>
        import Client.VotreServeur
        context.log.info("On a un client {} !", client)
        val i = Random.nextInt(serveurs.size)
        val msg = VotreServeur(serveurs.view(i, i + 1).head)
        client ! msg
        this
    }
}

object LeBar extends App {
  val testSystem = ActorSystem(BarMain(), "bar")
  testSystem ! "start"
}
