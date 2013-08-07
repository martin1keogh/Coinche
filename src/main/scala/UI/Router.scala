package UI

import akka.actor.{ActorRef, Actor}
import UI.Reader._
import GameLogic.Joueur

class Router extends Actor{
  import Router._

  var listSurCoinche = List[Joueur]()

  def waitingBid(s:ActorRef):Receive = {
    case m : BiddingMessage => {
      s forward m
      context.become(receive)
    }
    case Normal => context.become(receive)
    case AwaitCard => context.become(waitingCard(sender))
    case StopWaiting => context.become(receive)
    case StopGame => {
      context.become(receive)
      s forward StopGame
    }
    case _ => ()
  }

  def waitingCard(s:ActorRef):Receive = {
    case m : PlayingMessage => {
      s forward m
      context.become(receive)
    }
    case Normal => context.become(receive)
    case AwaitBid => context.become(waitingBid(sender))
    case StopWaiting => context.become(receive)
    case StopGame => {
      context.become(receive)
      s forward StopGame
    }
    case _ => ()
  }

  def waitingSurCoinche:Receive = {
    case SurCoinche(j) => listSurCoinche = j :: listSurCoinche
    case ReturnResults => {
      context.become(receive)
      sender ! listSurCoinche
    }
    case _ => ()
  }

  def receive:Receive = {
    case AwaitBid => context.become(waitingBid(sender))
    case AwaitCard => context.become(waitingCard(sender))
    case AwaitSurCoinche => {
      listSurCoinche = List[Joueur]()
      context.become(waitingSurCoinche)
    }
    case _ => ()
  }
}

object Router{
  sealed trait State
  case object AwaitBid extends State
  case object AwaitCard extends State
  case object AwaitSurCoinche extends State
  case object ReturnResults extends State
  case object Normal extends State
}
