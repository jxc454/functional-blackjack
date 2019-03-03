package com.blackjack

trait PlayerMove
object Hit extends PlayerMove
object DoubleDown extends PlayerMove
object Stay extends PlayerMove
object Surrender extends PlayerMove
object Split extends PlayerMove

trait GameState
object CheckShoe extends GameState
object DealHands extends GameState
object PlayerAct extends GameState
object DealerAct extends GameState
object SettleUp extends GameState


trait PlayerAction {

  type PlayerActionMethod = (Game, BjHand) => Game
  
  def hit: PlayerActionMethod
  def doubleDown: PlayerActionMethod
  def stay: PlayerActionMethod
  def surrender: PlayerActionMethod
  def split: PlayerActionMethod
  def blackjack: PlayerActionMethod
  def intro(game: Game, hand: BjHand): Unit

  def getAction(): PlayerMove

  final def action: PlayerActionMethod = (game: Game, hand: BjHand) => {
    intro(game, hand)

    if (hand.handValue() == 21 && hand.cards.size == 2) {
      blackjack(game, hand)
    } else {
      getAction() match {
        case Hit => hit(game, hand)
        case Stay => stay(game, hand)
        case DoubleDown => doubleDown(game, hand)
        case Surrender => surrender(game, hand)
        case Split => split(game, hand)
      }
    }
  }
}

trait Play {

  def checkShoe(game: Game): Unit
  def dealHands(game: Game): Unit
  def playerAct(game: Game): Unit
  def dealerAct(game: Game): Unit
  def settleUp(game: Game): Unit

//  @scala.annotation.tailrec
  final def run(game: Game): Unit = {
    game.state match {
      case CheckShoe => checkShoe(game)
      case DealHands => dealHands(game)
      case PlayerAct => playerAct(game)
      case DealerAct => dealerAct(game)
      case SettleUp => settleUp(game)
    }
  }
}

object BlackJackCore {

  @scala.annotation.tailrec
  def dealHands(shoe: Shoe): (Seq[Seq[BjCard]], Shoe) = {
    val (hands: Option[Seq[Seq[BjCard]]], newShoe) = Shoe.dealMultipleHands(shoe, 2)

    hands match {
      case Some(handsList) => (handsList, newShoe)
      case None => dealHands(BjCard.createShoe(1))
    }
  }

  def checkSplittable(hand: BjHand): Boolean = {
    val (card1, card2) = hand.cards.splitAt(1)
    card1.head.pip == card2.head.pip
  }

  def checkCanDD(hand: BjHand): Boolean = hand.cards.length == 2

  def checkCanSurrender(hand: BjHand): Boolean = hand.cards.length == 2

  def addOneCard(game: Game, hand: BjHand, newBet: Double): (BjHand, Shoe) = {
    game.dealer.shoe.deal(1).flatMap(newCard =>
      State(newShoe => {
      val newHand: BjHand = newCard.map(card => card ++ hand.cards) match {
        case Some(cards) => BjHand(cards = cards, bet = newBet)
        case _ =>
          println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
          sys.exit()
      }

      (newHand, newShoe)
    })).run(game.dealer.shoe)
  }
}