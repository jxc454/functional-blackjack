package com.blackjack

import scala.io.StdIn

object BlackJack {
  def main(args: Array[String]): Unit = {

    println("Let's play BlackJack!")

    // set up game
    val game = Game(
      minBet = 5,
      maxBet = 1000,
      player = new Player,
      dealer = new Dealer,
      state = CheckShoe
    )

    (new BlackJackGame).run(game)
  }
}

class BlackJackUserAction extends PlayerAction {
  def getAction(): PlayerMove = {
    BlackJackGame.userAction()
  }

  def hit: PlayerActionMethod = (game: Game, hand: BjHand) => {
    val (newHand: BjHand, newShoe: Shoe) = BlackJackCore.addOneCard(game, hand, hand.bet)

    if (Seq(newHand.handValue(), newHand.lowValue()).min > 21) {
      println(s"your cards: ${newHand.to_string()}")
      println("you busted!")
      println(s"balance: ${game.player.balance - hand.bet}")

      // hand busted
      game.copy(
        dealer=game.dealer.copy(
          shoe=newShoe),
        player=game.player.copy(
          balance=game.player.balance - hand.bet
        )
      )

    } else {
      // no bust, prompt for next action
      action(game.copy(
        dealer=game.dealer.copy(
          shoe=newShoe)
      ), newHand)
    }
  }

  def doubleDown: PlayerActionMethod = (game: Game, hand: BjHand) => {
    if (!BlackJackCore.checkCanDD(hand)){
      println("too late to double down!")
      action(game, hand)
    } else {
      val (newHand: BjHand, newShoe: Shoe) = BlackJackCore.addOneCard(game, hand, hand.bet * 2)

      println(s"your cards: ${newHand.cards.map(_.to_string()).mkString("")}" +
        (if (newHand.finalValue() > 21) "" else s" | value: ${newHand.finalValue()}"))

      if (newHand.finalValue > 21) {
        // hand busted
        println("you busted!")
        println(s"balance: ${game.player.balance - hand.bet * 2}")

        game.copy(
          dealer = game.dealer.copy(shoe = newShoe),
          player = game.player.copy(balance = game.player.balance - newHand.bet * 2))
      } else {
        // no bust
        game.copy(
          dealer = game.dealer.copy(shoe = newShoe),
          player = game.player.copy(hands = newHand +: game.player.hands))
      }
    }
  }

  def stay: PlayerActionMethod = (game: Game, hand: BjHand) => {
    game.copy(
      player=game.player.copy(hands=hand +: game.player.hands)
    )
  }

  def surrender: PlayerActionMethod = (game: Game, hand: BjHand) => {
    if (!BlackJackCore.checkCanSurrender(hand)) {
      println("Sorry, too late to surrender!")
      action(game, hand)
    } else {
      println("you surrendered!")
      println(s"balance: ${game.player.balance - hand.bet / 2}")

      game.copy(
        player=game.player.copy(balance=game.player.balance - hand.bet / 2)
      )
    }
  }

  def split: PlayerActionMethod = (game: Game, hand: BjHand) => {
    if (!BlackJackCore.checkSplittable(hand)) {
      println("Sorry, can't split that hand!")
      action(game, hand)
    } else {
      hand.cards.foldLeft(game)((acc, card) => {
        val (newCardOp: Option[Seq[BjCard]], newShoe: Shoe) = acc.dealer.shoe.deal(1).run(acc.dealer.shoe)

        val newHand: BjHand = newCardOp.map(newCard => card +: newCard) match {
          case Some(cards) => BjHand(cards=cards, bet=hand.bet)
          case _ =>
            println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
            sys.exit()
        }

        action(acc.copy(dealer=acc.dealer.copy(shoe=newShoe)), newHand)
      })
    }
  }

  def blackjack: PlayerActionMethod = (game: Game, hand: BjHand) => {
    println("you got BlackJack!")
    println(s"balance: ${game.player.balance + 1.5 * hand.bet}")

    game.copy(
      player=game.player.copy(
        balance=game.player.balance + 1.5 * hand.bet
      )
    )
  }

  def intro(game: Game, hand: BjHand): Unit = {
    println(s"your cards: ${hand.to_string()} | value: " +
      s"${if (hand.soft) hand.lowValue + "/" + hand.handValue() else hand.finalValue()}" +
      s"${if (hand.cards.size == 2 && hand.cards.head.pipName == hand.cards(1).pipName) " (splittable)" else ""}"
    )
  }
}

class BlackJackGame extends Play {

  def checkShoe(game: Game): Unit = {

    // just check the size of the shoe, create a new one if the size is < n
    if (game.dealer.shoe.size < 15) {
      run(game.copy(dealer = new Dealer))
    } else run(game.copy(state = DealHands))
  }

  def dealHands(game: Game): Unit = {
    println(s"true count: ${"%.2f".format(game.dealer.shoe.trueCount).toDouble}")

    // get bet
    val bet: Double = BlackJackGame.userBet()

    // check if bet is in range
    if (bet <= game.minBet || bet > game.maxBet) {
      println("that bet is out of range, try again.")
      run(game)
    } else {
      // deal cards
      val (handsCards: Seq[Seq[BjCard]], newShoe: Shoe) = BlackJackCore.dealHands(game.dealer.shoe)

      val playerHand: BjHand = BjHand(handsCards.head, bet)
      val dealerHand: BjHand = BjHand(handsCards(1), 0.0)

      println(s"dealer card: ${dealerHand.cards.head.to_string()}")

      val nextState: GameState = if (dealerHand.handValue() == 21) {
        println(s"your cards: ${playerHand.to_string()}")
        SettleUp
      } else PlayerAct

      run(
        game.copy(
          dealer = game.dealer.copy(
            shoe = newShoe,
            hand = dealerHand),
          player = game.player.copy(
            hands = Seq(playerHand)),
          state = nextState
        )
      )
    }
  }

  def playerAct(game: Game): Unit = {
    run(
      game.player.hands.foldLeft(game.copy(player=game.player.copy(hands=Seq())))((acc, hand) =>
        (new BlackJackUserAction).action(acc, hand)
      ).copy(state=DealerAct)
    )
  }

  def dealerAct(game: Game): Unit = {
    if (game.player.hands.isEmpty) {
      println(s"dealer cards: ${game.dealer.hand.to_string()}")

      run(
        game.copy(state=CheckShoe)
      )
    } else {
      game.dealer.rules.getAction(game.dealer.hand) match {
        case "hit" =>
          val (newHand: BjHand, newShoe: Shoe) = BlackJackCore.addOneCard(game, game.dealer.hand, 0)

          run(
            game.copy(
              dealer=game.dealer.copy(
                shoe=newShoe,
                hand=newHand),
              state=DealerAct
            )
          )
        case "stay" =>
          run(
            game.copy(
              state=SettleUp
            )
          )
        case _ =>
          run(
            game.copy(
              state=SettleUp
            )
          )
      }
    }
  }

  def settleUp(game: Game): Unit = {
    println(s"dealer cards: ${game.dealer.hand.to_string()}")

    val dealerHandValue: Int = game.dealer.hand.finalValue()

    val newBalance: Double = game.player.hands.foldLeft(game.player.balance)((acc, hand) => {
      val playerHandValue: Int = hand.finalValue()

      if (dealerHandValue > 21) {
        println("dealer busted!")
        acc + hand.bet
      } else if (playerHandValue < dealerHandValue) {
        println("you lose!")
        acc - hand.bet
      } else if (playerHandValue > dealerHandValue) {
        println("you won!")
        acc + hand.bet
      } else if (playerHandValue == dealerHandValue) {
        println("push!")
        acc
      } else acc
    })

    println(s"balance: $newBalance")

    val newGame = Game(
      minBet = 5,
      maxBet = 1000,
      player = game.player.copy(
        hands = Seq(),
        balance = newBalance
      ),
      dealer = game.dealer.copy(
        hand = null
      ),
      state = CheckShoe
    )

    (new BlackJackGame).run(newGame)
  }
}

object BlackJackGame {

  @scala.annotation.tailrec
  def userBet(): Double = {
    // prompt player for bet amount
    println("place your bet")

    try {
      StdIn.readDouble()
    } catch {
      case _: Exception =>
        println("couldn't accept that bet, try a different bet.")
        userBet()
    }
  }

  @scala.annotation.tailrec
  def userAction(): PlayerMove = {
    // prompt user to select an action
    val action: String = scala.io.StdIn.readLine("your move:")

    UserActions.actionsMap.get(action) match {
      case Some(a) => a
      case _ =>
        println("invalid action, choose a valid action: " + UserActions.actions.mkString(", "))
        userAction()
    }
  }

}
