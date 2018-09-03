package com.blackjack

import scala.io.StdIn
import scopt.OptionParser

case class Config(message: String = "")

object BlackJack {
  def main(args: Array[String]): Unit = {

    // example scopt snippets
    val parser = new OptionParser[Config]("scopt") {
      head("scopt", "3.x")

      opt[String]('i', "input") /*required()*/ action { (x, c) => c.copy(message = x) } text "playback message"
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val message: String = config.message
        println (message)

      case None =>
    }

    println("Let's play BlackJack!")

    // set up game
    val game = Game(
      minBet = 5,
      maxBet = 1000,
      player = new Player,
      dealer = new Dealer,
      state = "checkShoe"
    )

    (new BlackJackGame).play(game)
  }
}

class BlackJackGame {

  @scala.annotation.tailrec
  final def play(game: Game): Unit = {

    game.state match {
      case "checkShoe" =>
        // just check the size of the shoe, create a new one if the size is < n
        if (game.dealer.shoe.size < 15) {
          play(game.copy(dealer = new Dealer))
        } else play(game.copy(state = "dealHands"))

      case "dealHands" =>
        // get bet
        val bet: Double = BlackJackGame.userBet()

        // check if bet is in range
        if (bet <= game.minBet || bet > game.maxBet) {
          println("that bet is out of range, try again.")
          play(game)
        } else {
          // deal cards
          val (handsCards: Seq[Seq[BjCard]], newShoe: Shoe) = BlackJackGame.dealHands(game.dealer.shoe)

          val playerHand: BjHand = BjHand(handsCards.head, bet)
          val dealerHand: BjHand = BjHand(handsCards(1), 0.0)

          println(s"dealer card: ${dealerHand.cards.head.to_string()}")
          println(s"your cards: ${playerHand.to_string()} | value: ${playerHand.handValue()}")

          play(
            game.copy(
              dealer = game.dealer.copy(
                shoe = newShoe,
                hand = dealerHand),
              player = game.player.copy(
                hand = playerHand),
              state = "action"
            )
          )
        }

      case "action" =>

        BlackJackGame.userAction() match {
          case "h" =>
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val newHand: BjHand = newCard.map(card => game.player.hand.copy(cards=card ++ game.player.hand.cards)) match {
              case Some(hand) => hand
              case _ =>
                println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
                sys.exit()
            }

            println(s"dealer card: ${game.dealer.hand.cards.head.to_string()}")
            println(s"your cards: ${newHand.cards.map(_.to_string()).mkString("")} | value: ${newHand.handValue()}")

            play(
              game.copy(
                dealer=game.dealer.copy(
                  shoe=newShoe),
                player=game.player.copy(
                hand=newHand),
                state= if (newHand.handValue() > 21) "settleUp" else "action"
              )
            )
          case "s" =>
            play(
              game.copy(
                state="dealerAction"
              )
            )
          case "d" =>
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val newHand: BjHand = newCard.map(card =>
              BjHand(cards=card ++ game.player.hand.cards, bet=game.player.hand.bet * 2)) match {
                case Some(hand) => hand
                case _ =>
                  println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
                  sys.exit()
            }

            println(s"dealer card: ${game.dealer.hand.cards.head.to_string()}")
            println(s"your cards: ${newHand.cards.map(_.to_string()).mkString("")} | value: ${newHand.handValue()}")

            play(
              game.copy(
                dealer=game.dealer.copy(
                  shoe=newShoe
                ),
                player=game.player.copy(
                  hand=newHand
                ),
                state="dealerAction"
              )
            )
          case "r" =>
            play(
              game.copy(
                player=game.player.copy(
                  hand=game.player.hand.copy(
                    surrendered=true
                  )
                ),
                state="settleUp"
              )
            )
        }

      case "dealerAction" =>
        game.dealer.rules.getAction(game.dealer.hand) match {
          case "hit" =>
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val newHand: BjHand = newCard match {
              case Some(card) => game.dealer.hand.copy(cards=card ++ game.dealer.hand.cards)
              case _ => sys.exit
            }

            play(
              game.copy(
                dealer=game.dealer.copy(
                  shoe=newShoe,
                  hand=newHand
                  ),
                state="dealerAction"
              )
            )
          case "stay" =>
            play(
              game.copy(
                state="settleUp"
              )
            )
          case _ =>
            play(
              game.copy(
                state="settleUp"
              )
            )
        }
      case "settleUp" =>
        val playerHandValue: Int = game.player.hand.handValue()
        val dealerHandValue: Int = game.dealer.hand.handValue()

        val newBalance: Double = if (game.player.hand.surrendered) {
          println(s"dealer cards: ${game.dealer.hand.to_string()}")
          println("you surrendered!")

          game.player.balance - game.player.hand.bet / 2
        } else if (
          (playerHandValue > dealerHandValue && playerHandValue <= 21) || dealerHandValue > 21) {
          println(s"dealer cards: ${game.dealer.hand.to_string()}")
          println("you win!")

          game.player.balance + game.player.hand.bet
        } else if ((playerHandValue < dealerHandValue && dealerHandValue <= 21) || playerHandValue > 21) {
          println(s"dealer cards: ${game.dealer.hand.to_string()}")
          println("sorry, you lose!")

          game.player.balance - game.player.hand.bet
        } else {
          println(s"dealer cards: ${game.dealer.hand.to_string()}")
          println("push!")

          game.player.balance
        }

        println(s"balance: $newBalance")

        val newGame = Game(
          minBet = 5,
          maxBet = 1000,
          player = game.player.copy(
            hand = null,
            balance = newBalance
          ),
          dealer = game.dealer.copy(
            hand = null
          ),
          state = "checkShoe"
        )

        (new BlackJackGame).play(newGame)
    }
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
  def userAction(): String = {
    val action: String = scala.io.StdIn.readLine("your move:")

    if (UserActions.actions.contains(action))
      action
    else {
      println("invalid action, choose a valid action: " + UserActions.actions.mkString(", "))
      userAction()
    }
  }

  @scala.annotation.tailrec
  def dealHands(shoe: Shoe): (Seq[Seq[BjCard]], Shoe) = {
    val (hands: Option[Seq[Seq[BjCard]]], newShoe) = Shoe.dealMultipleHands(shoe, 2)

    hands match {
      case Some(handsList) => (handsList, newShoe)
      case None => dealHands(BjCard.createShoe(1))
    }
  }
}
