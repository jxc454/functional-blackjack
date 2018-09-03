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
      case Some(config) => {
        val message: String = config.message
        println (message)
      }
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
        } else play(game.copy(state = "getBets"))


      case "getBets" =>
        val bet = BlackJackGame.userBet()

        // check if bet is in range
        if (bet <= game.minBet || bet >= game.maxBet) {
          println("that bet is out of range, try again.")
          play(game)
        } else {
          play(game.copy(player=game.player.copy(bet=Some(bet)), state="dealHands"))
        }

      case "dealHands" =>
        val (hands: Seq[BjHand], newShoe: Shoe) = BlackJackGame.dealHands(game.dealer.shoe)

        println(s"dealer card: ${hands.head.cards.head.to_string}")

        println(s"your cards: ${hands(1).cards.map(_.to_string).mkString("")} | value: ${hands(1).handValue()}")

        println(s"cards remaining: ${newShoe.size}")

        play(
          game.copy(
            dealer=game.dealer.copy(
              shoe=newShoe,
              hand=Some(hands.head)),
            player=game.player.copy(
              hand=Some(hands(1))),
            state="action"
          )
        )

      case "action" =>

        val action: String = scala.io.StdIn.readLine("your move:")

        action match {
          case "h" =>
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val currentCards: Seq[BjCard] = game.player.hand match {
              case Some(hand) => hand.cards
              case _ => sys.exit
            }

            val newHand: BjHand = newCard match {
              case Some(card) => new BjHand(card ++ currentCards)
              case _ => sys.exit
            }
            println(s"dealer card: ${game.dealer.hand match { case Some(h) => h.cards.head.to_string}}")

            println(s"your cards: ${newHand.cards.map(_.to_string).mkString("")} | value: ${newHand.handValue()}")

            play(
              game.copy(
                dealer=game.dealer.copy(
                  shoe=newShoe),
                player=game.player.copy(
                hand=Some(newHand)),
                state= if (newHand.handValue() > 21) "settleUp" else "action"
              )
            )
          case "s" =>
            play(
              game.copy(
                state="dealerAction"
              )
            )
        }

      case "dealerAction" =>
        game.dealer.rules.getAction(game.dealer.hand match { case Some(h) => h}) match {
          case "hit" =>
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val currentCards: Seq[BjCard] = game.dealer.hand match {
              case Some(hand) => hand.cards
              case _ => sys.exit
            }

            val newHand: BjHand = newCard match {
              case Some(card) => new BjHand(card ++ currentCards)
              case _ => sys.exit
            }

            play(
              game.copy(
                dealer=game.dealer.copy(
                  shoe=newShoe,
                  hand=Some(newHand)
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
        val playerHandValue: Int = game.player.hand match {case Some(h) => h.handValue()}
        val dealerHandValue: Int = game.dealer.hand match {case Some(h) => h.handValue()}

        val newBalance: Double = if (
          (playerHandValue > dealerHandValue && playerHandValue <= 21) || dealerHandValue > 21) {
          println(s"dealer cards: ${game.dealer.hand match { case Some(h) => h.to_string()}}")
          println("you win!")

          game.player.balance + game.player.bet.getOrElse(0.0)
        } else if ((playerHandValue < dealerHandValue && dealerHandValue <= 21) || playerHandValue > 21) {
          println(s"dealer cards: ${game.dealer.hand match { case Some(h) => h.to_string()}}")
          println("sorry, you lose!")

          game.player.balance - game.player.bet.getOrElse(0.0)
        } else {
          println(s"dealer cards: ${game.dealer.hand match { case Some(h) => h.to_string()}}")
          println("push!")

          game.player.balance
        }

        println(s"your balance is $newBalance")

        val newGame = Game(
          minBet = 5,
          maxBet = 1000,
          player = game.player.copy(
            hand = None,
            balance = newBalance,
            bet = None
          ),
          dealer = game.dealer.copy(
            hand = None
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
      case e: Exception => {
        println("couldn't accept that bet, try a different bet.")
        userBet()
      }
    }
  }

  @scala.annotation.tailrec
  def dealHands(shoe: Shoe): (Seq[BjHand], Shoe) = {
    val (hands: Option[Seq[BjHand]], newShoe) = Shoe.dealMultipleHands(shoe, 2)

    hands match {
      case Some(handsList) => (handsList, newShoe)
      case None => dealHands(BjCard.createShoe(1))
    }
  }
}
