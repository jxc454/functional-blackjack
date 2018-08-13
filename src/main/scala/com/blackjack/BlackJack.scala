package com.blackjack

import scopt.OptionParser
import scala.io.StdIn

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
            state="getBets"
          )
        )

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
