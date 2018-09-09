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
        println(s"true count: ${"%.2f".format(game.dealer.shoe.trueCount).toDouble}")

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

          val nextState: String = if (dealerHand.handValue() == 21) {
            println(s"your cards: ${playerHand.to_string()}")
            "settleUp"
          } else "action"

          play(
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

      case "action" =>
        play(
          game.player.hands.foldLeft(game.copy(player=game.player.copy(hands=Seq())))((acc, hand) =>
          BlackJackGame.action(acc, hand))
            .copy(state="dealerAction")
        )

      case "dealerAction" =>
        if (game.player.hands.isEmpty) {
          println(s"dealer cards: ${game.dealer.hand.to_string()}")

          play(
            game.copy(state="checkShoe")
          )
        } else {
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
                    hand=newHand),
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
        }

      case "settleUp" =>
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
          state = "checkShoe"
        )

        (new BlackJackGame).play(newGame)
    }
  }
}

object BlackJackGame {

  def action(game: Game, hand: BjHand): Game = {
    println(s"your cards: ${hand.to_string()} | value: " +
      s"${if (hand.soft) hand.lowValue + "/" + hand.handValue() else hand.finalValue()}" +
      s"${if (hand.cards.size == 2 && hand.cards.head.pipName == hand.cards(1).pipName) " (splittable)" else ""}"
    )

    if (hand.handValue == 21 && hand.cards.size == 2) {
      println("you got BlackJack!")
      println(s"balance: ${game.player.balance + 1.5 * hand.bet}")

      game.copy(
        player=game.player.copy(
          balance=game.player.balance + 1.5 * hand.bet
        )
      )
    }
    else {
      BlackJackGame.userAction() match {
        case "h" =>
          val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

          val newHand: BjHand = newCard.map(card => card ++ hand.cards) match {
            case Some(cards) => hand.copy(cards=cards)
            case _ =>
              println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
              sys.exit()
          }

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

        case "s" =>
          game.copy(
            player=game.player.copy(hands=hand +: game.player.hands)
          )

        case "d" =>
          if (!BlackJackGame.checkCanDD(hand)){
            println("too late to double down!")
            action(game, hand)
          } else {
            val (newCard: Option[Seq[BjCard]], newShoe: Shoe) = game.dealer.shoe.deal(1).run(game.dealer.shoe)

            val newHand: BjHand = newCard.map(card => card ++ hand.cards) match {
              case Some(cards) => BjHand(cards = cards, bet = hand.bet * 2)
              case _ =>
                println("deck is out of cards!!!  Maybe need to reset the deck sooner...")
                sys.exit()
            }

            println(s"your cards: ${newHand.cards.map(_.to_string()).mkString("")} | value: ${newHand.finalValue()}")

            if (newHand.finalValue > 21) {
              // hand busted
              println("you busted!")
              println(s"balance: ${game.player.balance - hand.bet}")

              game.copy(
                dealer = game.dealer.copy(shoe = newShoe),
                player = game.player.copy(balance = game.player.balance - newHand.bet))
            } else {
              // no bust
              game.copy(
                dealer = game.dealer.copy(shoe = newShoe),
                player = game.player.copy(hands = newHand +: game.player.hands))
            }
          }

        case "r" =>
          println("you surrendered!")
          println(s"balance: ${game.player.balance - hand.bet / 2}")

          game.copy(
            player=game.player.copy(balance=game.player.balance - hand.bet / 2)
          )

        case "p" =>
          if (!checkSplittable(hand)) {
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
    }

  }


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
    // prompt user to select an action
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

  def checkSplittable(hand: BjHand): Boolean = {
    val (card1, card2) = hand.cards.splitAt(1)
    card1.head.pip == card2.head.pip
  }

  def checkCanDD(hand: BjHand): Boolean = hand.cards.length == 2

}
