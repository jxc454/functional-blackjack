package com.blackjack

object BlackJackConsole {
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

    doNextTransition(game, new BlackJackGame)
  }

  @scala.annotation.tailrec
  private def doNextTransition(gameState: Game, game: BlackJackGame): Game = {
    doNextTransition(game.run(gameState), game)
  }
}
