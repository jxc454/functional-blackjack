package com.blackjack

case class Player(
  hands: Seq[BjHand] = null,
  balance: Double = 0.0)
