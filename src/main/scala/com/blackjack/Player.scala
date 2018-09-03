package com.blackjack

case class Player(
  bet: Option[Double] = None,
  hand: Option[BjHand] = None,
  balance: Double = 0.0)
