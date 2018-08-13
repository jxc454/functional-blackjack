package com.blackjack


object DealerRules {
  def getAction(hand: BjHand): String = {
    actionMap(hand.handValue())
  }

  val actionMap: Map[Int, String] = Map(
    (4, "hit"),
    (5, "hit"),
    (6, "hit"),
    (7, "hit"),
    (8, "hit"),
    (9, "hit"),
    (10, "hit"),
    (11, "hit"),
    (12, "hit"),
    (13, "hit"),
    (14, "hit"),
    (15, "hit"),
    (16, "hit"),
    (17, "stay"),
    (18, "stay"),
    (19, "stay"),
    (20, "stay"),
    (21, "stay"))

  def blackjackPayout(bet: Double): Double = bet * 3 / 2

}