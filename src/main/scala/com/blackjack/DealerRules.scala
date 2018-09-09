package com.blackjack


object DealerRules {
  def getAction(hand: BjHand): String = {
    actionMap.getOrElse((hand.handValue(), hand.lowValue()), "stay")
  }

  val actionMap: Map[(Int, Int), String] = Map(
    ((4, 4), "hit"),
    ((5, 5), "hit"),
    ((6, 6), "hit"),
    ((7, 7), "hit"),
    ((8, 8), "hit"),
    ((9, 9), "hit"),
    ((10, 10), "hit"),
    ((11, 11), "hit"),
    ((12, 12), "hit"),
    ((13, 13), "hit"),
    ((14, 14), "hit"),
    ((15, 15), "hit"),
    ((16, 16), "hit"),
    ((17, 17), "stay"),
    ((18, 18), "stay"),
    ((19, 19), "stay"),
    ((20, 20), "stay"),
    ((21, 21), "stay"),
    ((22, 12), "hit"),
    ((23, 13), "hit"),
    ((24, 14), "hit"),
    ((25, 15), "hit"),
    ((26, 16), "hit"),
    ((27, 17), "stay"),
    ((28, 18), "stay"),
    ((29, 19), "stay"),
    ((30, 20), "stay"),
    ((31, 21), "stay"),
    ((31, 21), "stay"),
    ((12, 2), "hit"),
    ((13, 3), "hit"),
    ((14, 4), "hit"),
    ((15, 5), "hit"),
    ((16, 6), "hit"),
    ((17, 7), "hit"),
    ((18, 8), "stay"),
    ((19, 9), "stay"),
    ((20, 10), "stay"),
    ((21, 11), "stay"))

  def blackjackPayout(bet: Double): Double = bet * 3 / 2

}