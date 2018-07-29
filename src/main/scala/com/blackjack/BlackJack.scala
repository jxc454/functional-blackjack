package com.blackjack

object BlackJack {
  def main(args: Array[String]): Unit = {
    println("dealing...")

    val shoe = BjCard.createShoe(1)

    def getHands(shoe: Shoe): (Shoe, Option[List[Card]], Option[List[Card]]) = {
      val xy: (Option[List[Card]], Shoe) = shoe.deal(2)

      val hand1 = xy._1
      val res = xy._2.deal(2)

      (res._2, hand1, res._1)
    }

    val situation = getHands(shoe)

    val g = 0

    situation match {
      case (_, None, _) => println("no good")
      case (_, _, None) => println("no good")
      case (s, h1, h2) => {
        h1.foreach(listOfCards =>
          listOfCards.foreach(singleCard =>
            println(singleCard.to_string)
          )
        )
      }
    }

  }
}

