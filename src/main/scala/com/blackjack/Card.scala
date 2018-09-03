package com.blackjack

sealed trait Card {
  val pip: Int
  val pipName: String
  val group: String
  def to_string: String
}

object BjCard {
  private val validPipNames = Seq("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K")
  private val validGroups = Seq("h", "s", "d", "c")

  def createShoe(decks: Int): Shoe = {
    new Shoe(cardsIn = validPipNames.foldLeft(Nil:List[BjCard])((l: List[BjCard], p: String) => {
      l ++
        validGroups.foldLeft(Nil:List[BjCard])((l2: List[BjCard], g: String) =>
          List.fill(decks)(new BjCard(p, g)) ++ l2)
      })
    )(shuffle=true)
  }
}

class BjCard(val pipName: String, val group: String) extends Card {

  if (!BjCard.validGroups.contains(group)) {
    throw new IllegalArgumentException
  }
  if (!BjCard.validPipNames.contains(pipName)) {
    throw new IllegalArgumentException
  }

  private def getPipValue(pipValue: String): Int = pipValue match {
    case "T" | "J" | "Q" | "K" => 10
    case x => x.toInt
  }

  def to_string: String = this.pipName.concat(this.group)

  val pip: Int = getPipValue(pipName)
  val suit: String = group
}


trait Hand[Card] {
  def to_string(): String
}

class BjHand(val cards: Seq[BjCard], val bet: Double) extends Hand[BjCard] {
  def to_string(): String = cards.foldLeft("")((l, c) => l.concat(c.to_string))

  def handValue(): Int = this.cards.foldLeft(0)(_ + _.pip)
}
