import card._

object BlackJack {
  def main(args: Array[String]): Unit = {
    println("dealing...")

    val shoe = BjCard.createShoe(5).shuffle()

    val hand = shoe.deal(2)

    // deal returns an option!
    hand.foreach(v =>
      v.foreach(x => println(x.to_string()))
    )
  }
}