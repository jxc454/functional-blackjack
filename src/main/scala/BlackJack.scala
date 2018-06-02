import card._

object BlackJack {
  def main(args: Array[String]): Unit = {
    println("dealing...")

    val deck = BjCard.createShoe(1)

    deck.foreach(x => println(x.to_string))
  }
}