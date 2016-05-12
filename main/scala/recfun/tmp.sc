
object ws {

  import scala.collection._
  import scala.collection.immutable
  import common._
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money - coins.head , coins) + countChange (money , coins.tail)
  }
  countChange(4, List(1,2))
}
