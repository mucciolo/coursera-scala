package recfun

trait RecFunInterface {
  def pascal(c: Int, r: Int): Int
  def balance(chars: List[Char]): Boolean
  def countChange(money: Int, coins: List[Int]): Int
}
