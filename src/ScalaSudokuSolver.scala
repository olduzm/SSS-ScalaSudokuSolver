
object ScalaSudokuSolver {
  val digits = "123456789"
  val rows = "ABCDEFGHI"
  val cols = digits
  val box_r = List("ABC", "DEF", "GHI")
  val box_c = List("123", "456", "789")
  val squares = cross(rows, digits)
  val unitlist = cols.toList.map(c => cross(rows, c+"")) :::
                 rows.toList.map(r => cross(digits, r+""))



  def cross(x:String, y:String):List[String] = stringify(cross(x.toList, y.toList))
  def cross[A,B](x:List[A], y:List[B]):List[(A,B)] = for (a <- x; b <- y) yield (a,b)

  def main(args: Array[String]) {
    test()
  }

  /*** Some unit tests ***/
  def test() {
    assert( squares.size == 81 )
//    assert( unitlist.size == 27 )
  }

  /*** Other helper methods ***/
  def stringify(ls:List[(Char, Char)]) : List[String] = ls.map( t => t._1 + "" + t._2 )
}