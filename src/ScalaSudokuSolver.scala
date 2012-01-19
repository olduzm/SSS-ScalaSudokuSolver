
object ScalaSudokuSolver {
  val digits = "123456789"
  val rows = "ABCDEFGHI"
  val cols = digits
  val box_r = List("ABC", "DEF", "GHI")
  val box_c = List("123", "456", "789")
  val squares = stringify(cross, rows, cols).flatten
  val unitlist = stringify(cross, rows, cols) :::
                 stringify(crossR, cols, rows)


  def cross[A](xs:List[A], ys:List[A]):List[List[(A, A)]] = ys.map(y => xs.map( x => (x, y) ))
  def crossR[A](xs:List[A], ys:List[A]):List[List[(A, A)]] = ys.map(y => xs.map( x => (y, x) ))

  def main(args: Array[String]) {
    test()
  }

  /*** Some unit tests ***/
  def test() {
    assert( squares.size == 81 )
    println(unitlist)
    //    assert( unitlist.size == 27 )
  }

  /*** Other helper methods ***/
  def stringify(fn: (List[Char], List[Char]) => List[List[(Char, Char)]], x:String, y:String) : List[List[String]] =
    fn(x.toList, y.toList).map( t => t.map( c=> c._1 + "" + c._2 ) )
}