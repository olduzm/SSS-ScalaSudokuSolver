
object ScalaSudokuSolver {
  val digits = "123456789"
  val rows = "ABCDEFGHI"
  val cols = digits
  val box_r = List("ABC", "DEF", "GHI")
  val box_c = List("123", "456", "789")
  val squares = cross(rows, digits)
  val unitlist = cols.toList.map(c => cross(rows.toList, List(c))) :::
    rows.toList.map(r => cross(List(r), digits.toList)) :::
    box_r.flatMap(rs => box_c.map(cs => cross(rs, cs)))
  val units = squares.map(sq => (sq, unitlist.filter(_.contains(sq)))).toMap
  val peers = squares.map(sq => (sq, units.apply(sq).flatten.distinct.filterNot(_ == sq))).toMap

  def cross(x:String, y:String):List[String] = cross(x.toList, y.toList)
  def cross[A,B](x:List[A], y:List[B]):List[String] = for (a <- x; b <- y) yield (stringify(a, b))

  def parseGrid(grid:String) = {
    squares.map((_, digits)).toMap ++ gridValues(grid).filterNot(x => "0.".contains(x._2))
  }

  def gridValues(grid:String) : Map[String, String] = {
    val chars = grid.toSeq.map(_ + "")
    assert( chars.size == 81, {println("The puzzle given should have exactly 81 squares")} )
    squares.zip(chars).toMap
  }

  def display(grid:Map[String, String]) {
    val width = grid.values.map(_.length).max + 1
    def fmt(n:String) = ("%-" + width + "s").format(n)
    val paddedValues = grid.values.map(fmt(_))
    val hyphens = List.fill(width * 3)('-').mkString
    val line = hyphens + "+" + hyphens + "+" + hyphens
    println(
      paddedValues.grouped(9)
                  .map(_.grouped(3).map(_.mkString).mkString("|"))
                  .grouped(3).map(_:+line).flatten.mkString("\n")
    )
  }

  def main(args: Array[String]) {
    test()
    val grid1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
    display(parseGrid(grid1))
  }

  /*** Some unit tests ***/
  def test() {
    assert( squares.size == 81 )
    assert( unitlist.size == 27 )
    assert( squares.forall( units.apply(_).size == 3 ))
    assert( squares.forall( peers.apply(_).size == 20 ))
    assert( units.apply("C2") == List(
      List("A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2"),
      List("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"),
      List("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3")))
    assert( peers.apply("C2") == List("A2", "B2", "D2", "E2", "F2", "G2", "H2", "I2",
      "C1", "C3", "C4", "C5", "C6", "C7", "C8", "C9",
      "A1", "A3", "B1", "B3"))
    println ("All tests pass.")

  }

  /*** Other helper methods ***/
  def stringify(a:Any, b:Any) : String = (a + "" + b)
}