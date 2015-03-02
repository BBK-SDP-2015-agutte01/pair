import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class BoardTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var board = Board()
//  val p1 = new Human(YELLOW)
//  val p2 = new Human(RED)

  override def beforeEach(): Unit = {
    board = Board()
  }
  override def afterEach(): Unit = {

  }

  "A Board" should "return the row number of the top row when makeMove(column) is called, " +
    "in this case column 0 is empty so the result should be zero" in {

    val move = new Move(YELLOW, 1)
    board.makeMove(move)
    board.getTile(5, 1) should be (YELLOW)

  }

  "A Board" should "return an array of Moves of length 7 when getPossibleMoves is called " +
    "on an empty board" in {

    println(board.getPossibleMoves(YELLOW).length)
    var arr = Array[Move]()
    for (i <- 0 until 5) arr = arr :+ new Move(YELLOW, i)
    board.getPossibleMoves(YELLOW) should be (arr)

  }

  "A Board" should "return an array of Moves of length 6 when getPossibleMoves is called " +
    "and one column is full" in {

    for (i <- 0 until 5) board.makeMove(new Move(YELLOW, 0))
    board.getPossibleMoves(YELLOW).length should be (6)

  }

  "A Board" should "return an array of Moves of length 0 when getPossibleMoves is called " +
    "and all spaces are full" in {

    for (c <- 0 until 5) {
      for (i <- 0 until 5) board.makeMove(new Move(YELLOW, c))
    }

    val arr = Array[Move]()
    board.getPossibleMoves(YELLOW) should be (arr)

  }




}
