import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class BoardTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var board = Board()

  override def beforeEach(): Unit = {
    board = Board()
  }

  "A Board" should "place a Player disc when makeMove(column) is called." in {
    val move = new Move(YELLOW, 1)
    board.makeMove(move)
    board.getTile(5, 1) should be (YELLOW)
  }

  "A getPossibleMoves method" should "return an array of Moves of length 7 when" +
    "the board is empty" in {
    board.getPossibleMoves(YELLOW).length should be (7)
  }

  it should "return an array of Moves of length 6 when the board has one full column" in {
    board.fillColumn(0)
    board.getPossibleMoves(YELLOW).length should be (6)
  }

  it should "return an array of Moves of length 0 when the board is full." in {
    for (c <- 0 until 6) board.fillColumn(c)
    board.getPossibleMoves(YELLOW).length should be (0)
  }

}
