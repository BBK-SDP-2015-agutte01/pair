import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class BoardTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var board = Board()

  override def beforeEach(): Unit = {
    board = Board()
  }

  "A makeMove method " should "place a Player disc when called." in {
    val move = new Move(YELLOW, 1)
    board.makeMove(move)
    board.getTile(5, 1) should be (YELLOW)
  }

  it should "throw an IllegalArgumentException if the column of the Move is full." in {
    board.fillColumn(0)
    intercept[IllegalArgumentException] {
      board.makeMove(new Move(YELLOW, 0))
    }
  }

  "A getPossibleMoves method " should "return an array of Moves of length 7 when" +
    "the board is empty" in {
    board.getPossibleMoves(YELLOW) should have length 7
  }

  it should "return an array of Moves of length 6 when the board has one full column" in {
    board.fillColumn(0)
    board.getPossibleMoves(YELLOW) should have length 6
  }

  it should "return an array of Moves of length 0 when the board is full." in {
    for (c <- 0 until 6) board.fillColumn(c)
    board.getPossibleMoves(YELLOW) should have length 0
  }

}
