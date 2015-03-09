import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class StateTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var board = Board()

  override def beforeEach(): Unit = {
    board = Board()
  }
  override def afterEach(): Unit = {

  }

  "A State" should "assign an array of 7 States to var children " +
    "when initializeChildren is called. " in {

    val m = new Move(RED, 1)
    board.makeMove(m)

    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (7)
  }

  "A State" should "assign an array of 6 States to var children when " +
    "initializeChildren is called and the board has one full column. " in {

    board.fillColumn(0)

    val m = new Move(RED, 2)
    board.makeMove(m)

    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (6)
  }

  "A State" should "not be able to add States to var children when  " +
    "initializeChildren is called and the board is full. " in {

    for (c <- 0 until 6) {
      board.fillColumn(c)
    }

    val m = new Move(RED, 2)

    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (6)
  }

}
