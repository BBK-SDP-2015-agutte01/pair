import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class StateTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var board = Board()
  val m = new Move(RED, 1)

  override def beforeEach(): Unit = {
    board = Board()
  }

  "A State" should "assign an array of 7 States to var children when initializeChildren is called." in {
    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (7)
  }

  it should "assign an array of 6 States to var children when initializeChildren is called and " +
    "the board has one full column." in {
    board.fillColumn(0)
    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (6)
  }

  it should "not be able to add States to var children when initializeChildren is called and " +
    "the board is full." in {
    for (c <- 0 until 6) {
      board.fillColumn(c)
    }
    val s = new State(YELLOW, board, m)
    s.initializeChildren()
    s.getChildren.length should be (0)
  }

}
