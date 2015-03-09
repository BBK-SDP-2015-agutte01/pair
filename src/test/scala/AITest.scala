import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/** Test class for AI
  *
  */
class AITest extends FlatSpec with MockFactory with Matchers {

  val board = Board()

  val redMove1 = new Move(RED, 0)
  val redMove2 = new Move(RED, 5)
  val redMove3 = new Move(RED, 6)
  val yellowMove1 = new Move(YELLOW, 1)
  val yellowMove2 = new Move(YELLOW, 2)
  val yellowMove3 = new Move(YELLOW, 3)
  var count = 0
  var depth = 0
  var testChildren = Array[State]()

  board.makeMove(redMove1)
  board.makeMove(yellowMove1)
  board.makeMove(redMove2)
  board.makeMove(yellowMove2)
  board.makeMove(redMove3)
  board.makeMove(yellowMove3)

  val root = new State(YELLOW, board, yellowMove3)
  val board1 = Board(board)
  val board2 = Board(board)
  val board3 = Board(board)
  val testBoard = Board(board)
  val testRoot = new State(RED, testBoard, yellowMove3)



  board1.makeMove(new Move(RED, 0))
  val oneLevelChild0 = new State(RED, board1, new Move(RED, 0))

  board2.makeMove(new Move(RED, 3))
  board2.makeMove(new Move(YELLOW, 3))
  val twoLevelsChild3Child3 = new State(YELLOW, board2, yellowMove3)

  board3.makeMove(new Move(RED, 6))
  board3.makeMove(new Move(YELLOW, 3))
  board3.makeMove(new Move(RED, 5))
  val threeLevel3Child6Child3Child5 = new State(RED, board3, redMove2)

  "A createGameTree method" should "generate a game tree of the given depth(0) by utilising the variable " +
    "children: Array[State] in each state object." in {
    depth = 0
    count = 0

    AI.createGameTree(root, depth)
    testChildren = root.children

    while (testChildren.length > 0) {
      count += 1
      testChildren = testChildren(0).children
    }
    count should equal(depth)

  }

  it should "generate a game tree of the given depth(3) by utilising the variable " +
    "children: Array[State] in each state object." in {
    depth = 3
    count = 0

    AI.createGameTree(root, depth)
    testChildren = root.children

    while (testChildren.length > 0) {
      count += 1
      testChildren = testChildren(0).children
    }
    count should equal(depth)

  }

  it should "generate a game tree of the given depth(4) by utilising the variable " +
    "children: Array[State] in each state object." in {
    depth = 4
    count = 0

    AI.createGameTree(root, depth)
    testChildren = root.children

    while (testChildren.length > 0) {
      count += 1
      testChildren = testChildren(0).children
    }
    count should equal(depth)

  }

  it should "generate a game tree of the given depth(5) by utilising the variable " +
    "children: Array[State] in each state object." in {
    depth = 5
    count = 0

    AI.createGameTree(root, depth)
    testChildren = root.children

    while (testChildren.length > 0) {
      count += 1
      testChildren = testChildren(0).children
    }
    count should equal(depth)

  }

  it should "generate the expected state nodes at each level" in {

    AI.createGameTree(testRoot, 3)
    oneLevelChild0 should be(testRoot.children(0))
//    twoLevelsChild3Child3 should be(testRoot.children(3).children(3))
//    threeLevel3Child6Child3Child5 should be(testRoot.children(6).children(3).children(5))
  }

}
