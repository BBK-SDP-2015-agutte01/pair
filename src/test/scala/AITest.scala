import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

/** Test class for AI
  *
  */
class AITest extends FlatSpec with MockFactory with Matchers with BeforeAndAfterEach {
  //Fixtures used in more than one test
  var count = 0
  var depth = 0
  var testChildren = Array[State]()
  var testBoard: Board = _
  var testRoot: State = _
  val redMove0 = new Move(RED, 0)
  val redMove5 = new Move(RED, 5)
  val redMove6 = new Move(RED, 6)
  val yellowMove1 = new Move(YELLOW, 1)
  val yellowMove2 = new Move(YELLOW, 2)
  val yellowMove3 = new Move(YELLOW, 3)

  //Creates a standardised board and state before each test
  override def beforeEach() {
    testBoard = Board()
    testRoot = new State(YELLOW, testBoard, redMove6)
    testBoard.makeMove(yellowMove1)
    testBoard.makeMove(redMove0)
    testBoard.makeMove(yellowMove2)
    testBoard.makeMove(redMove5)
    testBoard.makeMove(yellowMove3)
    testBoard.makeMove(redMove6)
  }


  "A createGameTree method" should "generate a game tree of the given depth(0) by utilising the variable " +
    "children: Array[State] in each state object." in {
    depth = 0
    count = 0

    AI.createGameTree(testRoot, depth)
    testChildren = testRoot.children

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

    AI.createGameTree(testRoot, depth)
    testChildren = testRoot.children

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

    AI.createGameTree(testRoot, depth)
    testChildren = testRoot.children

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

    AI.createGameTree(testRoot, depth)
    testChildren = testRoot.children

    while (testChildren.length > 0) {
      count += 1
      testChildren = testChildren(0).children
    }
    count should equal(depth)

  }

  it should "generate the expected state nodes at each level" in {

    val board1 = Board(testBoard)
    val board2 = Board(testBoard)
    val board3 = Board(testBoard)

    board1.makeMove(new Move(YELLOW, 0))
    val oneLevelChild0 = new State(RED, board1, null)

    board2.makeMove(new Move(YELLOW, 3))
    board2.makeMove(new Move(RED, 3))
    val twoLevelsChild3Child3 = new State(YELLOW, board2, null)

    board3.makeMove(new Move(YELLOW, 6))
    board3.makeMove(new Move(RED, 3))
    board3.makeMove(new Move(YELLOW, 5))
    val threeLevel3Child6Child3Child5 = new State(RED, board3, null)

    AI.createGameTree(testRoot, 1)
    oneLevelChild0.toString() should be(testRoot.children(0).toString())
    AI.createGameTree(testRoot, 2)
    twoLevelsChild3Child3.toString() should be(testRoot.children(3).children(3).toString())
    AI.createGameTree(testRoot, 3)
    threeLevel3Child6Child3Child5.toString() should be(testRoot.children(6).children(3).children(5).toString())
  }

  "A minimax method" should "assign the correct values to each of the leaves." in {
    AI.createGameTree(testRoot, 3)
    val aiObject = new AI(testRoot.player, 3)
    aiObject.minimax(testRoot)
    var leaves = testRoot.children(0).children(0).children

    leaves(0).value should be (aiObject.evaluateBoard(leaves(0).board))
    leaves(1).value should be (aiObject.evaluateBoard(leaves(1).board))
    leaves(2).value should be (aiObject.evaluateBoard(leaves(2).board))
    leaves(3).value should be (aiObject.evaluateBoard(leaves(3).board))
    leaves(4).value should be (aiObject.evaluateBoard(leaves(4).board))
    leaves(5).value should be (aiObject.evaluateBoard(leaves(5).board))
    leaves(6).value should be (aiObject.evaluateBoard(leaves(6).board))

    leaves = testRoot.children(4).children(3).children

    leaves(0).value should be (aiObject.evaluateBoard(leaves(0).board))
    leaves(1).value should be (aiObject.evaluateBoard(leaves(1).board))
    leaves(2).value should be (aiObject.evaluateBoard(leaves(2).board))
    leaves(3).value should be (aiObject.evaluateBoard(leaves(3).board))
    leaves(4).value should be (aiObject.evaluateBoard(leaves(4).board))
    leaves(5).value should be (aiObject.evaluateBoard(leaves(5).board))
    leaves(6).value should be (aiObject.evaluateBoard(leaves(6).board))



  }

  it should "then assign the correct values to each of the parent nodes based on the values of their children." in {
    AI.createGameTree(testRoot, 3)
    val aiObject = new AI(testRoot.player, 3)
    aiObject.minimax(testRoot)
    val leaves = testRoot.children(0).children(0).children

    //Code below tests if the parents are being assigned the correct (either highest/lowest values) based on the values of their children
    var max = leaves(0).value
    var min = leaves(0).value

    for (c <- leaves) {
      if (c.value > max) max = c.value
      if (c.value < min) min = c.value
    }
    testRoot.children(0).children(0).value should be (max)

    // now testing if value of parent of leaves is equal to min of leaf values
    val nodes = testRoot.children(0).children

    max = nodes(0).value
    min = nodes(0).value

    for (n <- nodes) {
      if (n.value > max) max = n.value
      if (n.value < min) min = n.value
    }

    testRoot.children(0).value should be (min)
  }

  "A getBestMoves method" should "with a depth of 1, return an array of length 1 with " +
    "a Move of column 0 if the player has 3 discs in a row in column 0." in {
    val aiObject = new AI(RED, 1)

    testBoard.makeMove(redMove0)
    testBoard.makeMove(redMove0)

    aiObject.getBestMoves(testBoard)(0).column should equal (0)
  }

  it should "with a depth of 2, return an array of length 1 with a Move of column 0 if " +
    "the player has 3 discs in a row in column 0." in {
    val aiObject = new AI(RED, 2)

    testBoard.makeMove(redMove0)
    testBoard.makeMove(redMove0)

    aiObject.getBestMoves(testBoard)(0).column should equal (0)
  }

  it should "with a depth of 2, return an array of length 1 with a Move of column 0 if " +
    "the opponent has 3 discs in a row in column 0." in {
    val aiObject = new AI(YELLOW, 2)

    testBoard.makeMove(redMove0)
    testBoard.makeMove(redMove0)

    aiObject.getBestMoves(testBoard)(0).column should equal (0)
  }

  it should "with a depth of 3, return an array of length 1 with a Move of column 0 if " +
    "the opponent has 3 discs in a row in column 0." in {
    val aiObject = new AI(YELLOW, 3)

    testBoard.makeMove(redMove0)
    testBoard.makeMove(redMove0)

    aiObject.getBestMoves(testBoard)(0).column should equal (0)
  }

  it should "with a depth of 1, return an array of length 1 with a Move of column 4 if the player has " +
    "3 discs in a row in columns 1 to 3." in {
    val aiObject = new AI(YELLOW, 1)
    aiObject.getBestMoves(testBoard)(0).column should equal (4)
  }

  it should "with a depth of 2, return an array of length 1 with a Move of column 4 if the player has " +
    "3 discs in a row in columns 1 to 3." in {
    val aiObject = new AI(YELLOW, 2)
    aiObject.getBestMoves(testBoard)(0).column should equal (4)
  }

  it should "with a depth of 2, return an array of length 1 with a Move of column 4 if the opponent has " +
    "3 discs in a row in columns 1 to 3." in {
    val aiObject = new AI(RED, 2)
    aiObject.getBestMoves(testBoard)(0).column should equal (4)
  }

  it should "with a depth of 3, return an array of length 1 with a Move of column 4 if the opponent has " +
    "3 discs in a row in columns 1 to 3." in {
    val aiObject = new AI(RED, 3)
    aiObject.getBestMoves(testBoard)(0).column should equal (4)
  }

}
