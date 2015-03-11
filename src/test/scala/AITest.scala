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
  val testRoot = new State(YELLOW, testBoard, yellowMove3)



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

    AI.createGameTree(testRoot, 1)
    oneLevelChild0.toString() should be(testRoot.children(0).toString())
    AI.createGameTree(testRoot, 2)
    twoLevelsChild3Child3.toString() should be(testRoot.children(3).children(3).toString())
    AI.createGameTree(testRoot, 3)
    threeLevel3Child6Child3Child5.toString() should be(testRoot.children(6).children(3).children(5).toString())
  }

  "A minimax method" should "assign the correct values to each state from the leaves upwards." in {
    AI.createGameTree(testRoot, 3)
    val aiObject = new AI(testRoot.player, 3)
    aiObject.minimax(testRoot)
    val leaves = testRoot.children(0).children(0).children
    var max = leaves(0).value
    var min = leaves(0).value
    for (c <- leaves) {
      c.value should be (aiObject.evaluateBoard(c.board))
      if (c.value > max) max = c.value
      if (c.value < min) min = c.value
    }
//    println(testRoot)
//    testRoot.children(0).children(0).value should be (max)

  }

  "A getMoves method" should "return an array of length 1 with a Move of column 4 as its best move." in {
    val aiObject = new AI(testRoot.player, 3)

    board3.makeMove(redMove1)
    board3.makeMove(redMove1)

    aiObject.getMoves(board3)(0).column should equal (redMove1.column)
  }

//  inAnyOrder {
//    val aiMock = mock[AI]
//    val boardStub = stub[Board()]
//
//    (aiMock.getMoves(boardStub) _).expects(AI.createGameTree(), *, *)
//    (httpClient.sendRequest _).expects(Method.POST, "http://scalamock.org", *).noMoreThanOnce
//    (httpClient.sendRequest _).expects(Method.POST, "http://example.com", *).returning(Http.NotFound)
//    inSequence {
//      (counterMock.increment _) expects(*) onCall { arg: Int => arg + 1}
//      (counterMock.decrement _) expects(*) onCall { throw new RuntimeException() }
//    }
//  }


}
