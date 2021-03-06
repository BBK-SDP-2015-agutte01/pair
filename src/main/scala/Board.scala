import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * An instance represents a grid of pieces from two opposing
 * players in a game of Connect Four. The grid is 0-indexed first by rows
 * starting at the top, then by columns 0-indexed starting at the left.
 */
class Board {

  private val FOUR = 4

  /**
   * vertical, horizontal, uphill, downhill, directions from any position
   */
  private val deltas = Array(Array(1, 0), Array(0, 1), Array(-1, 1), Array(1, 1))

  /**
   * The grid of Player pieces.
   */
  private val board = Array.ofDim[Player](Board.NUM_ROWS, Board.NUM_COLS)

  /**
   * Constructor: a duplicate of Board b.
   */
  def this(b: Board) {
    this()
    for (r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS)
      board(r)(c) = b.board(r)(c)
  }

  /**
   * Return the element in row r col c.
   * Precondition: r and c give a position on the board
   */
  def getPlayer(r: Int, c: Int): Player = {
    assert(0 <= r && r < Board.NUM_ROWS && 0 <= c && c < Board.NUM_COLS)
    board(r)(c)
  }

  /**
   * Constructor: a Board constructed by duplicating b and
   * applying nextMove to the new Board.
   */
  def this(b: Board, nextMove: Move) {
    this(b)
    makeMove(nextMove)
  }

  /**
   * Applies the given move to this Board by placing a piece from the
   * player defined by the given move object into the column defines by the given move object.
   * @throws IllegalArgumentException if move's column is full on this Board.
   *
   * @param move the move you wish to make on this board.
   */
  def makeMove(move: Move): Unit = {
    if (getTop(move.column) != -1)
      board(getTop(move.column))(move.column) = move.player
    else
      throw new IllegalArgumentException

  }

  /**
   * Calculates the top position for the given column, returning -1 if the column is full.
   * @param column the column for which you wish to find the top position
   * @param row this should be left blank when calling the method as it is used for the tail recursive calls only.
   * @return the top position for the given column, or returns -1 if the column is full.
   */
  @tailrec
  private def getTop(column: Int, row: Int = Board.NUM_ROWS - 1): Int = {
    if (row < 0)  -1
    else if (board(row)(column) == null) row
    else getTop(column, row - 1)
  }

  /**
   * Return the Player at board position (row, col). Rows are
   * 0-indexed starting at the top and columns are 0-indexed starting
   * at the left. A null return value indicates an empty tile.
   */
  def getTile(row: Int, col: Int): Player = board(row)(col)


  /**
   * Calculates all moves that can be made on this board by the given player.
   * The moves are in order of increasing column number.
   *
   * Note: The length of the array is the number of possible moves.
   * Note: If the board has a winner (four things of the same colour in a row), no
   * move is possible because the game is over.
   * Note: If the game is not over, the number of possible moves is the number
   * of columns that are not full. Thus, if all columns are full, this method returns an
   * array of length 0.
   *
   * @param p the player for which you want to find the possible moves.
   * @return an array of all moves that can possibly be made by Player p on this
   * board.
   */
  def getPossibleMoves(p: Player): Array[Move] = {
    val output = new ArrayBuffer[Move]
    for (i <- 0 until Board.NUM_COLS) {
      val top = getTop(i)
      if (top != -1) output += new Move(p, i)
    }
    output.toArray
  }

  /**
   * Return a representation of this board
   */
  override def toString(): String = toString("")

  /**
   * Return the String representation of this Board with prefix
   * prepended to each line. Typically, prefix contains space characters.
   */
  def toString(prefix: String): String = {
    val str = new StringBuilder("")
    for (row <- board) {
      str.append(prefix + "|")
      for (spot <- row) {
        if (spot == null) {
          str.append(" |")
        } else if (spot == RED) {
          str.append("R|")
        } else {
          str.append("Y|")
        }
      }
      str.append("\n")
    }
    str.toString
  }

  /**
   * Return the Player that has four in a row (or null if no player has).
   */
  def hasConnectFour(): Option[Player] = {
    winLocations().find(loc => loc(0) != null && loc(0) == loc(1) && loc(0) == loc(2) &&
      loc(0) == loc(3))
      .map(_(0))
      .orElse(None)
  }

  /**
   * Return a list of all locations where it is possible to
   * achieve connect four. In this context, a "win location" is an
   * array of the Player pieces on this Board from four connected tiles.
   */
  def winLocations(): List[Array[Player]] = {
    val locations = ListBuffer[Array[Player]]()
    for (delta <- deltas; r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS) {
      val loc = possibleWin(r, c, delta)
      if (loc != null) {
        locations += loc
      }
    }
    locations.toList
  }

  /**
   * If the four locations in a row beginning in board[r][c] going in the direction
   * given by [delta[0]][delta[1]] are on the board, return an array of them.
   * Otherwise, return null;
   * <p/>
   * Precondition: board[r][c] is on the board and delta is one of the elements of
   * static variable deltas.
   */
  def possibleWin(r: Int, c: Int, delta: Array[Int]): Array[Player] = {
    val location = Array.ofDim[Player](FOUR)
    for (i <- 0 until FOUR) {
      val newR = r + i * delta(0)
      val newC = c + i * delta(1)
      if (0 <= newR && newR < Board.NUM_ROWS && 0 <= newC && newC < Board.NUM_COLS) {
        location(i) = board(newR)(newC)
      }
    }
    location
  }

  /**
   * Fills column c with discs.
   *
   * The top Player of column c is fetched, and the column is filled with alternating
   * discs starting with the opposite Player first to prevent four-in-a-row. If the
   * column is empty then the default first disc is of a YELLOW Player.
   *
   * Created for testing purposes.
   *
   * @param c zero-indexed column number to be filled
   */
  def fillColumn(c: Int) = {

    var top = getTop(c)
    var p1: Player = YELLOW
    var p2: Player = RED

    if (getPlayer(top - 1, c) == RED) {
      p1 = RED
      p2 = YELLOW
    }

    while (top != -1) {

      for (i <- 0 until Board.NUM_ROWS) {

        if (i % 2 == 0) {
          makeMove(new Move(p1, c))
        }
        else {
          makeMove(new Move(p2, c))
        }

        top = getTop(c)

      }
    }
  }

}

object Board {
  val NUM_ROWS = 6
  val NUM_COLS = 7

  def apply(b: Board): Board =
    new Board(b)

  def apply(): Board =
    new Board()
}
