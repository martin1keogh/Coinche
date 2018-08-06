package mk.coinche.models

case class ScoreBoard(northSouth: Int, westEast: Int) {
  private val winningScore = 1000

  def gameOver = math.max(northSouth, westEast) > winningScore
}
