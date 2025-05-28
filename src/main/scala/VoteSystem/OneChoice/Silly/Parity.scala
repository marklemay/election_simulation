package VoteSystem.Silly

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

/// worst system I could think of
class Parity(override val NunCandidates: Int)  extends VoteSystemFancy , VoteSystem(NunCandidates)  {
  type Ballot = Int

  lazy val allBallots: List[Ballot] = candidates.toList

  type Aggregate = Map[Ballot, Int]

  def aggregate(e: Seq[Ballot]): Aggregate = e.groupBy(i => i).map((x, y) => (x, y.size % NunCandidates)).withDefault(_ => 0)

  def winner(a: Aggregate): Dist[Candidate] = {
    val mostVotes = a.maxBy(_._2)._2

    Dist.Uniform(a.filter(_._2 == mostVotes).keySet)
  }
}


@main
def main(): Unit = {
  println("Hello world!")
  val e = Parity(5)
  println( e.winner(List(1,1,1,1,1,2,2,2)))

}