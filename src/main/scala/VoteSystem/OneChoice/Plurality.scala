package VoteSystem.OneChoice

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

class Plurality(override val NunCandidates: Int)  extends VoteSystemFancy, VoteSystem(NunCandidates)  {


  type Ballot = Int

  lazy val allBallots: Set[Ballot] = candidates.toSet

  type Aggregate = Map[Ballot, Int]

  def aggregate(e: Seq[Ballot]): Aggregate = e.groupBy(i => i).map((x, y) => (x, y.size)).withDefault(_ => 0)

  def winner(a: Aggregate): Dist[Candidate] = {
    val mostVotes = a.maxBy(_._2)._2

    Dist.Uniform(a.filter(_._2 == mostVotes).keySet)
  }

}

@main
def main(): Unit = {
  println("Hello world!")
  val e = Plurality(5)
  println( e.winner(List(1,1,4,1,2,2,2)))

}