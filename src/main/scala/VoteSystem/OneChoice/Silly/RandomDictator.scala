package VoteSystem.OneChoice.Silly

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

// incentives honesty (only system for this voting method where honesty is the best strategy)
class RandomDictator(override val NunCandidates: Int) extends VoteSystemFancy , VoteSystem(NunCandidates)  {
  type Ballot = Int

  lazy val allBallots: List[Ballot] = candidates.toList

  type Aggregate = Map[Ballot, Int]

  def aggregate(e: Seq[Ballot]): Aggregate = e.groupBy(i => i).map((x, y) => (x, y.size)).withDefault(_ => 0)

  def winner(a: Aggregate): Dist[Candidate] = Dist.FromCount(a)
}


@main
def mai343n(): Unit = {
  println("Hello world!")
  val e = RandomDictator(5)
  println( e.winner(List(1,1,1,1,1,2,2,2,3)))

}