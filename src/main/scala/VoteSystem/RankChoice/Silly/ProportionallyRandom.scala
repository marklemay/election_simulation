package VoteSystem.RankChoice.Silly

import VoteSystem.RankChoice.RankChoice
import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

// TODO there is an entire class of algorithms like this, givving different weights
// intresting to think about, would you actually want your 2nd to last choice chosen probablisticly?
class ProportionallyRandom(override val NunCandidates: Int)  extends RankChoice, VoteSystemFancy, VoteSystem(NunCandidates)  {

  type Aggregate = Map[Candidate, Int]

  def aggregate(e: Seq[Ballot]): Aggregate =
    e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, NunCandidates - position - 1))).groupBy((c, _) => c).map((c, v) => (c, v.map(_._2).sum))

  def winner(a: Aggregate): Dist[Candidate] = Dist.FromCount(a)

}

@main
def mainergerg(): Unit = {
  println("Hello world!")
  val e = ProportionallyRandom(5)
  println( e.winner(List(
    List(1,2,3,4,0),
    List(1,4,3,2,0),

    List(0,1,2,3,4),
  )))
  println( e.winner(List(
    List(0,2,3,4,1),
    List(1,2,3,4,0),
    List(3,2,4,0,1),

  )))

}