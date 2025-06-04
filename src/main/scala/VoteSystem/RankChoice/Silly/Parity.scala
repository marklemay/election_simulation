package VoteSystem.RankChoice.Silly

import VoteSystem.RankChoice.RankChoice
import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist


class Parity(override val NunCandidates: Int)  extends RankChoice, VoteSystemFancy, VoteSystem(NunCandidates)  {

  type Aggregate = Map[Candidate, Int]

  def aggregate(e: Seq[Ballot]): Aggregate =
    e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, NunCandidates - position - 1))).groupBy((c, _) => c).map((c, v) => (c,  (v.map(_._2).sum) % NunCandidates))

  def winner(a: Aggregate): Dist[Candidate] =  {
    val mostVotes = a.maxBy(_._2)._2

    Dist.Uniform(a.filter(_._2 == mostVotes).keySet)
  }

  override def estimatedNievePrefference(b: Ballot): Map[Candidate, Double] = candidates.map(c => (c, .5)).toMap
}