package VoteSystem.RankChoice

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

class InstantRunoffVoting(override val NunCandidates: Int) extends RankChoice,
//  VoteSystemFancy,
  VoteSystem(NunCandidates)  {

//  type Aggregate = Map[Ballot, Int]
//
//  def aggregate(e: Seq[Ballot]): Aggregate =
//    e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, NunCandidates - position - 1))).groupBy((c, _) => c).map((c, v) => (c, v.map(_._2).sum))


  // when the number of candidates is nearly eaqual yto the number of voters, pidgeon hole effects happen.  this will keep trying to break ties.
  def removeCandidate(e: List[List[Candidate]], candidaates: Set[Candidate], depth: Int): Set[Candidate] = {

    if (e(0).size <= depth) {
      candidaates
    } else {
      val counts = e.map(_(depth)).groupBy(c => c).map((c, l) => (c, l.size))

      val fullCounts = candidaates.map(c => (c, counts.getOrElse(c, 0))).toMap

      val minCount = fullCounts.minBy(_._2)._2

      val next = fullCounts.filter((c, counts) => counts == minCount).keys.toSet

      if (next.size == 1) {
        next
      } else {
        removeCandidate(e, next, depth + 1)
      }
    }
  }


  def winnerHelper(e: List[List[Candidate]], candidaates: Set[Candidate]): Set[Candidate] = {
    if (e(0).size == 0) {
      return Set()
    }

    val firstVotes1 = e.groupBy(x => x(0)).mapValues(v => v.size)

    //and 0 to the others
    val firstVotes = candidaates.map(c => (c, firstVotes1.getOrElse(c, 0))).toMap

    //    println("firstvotes")
    //    println(firstVotes.toList)
    val win = firstVotes.filter(_._2 > e.size.toDouble / 2.0)
    //    println(win.toList)

    if (win.size > 0) {
      return win.keySet.toSet
    } else {
      //      println("runn off")
      val candidatesToRemovw = removeCandidate(e, candidaates, 0)

      val runnoff = e.map(_.filter(c => candidatesToRemovw.find(c == _).size == 0))
      //      println("new ballot")
      //      println(runnoff)

      val res = winnerHelper(runnoff, candidaates -- candidatesToRemovw)
      if (res.size > 0) {
        return res
      } else {
        // in the case of a tye runoff, jsut use the first vote getters

        val mostvotes = firstVotes.maxBy(_._2)._2

        return firstVotes.filter(_._2 == mostvotes).keySet.toSet
      }
    }
  }


  override def winner(e: Election): Dist[Candidate] = {
    Dist.Uniform(winnerHelper(e, candidates.toSet))
  }
}