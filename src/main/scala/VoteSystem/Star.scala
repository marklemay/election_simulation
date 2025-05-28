package VoteSystem

import matr.Matrix
import prob.Dist



// https://en.wikipedia.org/wiki/STAR_voting
class Star[NunCandidates <: Int]
          (using
           Matrix.Requirements.NonNegativeDimensions[NunCandidates, NunCandidates], // TODO this should be refactored hard...
           // NunCandidates > 0,
           ValueOf[NunCandidates])
  extends VoteSystemFancy, VoteSystem(valueOf[NunCandidates])   {
  type Ballot = Map[Candidate, Int]


  override lazy val allBallots: Seq[Map[Candidate, Int]] = {
    ???
  }

  type Aggregate = (Map[Candidate, Int], Matrix[NunCandidates,NunCandidates, Int])

  def aggregate(e: Seq[Ballot]): Aggregate = {


    //e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, NunCandidates - position - 1))).groupBy((c, _) => c).map((c, v) => (c, v.map(_._2).sum))

    ???
  }

  def winner(a: Aggregate): Dist[Candidate] = {
//    val mostVotes = a.maxBy(_._2)._2
//
//    Dist.Uniform(a.filter(_._2 == mostVotes).keySet)
???
  }

}

@main
def main(): Unit = {
  println("Hello world!")
//  val e = BordaCount(5)
//  println( e.winner(List(
//    List(1,2,3,4,0),
//    List(1,4,3,2,0),
//
//    List(0,1,2,3,4),
//  )))
//  println( e.winner(List(
//    List(0,2,3,4,1),
//    List(1,2,3,4,0),
//    List(3,2,4,0,1),
//
//  )))

}