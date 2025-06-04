package VoteSystem.Silly

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

class Random(override val NunCandidates: Int)  extends VoteSystemFancy, VoteSystem(NunCandidates)  {


  type Ballot = Unit

  lazy val allBallots: Set[Ballot] = Set(())

  type Aggregate = Unit

  def aggregate(e: Seq[Ballot]): Aggregate = ()

  def winner(a: Aggregate): Dist[Candidate] = Dist.Uniform(candidates.toSet)

}


@main
def RandomMain(): Unit = {
  println("Hello world!")
  val e = Random(5)
  println( e.winner(List((),(),())))
}