
package VoteSystem

import prob.Dist

import scala.compiletime.ops.int.>


type NonNegative[R <: Int] = R > 0


trait VoteSystem(val NunCandidates: Int) {

  type Candidate = Int
  val candidates: Seq[Candidate] = Range(0, NunCandidates)
  type Ballot

  lazy val allBallots: Seq[Ballot]

  // TODO remove type ailias? better as Seq[Ballot]
  type Election = List[Ballot]

  def winner(e: Election): Dist[Candidate]

}


trait VoteSystemRestricted[NunCandidates <: Int](using
                                                  ValueOf[NunCandidates],
                                                  NunCandidates > 0
                                                ) extends  VoteSystem{

  override val NunCandidates: Int = valueOf[NunCandidates]
  type Candidate = Int
  override val candidates: Seq[Candidate] = Range(0, NunCandidates)
  type Ballot

  lazy val allBallots: List[Ballot]

  // TODO remove type ailias? better as Seq[Ballot]
  type Election = List[Ballot]

  def winner(e: Election): Dist[Candidate]
}


trait VoteSystemFancy extends VoteSystem {

  // minimal information that can be collected from any fraction of ballots to detrmine the winner
  type Aggregate

  def aggregate(a: Seq[Ballot]): Aggregate
  // TODO ++

  def winner(a: Aggregate): Dist[Candidate]

  override def winner(e: Election): Dist[Candidate] = winner(aggregate(e))
}




// TODO base off of partial "counts" that can(?) be moddeled with stats functions
