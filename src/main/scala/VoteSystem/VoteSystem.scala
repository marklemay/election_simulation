
package VoteSystem

import prob.Dist


trait VoteSystem(val candidates: Int) {

  type Candidate = Int
  type Ballot


  // TODO remove type ailias? better as Seq[Ballot]
  type Election = List[Ballot]

  def winner(e: Election): Dist[Candidate]

  }



// TODO base off of partial "counts" that can(?) be moddeled with stats functions
