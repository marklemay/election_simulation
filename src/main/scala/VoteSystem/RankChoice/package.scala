package VoteSystem

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

package object RankChoice {

  trait RankChoice extends VoteSystem, VoteSystemWithClarity {

    type Ballot = List[Candidate]

    lazy val allBallots: Set[Ballot] =
      Range(0, NunCandidates).toList.permutations.toSet

    override def estimatedNievePrefference(b: Ballot): Map[Candidate, Double] = b.zipWithIndex.map((c,i) => (c, (NunCandidates - i).toDouble / (NunCandidates + 1).toDouble)).toMap
  }


}


// TODO Test
//List(0.3239589853659237, 0.7237762197150686, 0.393561862685765)
//List(0.6411151053532688, 0.09701964973205579, 0.8477526174638808)
//List(0.5581368312184691, 0.5504887998119169, 0.7064163629006793)
//List(0.4221852727257607, 0.8274831276905598, 0.36118288105440033)
//List(0.9015449558574123, 0.9645493474737882, 0.6422602433561754)
//List(0.6785759561258068, 0.24817745105141753, 0.5470692210656187)
//Vector(3.5255171066466415, 3.411494595474807, 3.4982431885265197)
//0 -> 0.3333333333333333,1 -> 0.3333333333333333,2 -> 0.3333333333333333                              	List(1, 2, 0)                 	List(2, 0, 1)                 	List(2, 1, 0)                 	List(1, 0, 2)                 	List(0, 1, 2)                 	List(0, 2, 1)                  0.21184987326598453                      0.007849801627330955                     0.23920914844132804                     	+
//0 -> 0.0,1 -> 0.5,2 -> 0.5                                                                           	List(1, 2, 0)                 	List(2, 1, 0)                 	List(2, 1, 0)                 	List(1, 0, 2)                 	List(0, 1, 2)                 	List(0, 2, 1)                  0.18209655196345734                      0.011774702440996357                     0.23920914844132804                    	+


@main
def run(): Unit = {
  import _root_.VoteSystem.RankChoice.RankChoice

  object test extends RankChoice, VoteSystem(3){
    def winner(e: Election): prob.Dist[Candidate] = ???
  }

  val aggregatePreference = Vector(3.5255171066466415, 3.411494595474807, 3.4982431885265197)

  println(test.estimatedNievePrefference(List(2, 0, 1)))
  //val finalVote = List(	List(1, 2, 0),                	List(2,0, 1)  ,               	List(2, 1, 0)  ,               	List(1, 0, 2)       ,          	List(0, 1, 2)     ,            	List(0, 2, 1))
  val finalVote = List(	    	List(1, 2, 0)  ,               	List(2, 1, 0)    ,             	List(2, 1, 0)         ,        	List(1, 0, 2)          ,       	List(0, 1, 2)      ,           	List(0, 2, 1)  )

  val estimatedPreference = finalVote.map(b => test.estimatedNievePrefference(b))
  val estimateAggregatedPreference = test.candidates.map(c => estimatedPreference.map(_(c)).sum)
  println(estimateAggregatedPreference)
  val clarity = (test.candidates.map(c => Math.abs(aggregatePreference(c) - estimateAggregatedPreference(c))).sum) / 6.toDouble // TODO squear?
  println(clarity)

}