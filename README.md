

## Some hacky scripts to compare voting systems

https://www.rankedvote.co/guides/understanding-ranked-choice-voting/2024-rcv-on-the-ballot

I made some scripts to simulate elections under different voting systems:

* Instant runoff better reflects voter preference better than plurality when there at 5 candidates
* Instant runoff and plurality perform about the same with 3 or fewer candidates
* Approval performed best of the most popular proposed voting systems
* Rank choice can perform better with different resolution criteria than instant run off
* all differences were marginal (about a 1 or 2% per voter)

raw [results.txt](results.txt).  Sorry for the mess and the bugs.

## what does the reasurch say?


https://chatgpt.com/share/6814eabd-9ac8-8002-9dc3-f868c6a8f13a
https://chatgpt.com/share/682b314b-32e0-8002-b38a-02033a275d79



https://link.springer.com/article/10.1007/s10602-022-09376-8#:~:text=Using%20computer%20simulations%20based%20on,seven%20rules%20still%20deserve%20further
https://link.springer.com/article/10.1007/s10602-022-09376-8#:~:text=The%20social%20welfare%20of%20candidate,17%20%20and%20%2092
https://link.springer.com/article/10.1007/s10602-022-09389-3#:~:text=Smith%2FMinimax%20and%20STAR%20Voting%20delivered,scenario%20voter%20behavior.Footnote%2010


Mass stuff
https://www.bostonglobe.com/2025/02/26/opinion/ranked-choice-voting-boston/


## todo
* optimal single winner preferential voting system based on game theory
* implement STAR https://www.equal.vote/star
* citation needed on borda wiki?
* 