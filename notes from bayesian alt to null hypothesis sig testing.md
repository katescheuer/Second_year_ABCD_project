notes from bayesian alternative to null hypothesis significance testing talk:

bayes factors are not posterior odds: if bayes factor is 4.2, do NOT write "h0 is 4.2 more likely than h1 after observing the data"
priors matter: if change prior distrib then bayes factor changes so always report the prior that you used and justify it
bayes factors provide relative assessments: do not say there is evidence in favor of h0, instead say there is evidence in favor of h0 relative to (this) h1
bayes factors cannot establish the presence or absence of an effect: imagine bf=1000, do not say there is no effect. bayes factors cannot establish the absence or presence of any effect. instead say there is very strong evidence in favor of h0 relative to (this) h1
bayes factors are not effect sizes: large bayes factor does not mean effect size is very small
inconclusive evidence is not evidence of absence: bayes factor close to 1 does not tell us very much, if close to 1 then do not say there is (evidence of) no effect. instead say the evidence is ambiguous or the observated data are equally predicatble under either hypothesis
using description labels is not problem-free: imagine bayes factor = 0.5, can't use different classificaiton system to label "substantial" or "positive" or "moderate". do nto use these classificaiton systems. instead explain the amoutn of evidence int eh context of the research being conducted, look at belief (posterior probabilities)

There is a shiny app to help teach you these things

More on effect sizes:
significance is not the same as relevance
large bayes factor does not mean big effect
need to carefully study effect size and degree of uncertainty, requires using posterior distribution
how to interpret posterior probabilities?
    show where the most probability mass is: identify smallest interval with 95% probability (high density interval), similar to "credible internal" which gives middle 95% of probability mass
    compute probability for prespecified parameter ranges eg what is the posterior probability that theta > 0.6?

see paper null regions: a unified conceptaul framework for statistical inference
