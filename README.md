
# blms

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

The goal of blms is to enable users to run non-linear regression models
using great `brms` package by Paul Bürkner that require that the
non-linear calculations are based on subsets of the data.

The use case for which I developed the package are reinforcement
learning models fit to experimental data in which participants perform
some kind of learning task and their choice behavior should be described
as being the result of incremental updates to some latent variable (i.e.
the expected value for a given choice). To fit such models in a
hierarchical manner it is necessary to calculate the (incrementally
updated) latent variable, that forms the predictor for the choice
behavior, separately for each participant and session.

Currently, it is possible to use custom user defined functions in
non-linear models in `brms` by passing them as additional `stanvars`.
However, the only options for the non-linear function to be applied to
the data are given by setting the argument `loop` of the respective
non-linear formula (`nlf()`) to either `TRUE`, meaning that the function
will be applied to each row of the data separately, or to `FALSE`,
meaning that the function will receive all data (from respective
variables passed to the function) at once. blms allows to define a
`block()` structure based on other variables of the data that will be
used to apply the function separately for each subset of the data.

blms currently provides some predefined models that make use of this
feature and corresponding wrapper functions to run those models by
providing only the variables in data that are needed to use those
functions. Each model has a defined set of parameters that will be used
for running the model function. As is standard in non-linear models in
`brms`, each of those parameters can be given an individual formula that
defines prediction of the parameter. This way, the hierarchical
structure can be defined by such linear formulas.

## Installation

You can install the development version of blms like so:

``` r
require(remotes)
remotes::install_github('simon-desch/blms')
```

## Example

Say we had a data set from `N` participants that performed a learning
task under two different conditions. Let’s further assume we wanted to
fit the choice behavior using a Hidden Markov model (cf. (Schlagenhauf
et al. 2014)) using separate emission probabilities for positive and
negative outcomes. If choice is coded as `1` or `2`, respectively and
the reward is coded as `-1` for lose and `+1` for win, `id` defines each
participant and condition defines the experimental condition (`neut`or
`cat`in this example), the we can fit the model like this:

``` r
library(brms)
library(blms)
model.hmm_rp <- hmm_rp(choice|block(condition:id)~reward,
                       gamma + c + d ~ condition + (condition|id),
                       data = data,
                       run = T,
                       chains = 4, cores = 4,
                       prior =
                         c(
                           set_prior('normal(0, 1.2)', class = 'b', coef = 'Intercept', nlpar=paste0(c('gamma', 'c', 'd'), 'raw')),
                           set_prior('normal(0, 0.5)', class = 'b', coef = 'conditioncat', nlpar=paste0(c('gamma', 'c', 'd'), 'raw')),
                           set_prior('normal(0, 0.5)', class = 'sd', nlpar=paste0(c('gamma', 'c', 'd'), 'raw'))
                           )
                       )
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-schlagenhauf2014" class="csl-entry">

Schlagenhauf, Florian, Quentin J. M. Huys, Lorenz Deserno, Michael A.
Rapp, Anne Beck, Hans-Joachim Heinze, Ray Dolan, and Andreas Heinz.
2014. “Striatal Dysfunction During Reversal Learning in Unmedicated
Schizophrenia Patients.” *NeuroImage* 89 (April): 171–80.
<https://doi.org/10.1016/j.neuroimage.2013.11.034>.

</div>

</div>
