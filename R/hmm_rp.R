

#' Hidden-Markov-Model using separate emission probabilities for rewards and punishments
#'
#' @param formula response formula specifying the response and its prediction.
#'        For models of class \code{'hmm_rp'} this formula needs to define the
#'        response on the left-hand side, a variable coded as 1 or 2, and the
#'        feedback (aka outcome) on the right-hand side, coded as 1 or -1. In
#'        addition, you can define the block structure on the left-hand side
#'        of the formula. That is, a formula like
#'        \code{choice|block(id) ~ reward} would update the HMM's probabilities
#'        for state 1 and state 2 based on each trials choice and reward
#'        starting from the first row in \code{data} until
#'        \code{id[n] != id[n-1]}. I that case, updating of probabilities
#'        starts again from starting values \code{c(0.5, 0.5)}.
#' @param ... You can pass further formulas to define predictions of parameters
#'        used in the response formula. Any non-formula objects passed via
#'        `...` are passed on to other functions (i.e., `brmsformula` and
#'        `brm`)

#' @inheritParams blms_model
#'
#' @returns If `formula_only` is `TRUE` then the return value is an object of
#'        class `blmsformula` (inheriting from class `brmsformula`), else the
#'        return value is of class `blmsfit` (inheriting from class `brmsfit`)
#' @references Schlagenhauf, Florian, Quentin J. M. Huys, Lorenz Deserno, Michael
#'A. Rapp, Anne Beck, Hans-Joachim Heinze, Ray Dolan, and Andreas Heinz. 2014.
#'“Striatal Dysfunction During Reversal Learning in Unmedicated Schizophrenia
#'Patients.” *NeuroImage* 89 (April): 171–80.
#'<https://doi.org/10.1016/j.neuroimage.2013.11.034>
#'
#'
#' @export
hmm_rp <-
  function(formula,
           ...,
           data = NULL,
           formula_only = !compile,
           compile = run,
           run = F,
           model_class = 'hmm_rp',
           model_spec = get_model_spec(model_class),
           par_form = model_spec[['par_form']],
           par_transform = model_spec[['par_transform']]
  ) {
    return(
      blms_model(formula,
                 ...,
                 data = data,
                 formula_only = formula_only,
                 compile = compile,
                 run = run,
                 model_class = 'hmm_rp',
                 model_spec = get_model_spec(model_class),
                 par_form = model_spec[['par_form']],
                 par_transform = model_spec[['par_transform']]
                 )
      )
  }
