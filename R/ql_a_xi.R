

#' Rescorla-Wagner delta learning model with single learning rate and lapse parameter
#'
#' @param formula response formula specifying the response and its prediction.
#'        For models of class \code{'ql_a_xi'} this formula needs to define the
#'        response on the left-hand side, a variable coded as 1 or 2, and the
#'        feedback (aka outcome) on the right-hand side, coded as 1 or -1. In
#'        addition, you can define the block structure on the left-hand side
#'        of the formula. That is, a formula like
#'        \code{choice|block(id) ~ reward} would update (expected) values
#'        based on each trials choice and reward starting from the first row
#'        in \code{data} until \code{id[n] != id[n-1]}.
#'        In that case, updating of Q values starts again from starting values
#'        \code{c(0.0, 0.0)}.
#' @param ... You can pass further formulas to define predictions of parameters
#'        used in the response formula. Any non-formula objects passed via
#'        `...` are passed on to other functions (i.e., `brmsformula` and
#'        `brm`)

#' @inheritParams blms_model
#'
#' @returns If `formula_only` is `TRUE` then the return value is an object of
#'        class `blmsformula` (inheriting from class `brmsformula`), else the
#'        return value is of class `blmsfit` (inheriting from class `brmsfit`)
#'
#'
#' @export
ql_a_xi <-
  function(formula,
           ...,
           data = NULL,
           formula_only = !compile,
           compile = run,
           run = F,
           model_class = 'ql_a_xi',
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
                 model_class = model_class,
                 model_spec = model_spec,
                 par_form = par_form,
                 par_transform = par_transform
      )
    )
  }
