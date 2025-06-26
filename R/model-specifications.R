
#' Model specification for \code{blms_model}
#'
#' @param class \code{character}. The name of the model class.
#' @param parameters Named \code{list()} containing \code{list}s that define
#'        lower, upper bound, and optinally other specification for single
#'        parameters. The names of this list are interpreted as
#' @param resp_var \code{character}. Name for the response variable.
#' @param pred_vars \code{character}. Names of one or more response variables.
#' @param func_name \code{character}. Name of the Stan function that defines
#'        the model.
#' @param func_stanvar \code{stanvar} created using \code{brms::stanvar()} with
#'          \code{block = 'functions'} containing at least the model function.
#' @param description \code{character}, optional. Description of the model.
#' @param details \code{character}, optional. Details for the model.
#' @param note \code{character}, optional. Notes with additional information
#'        about the model.
#' @param example \code{character}, optional. Example code for running the
#'        model.
#' @param block_var \code{character}, optional. Name for the variable that
#'        defines the blocks in the data. This will be created by the
#'        \code{block()} call. Defaults to \code{'bockgrp'}.
#'        See \code{\link{blms_model}} for details.
#' @param dec_var \code{character}, optional. Name for the \code{dec} variable.
#'        Only used for models that use \code{\link[brms]{wiener()}} as their
#'        family.
#' @param fixed_vars Named \code{list()}, optional. List of variables or
#'        parameters used in model formulas whose values should be fixed.
#' @param func_data \code{character} vector, optional. List of data variables
#'        (including \code{block_var}) that will be passed to the model
#'        function defined in \code{func_stanvar}. Defaults to
#'        \code{c(block_var, resp_var, pred_vars)}.
#' @param func_params \code{character} vector, optional. List of parameters
#'        that will be passed to the model function defined in
#'        \code{func_stanvar}. Defaults to \code{names(parameters)}.
#' @param func_input \code{character} vector, optional. List of variables that
#'        will be passed to the model function defined in \code{func_stanvar}.
#' @param func_par \code{character}, optional. Name of the parameter for which
#'        \code{pred_vars} are specified on the right-hand side of its
#'        formula. The right-hand side of this formula will be replaced by a
#'        call to the model function defined in \code{func_stanvar}. Defaults
#'        to \code{mu}
#' @param family \code{family}, optional. Family used to predict
#'        \code{resp_var}. Defaults to \code{bernoulli(link = 'identity')},
#'        meaning that the expected prediction formula (usually the output of
#'        the model function defined in \code{func_stanvar}) provides the
#'        probability for the upper bound (\code{TRUE} if \code{resp_var}
#'        provided in the input data is \code{logical},
#'        \code{levels(factor(resp_var)))[2]} if \code{resp_var} provided in
#'        the input data is \code{facor}, \code{character}, or \code{numeric}).
#' @param par_form \code{list()}, optional. List of formulas for the
#'        prediction of parameters given by \code{parameters}. Defaults to
#'        \code{parameter ~ 1} for each parameter in \code{names(parameters)}.
#'        Multiple parameters on the left-hand side of the formula combined by
#'        \code{+} are allowed. The respective formula on the right-hand side
#'        will be used for each parameter on the left-hand side. If
#'        \code{par_form} is given as an argument to \code{blms_model},
#'        the formulas will overwrite those specified by \code{par_form}
#'        defined in the \code{model_spec}. Formulas passed via \code{...}
#'        to \code{blms_model} will overwrite formulas passed via its
#'        \code{par_form} argument or defined by its \code{model_spec}.
#' @param par_transform \code{list()}, optional. List of transformation
#'        formulas for parameters in \code{names(parameters)}. Transformation
#'        formulas given by \code{par_transform} can take the form of e.g.
#'        \code{parameter ~ inv_logit(parameter) * 5}. In this case, the
#'        parameter name on the right-hand side will be appended by
#'        \code{'raw'} (e.g. \code{alpha ~ inv_logit(alpha)} will become
#'        \code{alpha ~ inv_logit(alpharaw)}) and the corresponding formula
#'        for the prediction of the parameter will take the the new name
#'        (e.g. \code{alpharaw}) on their left-hand side (e.g.
#'        \code{alpha ~ 1} will become \code{alpharaw ~ 1}). Multiple
#'        parameters combined by \code{+} can be specified on the left-hand
#'        side of the formula. In this case, the variable \code{x} on the
#'        right-hand side of the formula will be interpreted as the
#'        parameter and replaced for each parameter on the left-hand side as
#'        described above (e.g. \code{alpha + beta ~ inv_logit(x)} will
#'        create two formulas \code{alpha ~ inv_logit(alpharaw)} and
#'        \code{beta ~ inv_logit(betaraw)} and the formulas for \code{alpha}
#'        and \code{beta} will have their left-hand side replaced by
#'        \code{alpharaw} and \code{betaraw}, respectively). If
#'        \code{par_transform} is given as an argument to \code{blms_model},
#'        the formulas will overwrite those specified by \code{par_transform}
#'        defined in the \code{model_spec}. In addition, transformation
#'        formulas can be given as formulas passed via the \code{...}
#'        arguments to \code{blms_model}, using a \code{transform()} call
#'        on their left-hand side. Again, multiple parameters combined by
#'        \code{+} can be specified on the left-hand side
#'        (inside the \code{transform()} call). Formulas passed via \code{...}
#'        to \code{blms_model} will overwrite formulas passed via its
#'        \code{par_transform} argument or defined by its \code{model_spec}.
#' @param use_blm \code{logical}, optional. Should the block structure be
#'        handled by \code{\link{blm_model}} internally. In this case,
#'        \code{func_input} will not contain \code{block_var}, so that the
#'        function specified in \code{func_stanvar} should not take
#'        \code{block_var} as its first argument. Currently not used.
#' @param ... Further arguments added to
#'
#' @returns Object of class \code{model_spec} to be used with \code{blms_model}.
#' @export
#'
#' @examples
#' func_wsls <- brms::stanvar(block = 'functions', scode = '
#'  vector win_stay_lose_switch(
#'    vector blockgrp, vector action, vector outcome, // func_data
#'    vector beta) { // func_params
#'    int N = size(action);
#'    vector out[N];
#'    out[1] = 0.5;
#'    for (n in 2:N) {
#'      if (blockgrp[n]!=blockgrp[n-1]) {
#'        out[n] = 0.5;
#'        continue;
#'      }
#'      int stay = action[n] == action[n-1] ? 1 : -1;
#'      int win = outcome > 0 ? 1 : -1;
#'      int resp_bound = action[n] == max(action) ? 1 : -1;
#'      real beta_x = stay * outcome * resp_bound * beta[n];
#'      out[n] = inv_logit(beta_x);
#'     }
#'     return(out)
#'  }
#' '
#' )
#' mspec <- new_model_spec(class = 'wsls',
#'                         description = 'Win-Stay-Lose-Switch Model',
#'                         parameters = list(beta=list(lb = -Inf, ub = Inf)),
#'                         resp_var = 'choice',
#'                         pred_vars = c('outcome'),
#'                         func_name = 'win_stay_lose_switch',
#'                         func_stanvar = func_wsls
#'                         )
#'
#'
#'
new_model_spec <-
  function(
    class,
    parameters,
    resp_var,
    pred_vars,
    func_name,
    func_stanvar,
    description = '',
    details = '',
    note = '',
    example = '',
    block_var = 'blockgrp',
    dec_var = NULL,
    fixed_vars = list(),
    func_data = c(resp_var, pred_vars),
    func_params = c(names(parameters)),
    func_input = c(block_var, resp_var, pred_vars, names(parameters)),
    func_par = 'mu',
    family =  brms::bernoulli(link = 'identity'),
    par_form = list(),
    par_transform = list(),
    use_blm = F,
    ...
  ) {
    if(missing(class)) {
      stop('Argument \'class\' needs to be provided.')
    }
    if(missing(parameters)) {
      stop('Argument \'parameters\' needs to be provided.')
    }
    if(missing(resp_var)) {
      stop('Argument \'resp_var\' needs to be provided.')
    }
    if(missing(pred_vars)) {
      stop('Argument \'pred_vars\' needs to be provided.')
    }
    if(missing(func_name)) {
      stop('Argument \'func_name\' needs to be provided.')
    }
    if(missing(func_stanvar)) {
      stop('Argument \'func_stanvar\' needs to be provided.')
    }


    # message('original')
    # print(par_form)
    if(length(par_form)!=0) {
      par_form <- split_flist_by_lhs(par_form)
    }
    # message('spliiited')
    # print(par_form)
    default_par_forms <-
      sapply(names(parameters), \(x) as.formula(paste0(x, ' ~ 1')),
             simplify = FALSE, USE.NAMES = TRUE)
    # message('default')
    # print(default_par_forms)
    undefined_par_forms <- setdiff(names(parameters), names(par_form))
    # message('subsetted')
    # print(default_par_forms[undefined_par_forms])
    par_form <- modifyList(par_form, default_par_forms[undefined_par_forms])
    # message('modified')
    # print(par_form)
    out <- nlist(
      class,
      parameters,
      resp_var,
      pred_vars,
      func_name,
      func_stanvar,
      description,
      details,
      note,
      example,
      block_var,
      dec_var,
      fixed_vars,
      func_input,
      func_data,
      func_params,
      use_blm,
      func_par,
      family,
      par_form,
      par_transform
    )
    dots <- list(...)
    dots <- dots[names(dots)!='']
    if(length(dots)) out <- c(out, dots)
    structure(out, class = 'model_spec')
  }

#' print method for \code{model_spec}
#'
#' @param ms A \code{model_spec} object.
#'
#' @method print model_spec
#' @export
print.model_spec <-
  function(ms) {
    cat('[', ms$class, ']\n')
    cat(paste(rep('=', nchar(ms$class)+4), collapse = ''), '\n')
    cat(ms$description, '\n\n')
    cat('Variables:\n')
    cat('response: ', ms$resp_var, '\n')
    cat(paste0('predictor', ifelse(length(ms$pred_vars)>1L, 's', ''), ': ',
        toString(ms$pred_vars), '\n'))
    cat('\n')
    cat('Parameters:\n')
    for (par in names(ms$parameters)) {
      par_def <- ms$parameters[[par]]
      cat(paste0(par, ' [', par_def$lb, ', ', par_def$ub, ']'))
      if ('desc'%in%names(par_def)) {
        cat(': ', par_def$desc)
      }
      cat('\n')
    }
    cat('\n')

    # suppressMessages(
    formula_lhs_ <- ms$resp_var
    if (!is.null(ms$dec_var)) {
      formula_lhs_ <- paste0(formula_lhs_,  '| dec(', ms$dec_var, ')')
    }
    formula_ <-
        as.formula(
          paste0(formula_lhs_, '~', paste(ms$pred_vars, collapse = '+'))
        )
    func_formula <- NULL
    if (ms$func_par!='mu') {
      formula_ <- as.formula(paste0(formula_lhs_, '~', ms$func_par))
      func_formula <-
        as.formula(
          paste0(ms$func_par, '~', paste(ms$pred_vars, collapse = '+'))
        )
    }

    cat('Formulas: \n')
    sample_form <-
      do.call(blmsformula,
              c(formula_,
                func_formula,
                list(model_spec = ms)
              )
        )
    # )
    print(sample_form)
    # cat('\n')

    # cat('Familiy:')
    print(ms$family)
  }

ms_env <- new.env(parent = emptyenv())
ms_env$all_model_specs <- NULL

create_all_model_specs <-
  function() {
    model_specs <-
      list(


        ql_a_it =
          new_model_spec(
            class = 'ql_a_it',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and inverse temperature'),
            parameters = list(
              alpha = list(lb = 0, ub = 1, desc = 'Learning rate'),
              tau = list(lb = 0.0, ub = Inf, desc = 'Inverse temperature')
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_it_probs',
            func_stanvar = ql_a_it_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   tau ~ inv_logit(tau) * 5.0),
            doc_helper = list(
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
                ),
              update_formulas = c(
                paste0('\\deqn{Q_{a,t+1} = Q_{a,t} + ',
                       '\\alpha \\times (R_{t} - Q_{a,t})}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{p(a_{i}) = \\frac{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}}}')
              )
            )
          ),
        ql_a_2it =
          new_model_spec(
            class = 'ql_a_it',
            description = paste0('Rescorla-Wagner (delta) learning model with ',
                                 'single learning rate and separate inverse ',
                                 'temperatures based on the outcome of the ',
                                 'previous trial'),
            parameters = list(
              alpha = list(lb = 0, ub = 1, desc = 'Learning rate'),
              taupos = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature ',
                                          'after receiving positive outcome')),
              tauneg = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature ',
                                          'after receiving negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2it_probs',
            func_stanvar = ql_a_2it_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   taupos + tauneg ~ inv_logit(x) * 5.0),
            doc_helper = list(
              title = paste0('RL model with single learning rate and ',
                             'dual inverse temperature'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_it =
          new_model_spec(
            class = 'ql_2a_it',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and inverse ',
                                 'temperature'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Inverse temperature'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_it_probs',
            func_stanvar = ql_2a_it_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   tau ~ inv_logit(tau) * 5.0),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'inverse temperature'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}}, ',
                       ' }')
              )
            )
          ),
        ql_2a_2it =
          new_model_spec(
            class = 'ql_2a_it',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'inverse temperatures based on the outcome ',
                                 'of the previous trial'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              taupos = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a positive outcome')),
              tauneg = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_2it_probs',
            func_stanvar = ql_2a_2it_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   taupos + tauneg ~ inv_logit(x) * 5.0),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'dual inverse temperature'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_xi =
          new_model_spec(
            class = 'ql_a_xi',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and lapse parameter'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              xi = list(lb = 0, ub = 1,
                        desc = paste0('Lapse parameter'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_xi_probs',
            func_stanvar = ql_a_xi_probs_func,
            par_transform =
              list(alpha + xi ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model with learning rate and ',
                             'lapse parameter'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       '}')
              )
            )
          ),
        ql_a_2xi =
          new_model_spec(
            class = 'ql_a_2xi',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate lapse ',
                                 'parameters based on the outcome of the ',
                                 'previous trial'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              xipos = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a positive outcome')),
              xineg = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2xi_probs',
            func_stanvar = ql_a_2xi_probs_func,
            par_transform =
              list(alpha + xineg + xipos ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model with learning rate and ',
                             'dual lapse parameter'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\xi = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_xi =
          new_model_spec(
            class = 'ql_2a_xi',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              xi = list(lb = 0, ub = 1,
                        desc = paste0('Lapse parameter'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_xi_probs',
            func_stanvar = ql_2a_xi_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xi ~ inv_logit(xi)),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'lapse parameter'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_2xi =
          new_model_spec(
            class = 'ql_2a_2xi',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              xipos = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a positive outcome')),
              xineg = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2xi_probs',
            func_stanvar = ql_2a_2xi_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xipos + xineg ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'dual lapse parameter'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\xi = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_rho =
          new_model_spec(
            class = 'ql_a_rho',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and reward sensitivity ',
                                 'parameter'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_rho_probs',
            func_stanvar = ql_a_rho_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha), rho ~ inv_logit(rho) * 20),
            doc_helper = list(
              title = paste0('RL model with learning rate and ',
                             'reward sensitivity'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_rho =
          new_model_spec(
            class = 'ql_2a_rho',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and reward ',
                                 'sensitivity parameter'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_rho_probs',
            func_stanvar = ql_2a_rho_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rho ~ inv_logit(rho) * 20),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'reward sensitivity'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_a_2rho =
          new_model_spec(
            class = 'ql_a_2rho',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate reward ',
                                 'sensitivity parameters for positive and ',
                                 'negative outcomes'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2rho_probs',
            func_stanvar = ql_a_2rho_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   rhopos + rhoneg ~ inv_logit(x) * 20),
            doc_helper = list(
              title = paste0('RL model with learning rate and ',
                             'dual reward sensitivity'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} &= ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_2rho =
          new_model_spec(
            class = 'ql_2a_2rho',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'reward sensitivity parameters for positive ',
                                 'and negative outcomes'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2rho_probs',
            func_stanvar = ql_2a_2rho_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rhopos + rhoneg ~ inv_logit(x) * 20),
            doc_helper = list(
              title = paste0('RL model with dual learning rate and ',
                             'dual reward sensitivity'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       ' } \\deqn{ ',
                       '\\quad \\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       ', \\quad \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),





        ql_a_it_fu =
          new_model_spec(
            class = 'ql_a_it_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and inverse ',
                                 'temperature plus fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              tau = list(lb = 0.0, ub = Inf,
                         desc = paste0('Inverse temperature'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_it_fu_probs',
            func_stanvar = ql_a_it_fu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   tau ~ inv_logit(tau) * 5.0),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'inverse temperature, and fictitious update of ',
                             'the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_a_2it_fu =
          new_model_spec(
            class = 'ql_a_it_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate inverse ',
                                 'temperatures based on the outcome of the ',
                                 'previous trial plus fictitious update of ',
                                 'the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              taupos = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                   'a positive outcome')),
              tauneg = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                   'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2it_fu_probs',
            func_stanvar = ql_a_2it_fu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   taupos + tauneg ~ inv_logit(x) * 5.0),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'dual inverse temperature, and fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_it_fu =
          new_model_spec(
            class = 'ql_2a_it_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and inverse ',
                                 'temperature plus fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                     'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                     'a negative outcome')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Inverse temeprature'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_it_fu_probs',
            func_stanvar = ql_2a_it_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   tau ~ inv_logit(tau) * 5.0),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'inverse temperature, and fictitious update of ',
                             'the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_2it_fu =
          new_model_spec(
            class = 'ql_2a_2it_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'inverse temperatures based on the outcome ',
                                 'of the previous trial plus fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                     'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              taupos = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a positive outcome')),
              tauneg = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_2it_fu_probs',
            func_stanvar = ql_2a_2it_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   taupos + tauneg ~ inv_logit(x) * 5.0),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'dual inverse temperature, and fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_xi_fu =
          new_model_spec(
            class = 'ql_a_xi_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and lapse parameter ',
                                 ' plus fictitious update of the unchosen ',
                                 'option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1),
              xi = list(lb = 0, ub = 1)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_xi_fu_probs',
            func_stanvar = ql_a_xi_fu_probs_func,
            par_transform =
              list(alpha + xi ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'lapse parameter, and fictitious update of the ',
                             'unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       '}')
              )
            )
          ),
        ql_a_2xi_fu =
          new_model_spec(
            class = 'ql_a_2xi_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate lapse ',
                                 'parameters based on the outcome of the ',
                                 'previous trial plus fictitious update of ',
                                 'the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Leanring rate')),
              xipos = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a positive outcome')),
              xineg = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2xi_fu_probs',
            func_stanvar = ql_a_2xi_fu_probs_func,
            par_transform =
              list(alpha + xipos + xineg ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'dual lapse parameter, and fictitious update ',
                             'of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       # 'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_xi_fu =
          new_model_spec(
            class = 'ql_2a_xi_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter plus fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              xi = list(lb = 0, ub = 1,
                        desc = paste0('Lapse parameter'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_xi_fu_probs',
            func_stanvar = ql_2a_xi_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xi ~ inv_logit(xi)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'lapse parameter, and fictitious update of ',
                             'the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       # ' } \\deqn{ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',

                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       '}')
              )
            )
          ),
        ql_2a_2xi_fu =
          new_model_spec(
            class = 'ql_2a_2xi_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter plus fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              xipos = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a positive outcome')),
              xineg = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2xi_fu_probs',
            func_stanvar = ql_2a_2xi_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xipos + xineg ~ inv_logit(x)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'dual inverse temperature, and fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_rho_fu =
          new_model_spec(
            class = 'ql_a_rho_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and reward sensitivity ',
                                 'parameter plus fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_rho_fu_probs',
            func_stanvar = ql_a_rho_fu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha), rho ~ inv_logit(rho) * 20),
            doc_helper = list(
              title = paste0('RL model with learning rate, ',
                             'reward sensitivity, and fictitious update ',
                             'of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_rho_fu =
          new_model_spec(
            class = 'ql_2a_rho_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and reward ',
                                 'sensitivity parameter plus fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_rho_fu_probs',
            func_stanvar = ql_2a_rho_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rho ~ inv_logit(rho) * 20),
            doc_helper = list(
              title = paste0('RL model with dual learning rate, ',
                             'reward sensitivity, and fictitious update ',
                             'of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_a_2rho_fu =
          new_model_spec(
            class = 'ql_a_2rho_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate reward ',
                                 'sensitivity parameters for positive and ',
                                 'negative outcomes plus fictitious update of ',
                                 'the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2rho_fu_probs',
            func_stanvar = ql_a_2rho_fu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   rhopos + rhoneg ~ inv_logit(x) * 20),
            doc_helper = list(
              title = paste0('RL model with learning rate, ',
                             'dual reward sensitivity, and fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_2rho_fu =
          new_model_spec(
            class = 'ql_2a_2rho_fu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'reward sensitivity parameters for positive ',
                                 'and negative outcomes plus fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2rho_fu_probs',
            func_stanvar = ql_2a_2rho_fu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rhopos + rhoneg ~ inv_logit(x) * 20),
            doc_helper = list(
              title = paste0('RL model with dual learning rate, ',
                             'dual reward sensitivity, and fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       ', \\quad \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),



        ql_a_it_kfu =
          new_model_spec(
            class = 'ql_a_it_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and inverse ',
                                 'temperature plus weighted fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              tau = list(lb = 0.0, ub = Inf,
                         desc = paste0('Inverse temperature')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_it_kfu_probs',
            func_stanvar = ql_a_it_kfu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   tau ~ inv_logit(tau) * 5.0,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'inverse temperature, and weighted fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_a_2it_kfu =
          new_model_spec(
            class = 'ql_a_it_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate inverse ',
                                 'temperatures based on the outcome of the ',
                                 'previous trial plus weighted fictitious ',
                                 'update of the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              taupos = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                   'a positive outcome')),
              tauneg = list(lb = 0.0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                   'a negative outcome')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2it_kfu_probs',
            func_stanvar = ql_a_2it_kfu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   taupos + tauneg ~ inv_logit(x) * 5.0,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'dual inverse temperature, and weihted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_it_kfu =
          new_model_spec(
            class = 'ql_2a_it_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and inverse ',
                                 'temperature plus weighted fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1),
              alphaneg = list(lb = 0, ub = 1),
              tau = list(lb = 0, ub = Inf),
              kappa = list(lb = 0, ub = 1)
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_it_kfu_probs',
            func_stanvar = ql_2a_it_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   tau ~ inv_logit(tau) * 5.0,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'inverse temperature, and weighted fictious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times (-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_2it_kfu =
          new_model_spec(
            class = 'ql_2a_it_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'inverse temperatures based on the outcome ',
                                 'of the previous trial plus weighted ',
                                 'fictitious update of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                     'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              taupos = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a positive outcome')),
              tauneg = list(lb = 0, ub = Inf,
                            desc = paste0('Inverse temperature after receiving ',
                                          'a negative outcome')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = 'blockgrp',
            func_name = 'ql_2a_2it_kfu_probs',
            func_stanvar = ql_2a_2it_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   taupos + tauneg ~ inv_logit(x) * 5.0,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model dual learning rate,  ',
                             'dual inverse temperature, and weighted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}} ',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\tau_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\tau_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_xi_kfu =
          new_model_spec(
            class = 'ql_a_xi_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and lapse parameter ',
                                 'plus weighted fictitious update of the ',
                                 'unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = 'Learning rate'),
              xi = list(lb = 0, ub = 1,
                        desc = 'Lapse parameter'),
              kappa = list(lb = 0, ub = 1,
                           desc = 'Discount weight')
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_xi_kfu_probs',
            func_stanvar = ql_a_xi_kfu_probs_func,
            par_transform =
              list(alpha + xi ~ inv_logit(x),
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'inverse temperature, and weighted fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       # 'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t}  - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       '}')
              )
            )
          ),
        ql_a_2xi_kfu =
          new_model_spec(
            class = 'ql_a_2xi_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate lapse ',
                                 'parameters based on the outcome of the ',
                                 'previous trial plus weighted fictitious ',
                                 'update of the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Leanring rate')),
              xipos = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a positive outcome')),
              xineg = list(lb = 0, ub = 1,
                           desc = paste0('Lapse parameter after receiving ',
                                         'a negative outcome')),
              kappa = list(lb = 0, ub = 1,
                           desc = 'Discount weight')
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2xi_kfu_probs',
            func_stanvar = ql_a_2xi_kfu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   xipos + xineg ~ inv_logit(x),
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model learning rate, ',
                             'dual inverse temperature, and weighted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_2a_xi_kfu =
          new_model_spec(
            class = 'ql_2a_xi_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter plus weighted fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              xi = list(lb = 0, ub = 1,
                        desc = paste0('Lapse parameter')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_xi_kfu_probs',
            func_stanvar = ql_2a_xi_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xi ~ inv_logit(xi),
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'lapse parameter, and weighted fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       ' \\\\ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' }\\deqn{',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       '}')
              )
            )
          ),
        ql_2a_2xi_kfu =
          new_model_spec(
            class = 'ql_2a_2xi_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and lapse ',
                                 'parameter plus weighted fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = 'Learning rate after positive outcome'),
              alphaneg = list(lb = 0, ub = 1,
                              desc = 'Learning rate after negative outcome'),
              xipos = list(lb = 0, ub = 1,
                           desc = 'Lapse parameter after positive outcome'),
              xineg = list(lb = 0, ub = 1,
                           desc = 'Lapse parameter after positive outcome'),
              kappa = list(lb = 0, ub = 1,
                           desc = 'Discount weight')
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2xi_kfu_probs',
            func_stanvar = ql_2a_2xi_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   xipos + xineg ~ inv_logit(x),
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model dual learning rate, ',
                             'dual inverse temperature, and weighted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-R_{t} - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\left(\\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}}\\right) ',
                       '\\times (1 - \\xi) + \\frac{\\xi}{2}',
                       ',\\quad \\text{where } \\tau = \\begin{cases} ',
                       '\\xi_{pos} & \\text{if } R_{t-1} \\geq 0',
                       ' \\\\ ',
                       '\\xi_{neg} & \\text{if } R_{t-1} \\lt 0',
                       '\\end{cases} ',
                       '}')
              )
            )
          ),
        ql_a_rho_kfu =
          new_model_spec(
            class = 'ql_a_rho_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and reward sensitivity ',
                                 'parameter plus weighted fictitious update ',
                                 'of the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity')),
              kappa = list(lb = 0, ub = 1,
                           desc = 'Discount weight')
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_rho_kfu_probs',
            func_stanvar = ql_a_rho_kfu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   rho ~ inv_logit(rho) * 20,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model with learning rate, ',
                             'reward sensitivity, and weighted fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\alpha \\times (-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_rho_kfu =
          new_model_spec(
            class = 'ql_2a_rho_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and reward ',
                                 'sensitivity parameter plus weighted ',
                                 'fictitious update of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_rho_kfu_probs',
            func_stanvar = ql_2a_rho_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rho ~ inv_logit(rho) * 20,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model with dual learning rate, ',
                             'reward sensitivity, and weighted fictitious ',
                             'update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       ' \\\\ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa \\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_a_2rho_kfu =
          new_model_spec(
            class = 'ql_a_2rho_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and separate reward ',
                                 'sensitivity parameters for positive and ',
                                 'negative outcomes plus weighted fictitious ',
                                 'update of the unchosen option'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Learning rate')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_a_2rho_kfu_probs',
            func_stanvar = ql_a_2rho_kfu_probs_func,
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   rhopos + rhoneg ~ inv_logit(x) * 20,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model with learning rate, ',
                             'dual reward sensitivity, and weighted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       ' \\\\ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa\\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),
        ql_2a_2rho_kfu =
          new_model_spec(
            class = 'ql_2a_2rho_kfu',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'separate learning rates for positive and ',
                                 'negative prediction errors and separate ',
                                 'reward sensitivity parameters for positive ',
                                 'and negative outcomes plus weighted ',
                                 'fictitious update of the unchosen option'),
            parameters = list(
              alphapos = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a positive outcome')),
              alphaneg = list(lb = 0, ub = 1,
                              desc = paste0('Learning rate after receiving ',
                                            'a negative outcome')),
              rhopos = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'positive outcome')),
              rhoneg = list(lb = -Inf, ub = Inf,
                            desc = paste0('Reward sensitivity for receiving ',
                                          'negative outcome')),
              kappa = list(lb = 0, ub = 1,
                           desc = paste0('Discount weight'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'ql_2a_2rho_kfu_probs',
            func_stanvar = ql_2a_2rho_kfu_probs_func,
            par_transform =
              list(alphapos + alphaneg ~ inv_logit(x),
                   rhopos + rhoneg ~ inv_logit(x) * 20,
                   kappa ~ inv_logit(kappa)),
            doc_helper = list(
              title = paste0('RL model with dual learning rate, ',
                             'dual reward sensitivity, and weighted ',
                             'fictitious update of the unchosen option'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{',
                       # '\\[ \\begin{align*}',
                       'Q_{a,t+1} = ',
                       'Q_{a,t} + \\alpha \\times (\\rho R_{t} - Q_{a,t})',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       'Q_{a_{unchosen},t+1} = ',
                       'Q_{a_{unchosen},t} + ',
                       '\\kappa\\alpha \\times ',
                       '(-(\\rho R_{t}) - Q_{a_{unchosen},t})',
                       # '\\end{align*} ',
                       # ' \\\\ ',
                       ' } \\deqn{ ',
                       '\\text{where } \\alpha = \\begin{cases} ',
                       '\\alpha_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\alpha_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       ', \\quad  \\rho = \\begin{cases} ',
                       '\\rho_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\rho_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       'p(a_{i}) = \\frac',
                       '{e^{Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{Q_{a_{j}}}} ',
                       '}')
              )
            )
          ),












        hmm_rp =
          new_model_spec(
            class = 'hmm_rp',
            description = paste0('HMM model with separate emission ',
                                 'probabilities for reward and punishment ',
                                 'outcomes (cf. Schlagenhauf et al. 2014)'),
            parameters = list(
              gamma = list(lb = 0, ub = 1,
                           desc = paste0('Transition probability')),
              c = list(lb = 0.5, ub = 1,
                       desc = paste0('Emission probability for rewards')),
              d = list(lb = 0.5, ub = 1,
                       desc = paste0('Emission probability for rewards'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            func_name = 'hmm_rp_probs',
            func_stanvar = hmm_rp_probs_func,
            par_transform =
              list(gamma ~ inv_logit(gamma),
                   c + d ~ inv_logit(x) * 0.5 + 0.5)
          ) ,
        qlddm_a =
          new_model_spec(
            class = 'qlddm_a',
            description = paste0('Reinforcement leanrining drift diffusion ',
                                 'model with learning rate \\eqn{\\eta}. ',
                                 'The scaled difference of Q-values is ',
                                 'mapped to the drift rate of ',
                                 'the Wiener distribution.'),
            parameters = list(
              delta = list(lb=-Inf, ub = Inf,
                           desc = paste0('Slope of the random walk ',
                                         '(RL model parameter)')),
              eta = list(lb = 0, ub = 1,
                         desc = paste0('Learning rate')),
              nu = list(lb=-Inf, ub = Inf,
                        desc = paste0('Scale factor for mapping the ',
                                      'difference of Q-values ',
                                      '(\\eqn{Q_{upper} - Q_{lower}}) ',
                                      'to the drift rate \\eqn{\\delta}')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Non-descision time')),
              alpha = list(lb = 0, ub = Inf,
                           desc = paste0('Boundary separation')),
              beta = list(lb = 0, ub = 1,
                          desc = paste0('Bias'))
            ),
            resp_var = 'rt',
            dec_var = 'choice',
            pred_vars = c('reward'),
            fixed_vars = list(
              rtbound = .1,
              beta = .5
            ),
            func_name = 'qlddm_a_qdiff_nu',
            func_stanvar = qlddm_a_qdiff_nu_func,
            func_input = c('blockgrp', 'choice', 'reward', 'eta', 'nu'),
            func_data = c('choice', 'reward'),
            func_params = c('eta', 'nu'),
            func_par = 'delta',
            family =  brms::wiener(link = 'identity',
                                   link_bs = 'identity',
                                   link_ndt = 'identity',
                                   link_bias = 'identity'),
            par_form = list(delta ~ 1, nu ~ 1,
                            eta ~ 1,
                            brms::nlf(bias ~ beta), beta ~ 1,
                            brms::nlf(bs ~ alpha), alpha ~ 1,
                            brms::nlf(ndt ~ tau), tau ~ 1
            ),
            par_transform =
              list(eta ~ inv_logit(eta), alpha ~ exp(alpha),
                   tau ~ vec_prod(inv_logit(tau), (min_rt - rtbound)) + rtbound),
            doc_helper = list(
              title = paste0('RLDDM model with single learning rate'),
              formula_arg = '',
              references = c(
                paste0('Pedersen ML, Frank MJ, Biele G. 2017. ',
                       'The drift diffusion model as the choice rule ',
                       'in reinforcement learning. ',
                       'Psychonomic Bulletin & Review 24:1234–1251. ',
                       '\\url{https://doi.org/10.3758/s13423-016-1199-y}')
              ),
              update_formulas = c(
                paste0('\\deqn{Q_{a,t+1} = Q_{a,t} + ',
                       '\\alpha \\times (R_{t} - Q_{a,t})}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       '\\mathrm{RT}_{a_{i}} \\sim ',
                       '\\begin{cases} ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, \\beta, \\delta) & ',
                       '\\text{if choice } c_{i} = 1 \\text{ (upper bound)}',
                       ' \\\\ ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, 1-\\beta, -\\delta) & ',
                       '\\text{if choice } c_{i} = 0 \\text{ (lower bound)}',
                       '\\end{cases}',
                       '}')
              )
            )
          ),
        qlddm_2a =
          new_model_spec(
            class = 'qlddm_2a',
            description = paste0('Reinforcement leanrining drift diffusion ',
                                 'model with ',
                                 'separate learning rates \\eqn{\\eta_{pos}}',
                                 'for positive and \\eqn{eta_{neg}} for',
                                 'negative prediction errors.',
                                 'The scaled difference of Q-values is ',
                                 'mapped to the drift rate of ',
                                 'the Wiener distribution.'),
            parameters = list(
              delta = list(lb=-Inf, ub = Inf,
                           desc = paste0('Slope of the random walk ',
                                         '(RL model parameter)')),
              etapos = list(lb = 0, ub = 1,
                            desc = paste0('Learning rate after receiving ',
                                          'a positive outcome')),
              etaneg = list(lb = 0, ub = 1,
                            desc = paste0('Learning rate after receiving ',
                                          'a negative outcome')),
              nu = list(lb=-Inf, ub = Inf,
                        desc = paste0('Scale factor for mapping the ',
                                      'difference of Q-values ',
                                      '(\\eqn{Q_{upper} - Q_{lower}}) ',
                                      'to the drift rate \\eqn{\\delta}')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Non-descision time')),
              alpha = list(lb = 0, ub = Inf,
                           desc = paste0('Boundary separation')),
              beta = list(lb = 0, ub = 1,
                          desc = paste0('Bias'))
            ),
            resp_var = 'rt',
            dec_var = 'choice',
            pred_vars = c('reward'),
            fixed_vars = list(
              rtbound = .1,
              beta = .5
            ),
            func_name = 'qlddm_2a_qdiff_nu',
            func_stanvar = qlddm_2a_qdiff_nu_func,
            func_input = c('blockgrp', 'choice', 'reward',
                           'etapos', 'etaneg', 'nu'),
            func_params = c('etapos', 'etaneg', 'nu'),
            func_par = 'delta',
            family =  brms::wiener(link = 'identity',
                                   link_bs = 'identity',
                                   link_ndt = 'identity',
                                   link_bias = 'identity'),
            par_form = list(delta ~ 1, nu ~ 1,
                            etapos ~ 1, etaneg ~ 1,
                            brms::nlf(bias ~ beta), beta ~ 1,
                            brms::nlf(bs ~ alpha), alpha ~ 1,
                            brms::nlf(ndt ~ tau), tau ~ 1
            ),
            par_transform =
              list(etapos + etaneg ~ inv_logit(x), alpha ~ exp(alpha),
                   tau ~ vec_prod(inv_logit(tau), (min_rt - rtbound)) + rtbound),
            doc_helper = list(
              title = paste0('RLDDM model with dual learning rate'),
              formula_arg = '',
              references = c(
                paste0('Pedersen ML, Frank MJ, Biele G. 2017. ',
                       'The drift diffusion model as the choice rule ',
                       'in reinforcement learning. ',
                       'Psychonomic Bulletin & Review 24:1234–1251. ',
                       '\\url{https://doi.org/10.3758/s13423-016-1199-y}')
              ),
              update_formulas = c(
                paste0('\\deqn{Q_{a,t+1} = Q_{a,t} + ',
                       '\\eta \\times (R_{t} - Q_{a,t})',
                       ',\\quad \\text{where } \\eta = \\begin{cases} ',
                       '\\eta_{pos} & \\text{if } R_{t} \\geq 0',
                       ' \\\\ ',
                       '\\eta_{neg} & \\text{if } R_{t} \\lt 0',
                       '\\end{cases}',
                       '}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       '\\mathrm{RT}_{a_{i}} \\sim ',
                       '\\begin{cases} ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, \\beta, \\delta) & ',
                       '\\text{if choice } c_{i} = 1 \\text{ (upper bound)}',
                       ' \\\\ ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, 1-\\beta, -\\delta) & ',
                       '\\text{if choice } c_{i} = 0 \\text{ (lower bound)}',
                       '\\end{cases}',
                       '}')
              )
            )
          ),
        qlddm_a_rho =
          new_model_spec(
            class = 'qlddm_a_rho',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'learning rate \\eqn{\\eta} and reward ',
                                 'reward sensitivity \\eqn{\\rho}.',
                                 'The scaled difference of Q-values is ',
                                 'mapped to the drift rate of ',
                                 'the Wiener distribution.'),
            parameters = list(
              delta = list(lb=-Inf, ub = Inf,
                           desc = paste0('Slope of the random walk ',
                                         '(RL model parameter)')),
              eta = list(lb = 0, ub = 1,
                         desc = paste0('Learning rate')),
              rho = list(lb = -Inf, ub = Inf,
                         desc = paste0('Reward sensitivity')),
              nu = list(lb=-Inf, ub = Inf,
                        desc = paste0('Scale factor for mapping the ',
                                      'difference of Q-values ',
                                      '(\\eqn{Q_{ub} - Q_{lb}}) ',
                                      'to the drift rate \\eqn{\\delta}')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Non-descision time')),
              alpha = list(lb = 0, ub = Inf,
                           desc = paste0('Boundary separation')),
              beta = list(lb = 0, ub = 1,
                          desc = paste0('Bias'))
            ),
            resp_var = 'rt',
            dec_var = 'choice',
            pred_vars = c('reward'),
            fixed_vars = list(
              rtbound = .1,
              beta = .5
            ),
            func_name = 'qlddm_a_rho_qdiff_nu',
            func_stanvar = qlddm_a_rho_qdiff_nu_func,
            func_input = c('blockgrp', 'choice', 'reward', 'eta', 'rho', 'nu'),
            func_data = c('choice', 'reward'),
            func_params = c('eta', 'rho', 'nu'),
            func_par = 'delta',
            family =  brms::wiener(link = 'identity',
                                   link_bs = 'identity',
                                   link_ndt = 'identity',
                                   link_bias = 'identity'),
            par_form = list(delta ~ 1, nu ~ 1,
                            eta ~ 1, rho ~ 1,
                            brms::nlf(bias ~ beta), beta ~ 1,
                            brms::nlf(bs ~ alpha), alpha ~ 1,
                            brms::nlf(ndt ~ tau), tau ~ 1
            ),
            par_transform =
              list(eta ~ inv_logit(eta), rho ~ inv_logit(rho) * 20,
                   alpha ~ exp(alpha),
                   tau ~ vec_prod(inv_logit(tau), (min_rt - rtbound)) + rtbound),
            doc_helper = list(
              title = paste0('RLDDM model with single learning rate and ',
                             'reward sensitivity'),
              formula_arg = '',
              references = c(
                paste0('Pedersen ML, Frank MJ, Biele G. 2017. ',
                       'The drift diffusion model as the choice rule ',
                       'in reinforcement learning. ',
                       'Psychonomic Bulletin & Review 24:1234–1251. ',
                       '\\url{https://doi.org/10.3758/s13423-016-1199-y}')
              ),
              update_formulas = c(
                paste0('\\deqn{Q_{a,t+1} = Q_{a,t} + ',
                       '\\alpha \\times (\\rho R_{t} - Q_{a,t})}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{',
                       '\\mathrm{RT}_{a_{i}} \\sim ',
                       '\\begin{cases} ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, \\beta, \\delta) & ',
                       '\\text{if choice } c_{i} = 1 \\text{ (upper bound)}',
                       ' \\\\ ',
                       '\\mathrm{Wiener}(\\alpha, \\tau, 1-\\beta, -\\delta) & ',
                       '\\text{if choice } c_{i} = 0 \\text{ (lower bound)}',
                       '\\end{cases}',
                       '}')
              )
            )
          ),
        ql_a_it_blm =
          new_model_spec(
            class = 'ql_a_it',
            description = paste0('Rescorla-Wagner delta learning model with ',
                                 'single learning rate and inverse temperature'),
            parameters = list(
              alpha = list(lb = 0, ub = 1,
                           desc = paste0('Leanring rate')),
              tau = list(lb = 0, ub = Inf,
                         desc = paste0('Inverse temperature'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = NULL,
            func_name = 'ql_a_it_probs',
            func_stanvar = ql_a_it_blm_probs_func,
            func_input = c('choice', 'reward', 'alpha', 'tau'),
            par_transform =
              list(alpha ~ inv_logit(alpha),
                   tau ~ inv_logit(tau) * 5.0),
            doc_helper = list(
              title = paste0('RL model with learning rate and ',
                             'inverse temperature'),
              formula_arg = '',
              references = c(
                paste0('Watkins, C. J. C. H., & Dayan, P. (1992). ',
                       'Q-learning. *Machine Learning*, 8(3), 279–292. ',
                       '\\url{https://doi.org/10.1007/BF00992698}')
              ),
              update_formulas = c(
                paste0('\\deqn{Q_{a,t+1} = Q_{a,t} + ',
                       '\\alpha \\times (R_{t} - Q_{a,t})}')
              )
              ,
              link_formula = c(
                paste0('\\deqn{p(a_{i}) = \\frac{e^{\\tau Q_{a_{i}}}}',
                       '{\\sum_{j=1}^{K} e^{\\tau Q_{a_{j}}}}}')
              )
            ),
            use_blm = T
          ),
        hmm_rp_blm =
          new_model_spec(
            class = 'hmm_rp',
            description = paste0('HMM model with separate emission probabilities ',
                                 'for reward and punishment outcomes ',
                                 '(cf. Schlagenhauf et al. 2014)'),
            parameters = list(
              gamma = list(lb = 0, ub = 1,
                           desc = paste0('Transition probability')),
              c = list(lb = 0.5, ub = 1,
                       desc = paste0('Emission probability for rewards')),
              d = list(lb = 0.5, ub = 1,
                       desc = paste0('Emission probability for rewards'))
            ),
            resp_var = 'choice',
            pred_vars = c('reward'),
            block_var = NULL,
            func_name = 'hmm_rp_probs',
            func_stanvar = hmm_rp_blm_probs_func,
            func_input = c('choice', 'reward', 'gamma', 'c', 'd'),
            par_transform =
              list(gamma ~ inv_logit(gamma),
                   c + d ~ inv_logit(x) * 0.5 + 0.5)
          )

      )
  }

#' Get currently available model specifications
#'
#' @returns a `list()` of model specifications for blms models
#' @export
get_all_model_specs <-
  function() {
    if (is.null(ms_env$all_model_specs)) {
      ms_env$all_model_specs <-
        create_all_model_specs()
    }
    return(ms_env$all_model_specs)
  }

#' Get model specification for a specific model class
#'
#' @param class character giving the name of the model class
#'
#' @returns a model specification for a blms model
#' @export
get_model_spec <-
  function(class) {
    model_specs <- get_all_model_specs()
    if(!class%in%names(model_specs)) {
      stop('Invalid model class \'', class, '\' ',
           '(not found in get_all_model_specs())')
    }
    model_spec <- model_specs[[class]]
    return(model_spec)
  }

#' Get the names of the parameters fitted by a blms model of a specific class
#'
#' @param model_spec model specification as returned by `get_model_spec()` (see
#'        `get_all_model_specs()` for a list of currently defined models)
#'
#' @returns character vector containing the names of the parameters fitted by
#'          a blms model of a specific class
#' @export
get_parameter_names <-
  function(model_spec) {
    names(model_spec$parameters)
  }
