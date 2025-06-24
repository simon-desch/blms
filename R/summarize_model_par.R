#' Extract Symbols from a Formula Expression
#'
#' Recursively extracts all variable names (symbols) from a formula or expression.
#'
#' @param expr A formula or R expression.
#'
#' @return A character vector of variable names.
#' @keywords internal
#' @export
extract_symbols <- function(expr) {
  if (is.null(expr)) return(character())
  if (inherits(expr, "formula")) expr <- expr[[3]]
  unique(all.names(expr))
}

#' Remove Group-Level Terms (Bar Terms) from a Formula
#'
#' Removes `(1 | group)` and similar random effects terms from a formula.
#'
#' @param expr A formula or language object.
#'
#' @return A modified formula or expression without bar terms.
#' @keywords internal
#' @export
remove_bar_terms <- function(expr) {
  if (inherits(expr, "formula")) {
    expr[[2+is.twosided(expr)]] <- remove_bar_terms(rhs(expr))
    return(expr)
  }
  math_operators <- c("+", "-", "*", "/", "^", ":", "|")#, "~", "$", "[", "[[")
  if (!is.call(expr)) return(expr)
  if (expr[[1]] == as.name("(")){
    nested_calls <- expr[-1][sapply(expr[-1], is.call)]
    if(length(nested_calls)) {
      if(any(sapply(nested_calls, \(x)x[[1]]==as.name('|')))) {
        return(NULL)
      }
    }
  }
  # if (expr[[1]] == as.name("|")) return(NULL)
  expr[] <- lapply(expr, remove_bar_terms)
  expr <- expr[!vapply(expr, is.null, logical(1))]
  if (deparse1(expr[[1]]) %in%math_operators && length(expr)==2)
    return(expr[[2]])
  expr
}

#' Get All Predictors for a Nonlinear Parameter
#'
#' Recursively identifies all variables (excluding group-level terms) needed to compute a specific nonlinear parameter.
#'
#' @param nlpar_name Name of the nonlinear parameter.
#' @param formula_obj A brms formula object.
#' @param data_vars Character vector of variable names in the data.
#' @param seen Internal recursion tracker.
#'
#' @return A character vector of required variable names.
#' @keywords internal
#' @export
get_nlpar_predictors <-
  function(nlpar_name, formula_obj, data_vars, seen = character()) {
    if (nlpar_name %in% seen) return(character())
    seen <- c(seen, nlpar_name)
    required <- character()

    if (nlpar_name %in% names(formula_obj$pforms)) {
      nl_formula <- formula_obj$pforms[[nlpar_name]]

      # Check and warn if bar terms are present
      # if (length(lme4::findbars(nl_formula)) > 0) {
      #   warning("Group-level terms found in nonlinear formula for '", nlpar_name,
      #           "'. These will be ignored. Use `re_formula` to include them explicitly.")
      # }

      clean_expr <- remove_bar_terms(nl_formula)
      rhs_syms <- extract_symbols(clean_expr)
      # rhs_syms <- find_bar_terms(nl_formula, invert = T)

      for (sym in rhs_syms) {
        if (sym %in% names(formula_obj$pforms)) {
          required <- c(required, Recall(sym, formula_obj, data_vars, seen))
        } else if (sym %in% data_vars) {
          required <- c(required, sym)
        }
      }
    }

    unique(required)
  }

#' Get All Predictors Used in a brms Model
#'
#' Identifies all data-level variables (excluding bar terms) needed by any nonlinear parameter.
#'
#' @param formula_obj A brms formula object.
#' @param data_vars Variable names in the original dataset.
#'
#' @return A character vector of predictor names.
#' @keywords internal
#' @export
get_all_model_predictors <- function(formula_obj, data_vars) {
  # rhs_syms <- extract_symbols(remove_bar_terms(formula_obj$formula))
  required <- character()
#
#   for (sym in rhs_syms) {
#     if (sym %in% names(formula_obj$pforms)) {
#       required <- c(required, get_nlpar_predictors(sym, formula_obj, data_vars))
#     } else if (sym %in% data_vars) {
#       required <- c(required, sym)
#     }
#   }
  required <- all.vars(brms::brmsterms(formula_obj)$allvars)
  unique(required)
}

#' get nlpars used in other nlpar formula
#'
#' @param formula `brmsformula`
#' @param nlpar `nlpar` for which used nlpars should be return
#'
#' @returns character vector
#' @export
get_used_nlpars <-
  function(formula, nlpar) {
    # ToDo: recurse into nlpars
    bts <- brms::brmsterms(formula)
    if ('used_nlpars'%in%names(bts$nlpars[[nlpar]]))
      return(bts$nlpars[[nlpar]][['used_nlpars']])
    return(NULL)
  }

#' Get all variables from group-level effects of all formulas in brmsformula
#'
#' @param formula (non-linear) `brmsformula`
#' @param nlpar list of non-linear parameters to obtain re vars for
#' @param group return grouping variables for selected `nlpar`s
#' @param effect return effect variables for selected `nlpar`s
#'
#' @returns character vector of variable names used in group-level terms
#' @export
get_all_re_vars <-
  function(formula, nlpar = NULL, group = T, effect = T) {
    bts <- brms::brmsterms(formula)
    requested_nlpars <- names(bts$nlpars)
    if (!is.null(nlpar)) {
      nlpars_oi <- intersect(nlpar, c(names(bts$nlpars)))
      requested_nlpars <- c(nlpars_oi,
                            unlist(sapply(nlpars_oi,
                                          \(x) get_used_nlpars(formula, x)))
      )
    }
    # msg_var(requested_nlpars)
    if (length(requested_nlpars)) {
      # msg_var(bts$nlpars[requested_nlpars])
      re_vars <-
        sapply(lapply(bts$nlpars[requested_nlpars], '[[', 're') ,
               \(x) c(if (T) x$group else NULL,
                      if (T) sapply(x$form, all.vars) else NULL),
               # \(x) c(if (group) x$group else NULL,
               #        if (effect) sapply(x$form, all.vars) else NULL),
               simplify = T)
      re_vars <- unique(unlist(re_vars))
      return(re_vars)
    }
    return(NULL)
  }

#' Try to copy classes of variables from another data.frame
#'
#' @param df `data.frame` to copy classes of variables to
#' @param df2 `data.frame` to take variable classes from
#'
#' @returns `df` with variables found in `df2` coerced to their class
#' @export
df_copy_classes <-
  function(df, df2, unlist = T) {
    for (var in names(df)) {
      if(!var%in%names(df2)) next
      if (unlist && is.list(df[[var]]) && !is.list(df2[[var]]))
        df[[var]] <- unlist(df[[var]])
      # message('converting ', var, ' from ', class(df[[var]]))
      if (is.factor(df2[[var]])) {
        # message(var,' is factor with levels ', toString(levels(df2[[var]])))
        df[[var]] <- factor(df[[var]], levels = levels(df2[[var]]))
      } else if (is.character(df2[[var]])) {
        # message(var,' is character with unique values ',
        # toString(unique(df2[[var]])))
        df[[var]] <- as.character(df[[var]])
      } else if (is.integer(df2[[var]])) {
        # message(var,' is integer with ', length(unique(df2[[var]])),
        # ' unique values  ', toString(head(unique(df2[[var]]), 10)))
        df[[var]] <- as.integer(df[[var]])
      } else if (is.numeric(df2[[var]])) {
        # message(var,' is numeric with ', length(unique(df2[[var]])),
        #         ' unique values  ', toString(head(unique(df2[[var]]), 10)))
        df[[var]] <- as.numeric(df[[var]])
      } else {

        message(var,' is ', class(df2[[var]]), ' with ', length(unique(df2[[var]])),
                ' unique values  ', toString(head(unique(df2[[var]]), 10)))
      }
      # message(class(df[[var]]))
    }
    return(df)
  }

#' Average across factors in a data.frame
#'
#' @param df a `data.frame`
#' @param varname the variable to be averaged
#' @param collapse_factors the variables to average across
#' @param weight
#'
#' @returns `data.frame` in which the variable named by `varname` was averaged
#'          across levels of
#' @export
average_over <- function(df, varname, collapse_factors, weight = NULL, auto_weight = TRUE) {
  # Identify grouping variables
  group_vars <- setdiff(names(df), c(varname, collapse_factors))

  # Calculate weights
  if (!is.null(weight)) {
    weights <- df[[weight]]
  } else if (auto_weight && length(collapse_factors) > 0) {
    grp_collapse <- interaction(df[collapse_factors], drop = TRUE)
    weights <- ave(rep(1, nrow(df)), grp_collapse, FUN = length)
  } else {
    weights <- rep(1, nrow(df))
  }

  # If no grouping variables, return full-data weighted mean
  if (length(group_vars) == 0) {
    return(data.frame(weighted_mean = weighted.mean(df[[varname]], weights)))
  }

  # Generate grouping ID for the group_vars
  grp_id <- interaction(df[group_vars], drop = TRUE, sep = '__')
  # grp_id2 <- interaction(df[group_vars], drop = TRUE, sep = '__', lex.order = T)
  # grp_id <- interaction(df[, group_vars], drop = TRUE, sep = '__')

  # Use rowsum to get sum of values and weights per group
  value_sum <- rowsum(df[[varname]] * weights, grp_id)
  weight_sum <- rowsum(weights, grp_id)

  # Compute weighted means
  weighted_means <- value_sum / weight_sum

  # Recover group keys from row names
  group_levels <- strsplit(rownames(weighted_means), split = '__')
  group_df <- as.data.frame(do.call(rbind, group_levels), stringsAsFactors = FALSE)
  # msg_var(group_df)
  # msg_var(group_vars)
  names(group_df) <- group_vars

  # Combine and return
  result <- cbind(group_df, weighted_mean = as.numeric(weighted_means))
  names(result)[ncol(result)] <- varname
  # msg_var(names(result))
  # msg_var(setdiff(names(df), collapse_factors))
  result <- result[, setdiff(names(df), collapse_factors)]
  result <- df_copy_classes(result, df)
  rownames(result) <- NULL
  return(result)
}


#' Get prediction grid for posterior draws of a non-linear parameter
#'
#' @param fit A `brmsfit`object or inheritor thereof
#' @param nlpar The non-linear parameter for which draws should be obtained
#' @param by Either one-sided formula specifying predictors of the non-linear
#'        parameter that should be included, or a character vector of names of
#'        those predictors. For factors, draws will be obtained separately for
#'        each level, for continuous variables, by default, draws will be
#'        obtained for mean-sd, mean, and mean+sd (can be adjusted/replaced by
#'        defining specific values for the predictors in `predictor_values`).
#'        For variables not named here (or in `predictor_values`), if they
#'        are factors, draws will be averaged across all levels, for
#'        continuous variables only the mean will be used to obtain draws.
#' @param re_formula formula for group-level effects to be included
#' @param predictor_values a named `list()` with names corresponding to
#'        predictor variables of the non-linear parameter, and their values
#'        values of the predictor variable for which draws should be obtained
#'
#' @returns A `data.frame` expanding combinations of variables as defined by
#'          the inputs. The main purpose is to use this to pbtain some kind of
#'          expected or predicted draws.
#' @export
#'
get_nlpar_pred_grid <- function(
    fit,
    nlpar,
    by = ~ 1,
    re_formula = NA,
    predictor_values = list()
) {

  if (!inherits(fit, 'brmsfit')) {
    stop('Need brmsfit object to plot model pars')
  }
  model_is_empty <- !length(fit$fit@sim) || isTRUE(fit$fit@sim$iter <=
                                                     fit$fit@sim$warmup)
  if (model_is_empty) {
    stop('brmsfit object does not contain draws')
  }

  # ToDo: validate nlpar!!!
  # msg_var(nlpar)
  # msg_var(by)
  # msg_var(method)
  # msg_var(re_formula)
  # msg_var(predictor_values)

  model_formula <- fit$formula
  data <- fit$data
  data_vars <- names(data)
  # msg_var(data_vars)

  if (length(lme4::findbars(formula)) > 0) {
    warning('Group-level terms found in formula. ',
            'These will be ignored. ',
            'Use `re_formula` to include them explicitly.')
    formula <- remove_bar_terms(formula)
  }

  # All predictors used in model and in the nonlinear system
  nlpar_predictors <- get_nlpar_predictors(nlpar, model_formula, data_vars)
  # msg_var(nlpar_predictors)
  model_predictors <- get_all_model_predictors(model_formula, data_vars)
  # msg_var(model_predictors)
  all_valid_predictors <- union(nlpar_predictors, model_predictors)
  additional_model_predictors <- setdiff(model_predictors, nlpar_predictors)

  # Variables requested in formula
  input_vars <- if (is.formula(by)) all.vars(by) else by
  # msg_var(input_vars)
  if(!is.character(input_vars)) {
    stop('Invalid definition of by= as ', class(input_vars))
  }
  if (!all(input_vars %in% all_valid_predictors)) {
    stop('Some formula variables are not found among predictors.\n',
         'Allowed: ', paste(all_valid_predictors, collapse = ", "), '\n',
         'Additional: ',
         paste(input_vars[!input_vars %in% all_valid_predictors]))
  }
  block_grp <- fit$blms_formula$model_spec$block_var
  # msg_var(block_grp)
  grand_mean_predictors_fixed <-
    setdiff(nlpar_predictors, c(input_vars, block_grp))
  # msg_var(grand_mean_predictors_fixed)


  # Predictor grid construction
  missing_vars <- setdiff(all_valid_predictors, input_vars)
  all_predictors <- union(input_vars, missing_vars)

  model_re_vars_grp <-
    get_all_re_vars(model_formula, nlpar = nlpar, effect = F)
  model_re_vars_eff <-
    get_all_re_vars(model_formula, nlpar = nlpar, group = F)
  model_re_vars <-
    unique(c(model_re_vars_grp, model_re_vars_eff))
  # msg_var(model_re_vars)


  # ToDo: check whether NULL (& NA) are handled correctly (should now..)
  re_vars <- if (is.null(re_formula)) model_re_vars else all.vars(re_formula)
  # msg_var(re_vars)
  undefined_re_vars <- setdiff(re_vars, model_re_vars)
  if(length(undefined_re_vars)) {
    stop('Some group-level variables are not found in model.\n',
         'Allowed: ', toString(model_re_vars), '\n',
         'Additional: ', toString(undefined_re_vars))
  }
  all_predictors <- union(all_predictors, re_vars)
  # msg_var(all_predictors)


  values_list <- list()
  average_factors <- c()
  for (var in all_predictors) {
    # message('var: ', var)
    if (!var %in% names(data)) stop("Variable '", var, "' not found in data")
    #print(head(data))
    x <- data[[var]]
    # msg_var(x)

    if (!is.null(predictor_values[[var]])) {
      values_list[[var]] <- predictor_values[[var]]
    } else if (var %in% input_vars) {
      values_list[[var]] <- if (is.factor(x)) levels(x)
      else if (is.character(x)) unique(x)
      else if (var %in% model_re_vars_grp)
        levels(factor(x))
      else if (is.numeric(x))
        round(c(mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
                mean(x, na.rm = TRUE),
                mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)), 2)
      else stop("Unsupported type for input var: ", var)

    } else if (var %in% re_vars) {
      values_list[[var]] <- if (is.factor(x)) levels(x)
      else if (is.character(x)) unique(x)
      else if (var %in% model_re_vars_grp)
        levels(factor(x))
      else if (is.numeric(x))
        mean(x, na.rm = TRUE)
      else stop("Unsupported type for input var: ", var)
      if(var%in%grand_mean_predictors_fixed) {
        # message('adding re_var ', var, ' to average_factors ',
        #         'because it is in grand_mean_predictors_fixed')
        average_factors <- c(average_factors, var)
      }
    } else if (var==block_grp) {
      # message('is blockgrp')
      values_list[[var]] <- 1
    }else if (var %in% grand_mean_predictors_fixed) {
      # message('grand_mean_predictors_fixed')
      values_list[[var]] <- NULL
      if (is.factor(x)) {
        fctr_contrs <- contrasts(x)
        if (all(colSums(fctr_contrs)==0)) {
          values_list[[var]] <- NA
        } else {
          values_list[[var]] <- levels(x)
          # message('adding factor() variable ', var, ' to average_factors ',
          #         'because it is in grand_mean_predictors_fixed')
          average_factors <- c(average_factors, var)
        }
      } else if (is.character(x)) {
        default_contr <- options('contrasts')[['contrasts']][['unordered']]
        if (default_contr=='contr.sum') {
          values_list[[var]] <- NA
        } else {
          values_list[[var]] <- unique(x)
          # message('adding character() variable', var, ' to average_factors ',
          #         'because it is in grand_mean_predictors_fixed')
          average_factors <- c(average_factors, var)
        }
      } else if (is.numeric(x)) {
        values_list[[var]] <- mean(x, na.rm = TRUE)
      }
      if(is.null(values_list[[var]]))
        stop("Unsupported type for fallback var: ", var)
    }  else if (var%in%additional_model_predictors) {
      values_list[[var]] <-
        if (is.factor(x)) levels(x)[1]
      else if (is.character(x)) unique(x)[1]
      else if (is.numeric(x)) round(x[which(!is.na(x))[1]], 2)
      else stop("Unsupported type for fallback var: ", var)
    } else {
      stop('Unsupported gridding for var: ', var, '\n',
           'Where did you find this variable???')
    }
  }

  # msg_var(values_list)
  new_data <- expand.grid(values_list, stringsAsFactors = FALSE)

  # Retype to match original
  new_data <- df_copy_classes(new_data, data)
  class(new_data) <- c('nlpar_pred_grid', class(new_data))
  attr(new_data, 'average_factors') <- average_factors
  # msg_var(new_data)

  return(new_data)
}


#' Get predicted draws for non-linear parameters
#'
#' @param fit A `brmsfit`object or inheritor thereof
#' @param nlpar The non-linear parameter for which draws should be obtained
#' @param by Either one-sided formula specifying predictors of the non-linear
#'        parameter that should be included, or a character vector of names of
#'        those predictors. For factors, draws will be obtained separately for
#'        each level, for continuous variables, by default, draws will be
#'        obtained for mean-sd, mean, and mean+sd (can be adjusted/replaced by
#'        defining specific values for the predictors in `predictor_values`).
#'        For variables not named here (or in `predictor_values`), if they
#'        are factors, draws will be averaged across all levels, for
#'        continuous variables only the mean will be used to obtain draws.
#' @param method `'epred'` and `'linpred'` should be the same for non-linear
#'        parameters and account for uncertainty of the fixed coefficients and
#'        uncertainty of the variance parameters of the groups. `'predict'`
#'        additionally accounts for uncertainty for each individual observation
#'        (e.g., observational-level residual variance)
#' @param re_formula formula for group-level effects to be included
#' @param predictor_values a named `list()` with names corresponding to
#'        predictor variables of the non-linear parameter, and their values
#'        values of the predictor variable for which draws should be obtained
#'
#' @returns `data.frame` with columns for the predictor variables containing
#'          the values used to obtain draws and a column named after the
#'          non-linear parameter respective
#' @export

get_nlpar_draws <- function(
    fit,
    nlpar,
    by = ~ 1,
    predictor_values = list(),
    re_formula = NA,
    method = c('epred', 'linpred', 'predict'),
    new_data = NA
) {

  # message('get_nlpar_draws: ', nlpar)
  if (!inherits(fit, 'brmsfit')) {
    stop('Need brmsfit object to plot model pars')
  }
  model_is_empty <- !length(fit$fit@sim) || isTRUE(fit$fit@sim$iter <=
                                                     fit$fit@sim$warmup)
  if (model_is_empty) {
    stop('brmsfit object does not contain draws')
  }
  method <- match.arg(method)

  if (is.na(new_data)) {
    new_data <-
      get_nlpar_pred_grid(fit, nlpar, by = by,
                          predictor_values = predictor_values,
                          re_formula = re_formula)
  }

  if (F) {
  # ToDo: validate nlpar!!!
  msg_var(nlpar)
  msg_var(by)
  msg_var(method)
  msg_var(re_formula)
  msg_var(predictor_values)

  model_formula <- fit$formula
  data <- fit$data
  data_vars <- names(data)

  if (length(lme4::findbars(formula)) > 0) {
    warning('Group-level terms found in formula. ',
            'These will be ignored. ',
            'Use `re_formula` to include them explicitly.')
    formula <- remove_bar_terms(formula)
  }

  # All predictors used in model and in the nonlinear system
  nlpar_predictors <- get_nlpar_predictors(nlpar, model_formula, data_vars)
  msg_var(nlpar_predictors)
  model_predictors <- get_all_model_predictors(model_formula, data_vars)
  msg_var(nlpar_predictors)
  all_valid_predictors <- union(nlpar_predictors, model_predictors)
  additional_model_predictors <- setdiff(model_predictors, nlpar_predictors)

  # Variables requested in formula
  input_vars <- if (is.formula(by)) all.vars(by) else by
  msg_var(input_vars)
  if(!is.character(input_vars)) {
    stop('Invalid definition of by= as ', class(input_vars))
  }
  if (!all(input_vars %in% all_valid_predictors)) {
    stop('Some formula variables are not found among predictors.\n',
         'Allowed: ', paste(all_valid_predictors, collapse = ", "), '\n',
         'Additional: ',
         paste(input_vars[!input_vars %in% all_valid_predictors]))
  }
  grand_mean_predictors_fixed <- setdiff(nlpar_predictors, input_vars)

  # Predictor grid construction
  missing_vars <- setdiff(all_valid_predictors, input_vars)
  all_predictors <- union(input_vars, missing_vars)

  model_re_vars_grp <-
    get_all_re_vars(model_formula, nlpar = nlpar, effect = F)
  model_re_vars_eff <-
    get_all_re_vars(model_formula, nlpar = nlpar, group = F)
  model_re_vars <-
    unique(c(model_re_vars_grp, model_re_vars_eff))
  msg_var(model_re_vars)


  # ToDo: check whether NULL (& NA) are handled correctly (should now..)
  re_vars <- if (is.null(re_formula)) model_re_vars else all.vars(re_formula)
  msg_var(re_vars)
  undefined_re_vars <- setdiff(re_vars, model_re_vars)
  if(length(undefined_re_vars)) {
    stop('Some group-level variables are not found in model.\n',
         'Allowed: ', toString(model_re_vars), '\n',
         'Additional: ', toString(undefined_re_vars))
  }
  all_predictors <- union(all_predictors, re_vars)


  values_list <- list()
  average_factors <- c()
  for (var in all_predictors) {
    if (!var %in% names(data)) stop("Variable '", var, "' not found in data")

    x <- data[[var]]

    if (!is.null(predictor_values[[var]])) {
      values_list[[var]] <- predictor_values[[var]]
    } else if (var %in% input_vars) {
      values_list[[var]] <- if (is.factor(x)) levels(x)
      else if (is.character(x)) unique(x)
      else if (var %in% model_re_vars_grp)
        levels(factor(x))
      else if (is.numeric(x))
        round(c(mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
                mean(x, na.rm = TRUE),
                mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)), 2)
      else stop("Unsupported type for input var: ", var)

    } else if (var %in% re_vars) {
      values_list[[var]] <- if (is.factor(x)) levels(x)
      else if (is.character(x)) unique(x)
      else if (var %in% model_re_vars_grp)
        levels(factor(x))
      else if (is.numeric(x))
        mean(x, na.rm = TRUE)
      else stop("Unsupported type for input var: ", var)
      if(var%in%grand_mean_predictors_fixed) average_factors <- c(average_factors, var)
    } else if (var %in% grand_mean_predictors_fixed) {
      values_list[[var]] <- NULL
      if (is.factor(x)) {
        fctr_contrs <- contrasts(x)
        if (all(colSums(fctr_contrs)==0)) {
          values_list[[var]] <- NA
        } else {
          values_list[[var]] <- levels(x)
          average_factors <- c(average_factors, var)
        }
      } else if (is.character(x)) {
        default_contr <- options('contrasts')[['contrasts']][['unordered']]
        if (default_contr=='contr.sum') {
          values_list[[var]] <- NA
        } else {
          values_list[[var]] <- unique(x)
          average_factors <- c(average_factors, var)
        }
      } else if (is.numeric(x)) {
        values_list[[var]] <- mean(x, na.rm = TRUE)
      }
      if(is.null(values_list[[var]]))
        stop("Unsupported type for fallback var: ", var)
    } else if (var%in%additional_model_predictors) {
      values_list[[var]] <-
        if (is.factor(x)) levels(x)[1]
      else if (is.character(x)) unique(x)[1]
      else if (is.numeric(x)) round(x[which(!is.na(x))[1]], 2)
      else stop("Unsupported type for fallback var: ", var)
    } else {
      stop('Unsupported gridding for var: ', var, '\n',
           'Where did you find this variable???')
    }
  }

  new_data <- expand.grid(values_list, stringsAsFactors = FALSE)

  # Retype to match original
  new_data <- df_copy_classes(new_data, data)

  }
  # Posterior predictions
  pred_fun <-
    if (method == "epred") tidybayes::epred_draws else
      if (method == 'linpred') tidybayes::linpred_draws else
        tidybayes::predicted_draws
  # message('new_data: ', toString(names(new_data)), ' (nrow: ', nrow(new_data), ')')
  # msg_var(new_data)
  # print(head(new_data))
  # message('re_formula: ', toString(re_formula))
  # message('nlpar: ', toString(nlpar))
  long_preds <-
    pred_fun(fit, newdata = new_data,
             re_formula = re_formula, nlpar = nlpar, value = nlpar)
  # keep .draw to perform averaging within each draws
  long_preds <-
    long_preds[, !names(long_preds)%in%c('.chain', '.iteration', '.row')]
  # msg_var(long_preds)

  average_factors <- NULL
  if(inherits(new_data, 'nlpar_pred_grid')) {
    average_factors <- attr(new_data, 'average_factors')
  }
  # msg_var(average_factors)
  average_factors_len <- length(average_factors)
  if (average_factors_len) {
    # message('Averaging across levels of factor',
    #         ifelse(average_factors_len>1, 's', ''), ': ',
    #         toString(average_factors))
    long_preds <-
      average_over(long_preds, nlpar, average_factors)
  }
  return(long_preds)
  }


#' Default method to summarise draws
#'
#' @param draws a `draws_df`
#' @param variables variables to summarize
#' @param probs quantiles to include in draws summary
#' @param robust use median and mad instead of mean and sd
#' @param mc_se include mean Monte-Carlo standard error
#'
#' @returns `data.frame` containing summaries of rquested variables
#' @importFrom posterior quantile2 subset_draws mcse_median mcse_mean rhat ess_bulk ess_tail summarise_draws
#' @export
default_draws_summary <-
  function(draws, variables, probs = c(.025, .975), robust = F, mc_se = F,
           ...) {
    # the following is merely a partly reimplementation of brms'
    # summary.brmsfit method to match the output seen in that function

    .quantile <- function(x, .probs = probs, ...) {
      qs <- posterior::quantile2(x, probs = .probs, ...)
      prob <- probs[2] - probs[1]
      names(qs) <- paste0(c("l-", "u-"), prob * 100, "% CI")
      return(qs)
    }
    draws <- posterior::subset_draws(draws, variable = variables)
    measures <- list()
    if (robust) {
      measures$Estimate <- median
      if (mc_se) {
        measures$MCSE <- posterior::mcse_median
      }
      measures$Est.Error <- mad
    }
    else {
      measures$Estimate <- mean
      if (mc_se) {
        measures$MCSE <- posterior::mcse_mean
      }
      measures$Est.Error <- sd
    }
    measures <-
      c(measures,
        list(quantiles = .quantile, Rhat = posterior::rhat,
             Bulk_ESS = posterior::ess_bulk, Tail_ESS = posterior::ess_tail))
    out <- do.call(posterior::summarize_draws, c(list(draws), measures))
    out <- as.data.frame(out)
    rownames(out) <- out$variable
    out$variable <- NULL
    return(out)
  }

#' Get summary of a non-linear parameter
#'
#' @param fit An object of class `brmsfit`
#' @param nlpar The name of the non-linear parameter
#' @param ...
#'
#' @returns A `data.frame` with summaries of the non-linear parameter
#' @export
summarize_model_par <-
  function(fit,
           nlpar,
           # method = c('epred', 'linpred', 'predict'),
           ...) {
    if (!inherits(fit, 'brmsfit')) {
      stop('Need brmsfit object to plot model pars')
    }
    model_is_empty <- !length(fit$fit@sim) || isTRUE(fit$fit@sim$iter <=
                                                       fit$fit@sim$warmup)
    if (model_is_empty) {
      stop('brmsfit object does not contain draws')
    }
    # method = match.arg(method)
    dots <- list(...)
    get_nlpar_draws_args <-
      c(list(fit, nlpar), #, method = method
        dots[names(dots)%in%c('by', 'method', 're_formula', 'predictor_values')])
    nlpar_draws <-
      do.call(get_nlpar_draws, get_nlpar_draws_args)

    # msg_var(nlpar_draws)
    # message('draws: ', toString(unique(nlpar_draws$.draw)))

    pop_vars <- NULL
    if('by'%in%names(dots)) {
      pop_vars <- if (is.formula(dots$by)) all.vars(dots$by) else dots$by
    }

    re_vars <- NULL
    if(all(c('re_formula', 'by_group_levels')%in%names(dots))) {
      if(isTRUE(dots$by_group_levels)) {
        re_formula <- dots$re_formula
        model_formula <- fit$formula
        model_re_vars_grp <-
          get_all_re_vars(model_formula, nlpar = nlpar, effect = F)
        model_re_vars_eff <-
          get_all_re_vars(model_formula, nlpar = nlpar, group = F)
        model_re_vars <-
          unique(c(model_re_vars_grp, model_re_vars_eff))
        # msg_var(model_re_vars)
        re_vars <-
          if (is.null(re_formula)) model_re_vars else all.vars(re_formula)
      }
    }

    # msg_var(pop_vars)
    # msg_var(re_vars)
    grp_vars <- unique(c(pop_vars, re_vars))
    # msg_var(grp_vars)

    sum_vars <- nlpar
    nlpar_draws_ <- nlpar_draws
    if (!is.null(grp_vars)) {
      nlpar_draws_ext <-
        nlpar_draws_
      value_columns <- c()
      for (var in grp_vars) {
        nlpar_draws_ext[[paste0(var, '_nmd')]] <-
          paste0(var, '_', nlpar_draws_ext[[var]])
        value_columns <- c(value_columns, paste0(var, '_nmd'))
      }
      nlpar_draws_ext$names_from <-
        as.character(interaction(nlpar_draws_ext[, value_columns], sep ='__'))
      nlpar_draws_ext <-
        nlpar_draws_ext[, !names(nlpar_draws_ext)%in%value_columns]
      nlpar_draws_ext <-
        nlpar_draws_ext[, !names(nlpar_draws_ext)%in%grp_vars]
      nlpar_draws_ext$new_col_names <-
        paste0(nlpar, '.', nlpar_draws_ext$names_from)
      sum_vars <-
        unique(nlpar_draws_ext$new_col_names)
      nlpar_draws_ext <-
        nlpar_draws_ext[, !names(nlpar_draws_ext)%in%'new_col_names']
      na_cols <-
        names(nlpar_draws_ext)[which(sapply(names(nlpar_draws_ext), \(x) all(is.na(nlpar_draws_ext[[x]]))))]
      # msg_var(na_cols)
      nlpar_draws_ext <-
        nlpar_draws_ext[, !names(nlpar_draws_ext)%in%na_cols]

      id_vars <-
        names(nlpar_draws_ext)[!names(nlpar_draws_ext)%in%c('names_from', nlpar)]
      # msg_var(nlpar_draws_ext)
      # msg_var(id_vars)
      nlpar_draws_ <- reshape(as.data.frame(nlpar_draws_ext),
                                  idvar = id_vars, #'.draw',
                                  timevar = 'names_from',
                                  v.names = c(nlpar),
                                  direction = 'wide'
                                  )
    }
      # message('nrows(nlpar_draws_): ', nrow(nlpar_draws_))
    # msg_var(nlpar_draws_)

    default_draws_summary_args <-
      c(list(draws = as_draws_df(nlpar_draws_), variables = sum_vars),
        dots[names(dots)%in%c('probs', 'robust', 'mc_se')])
    nlpar_draws_summary <-
      do.call(default_draws_summary, default_draws_summary_args)

    if (!is.null(grp_vars)) {
      # reconstruct grp_vars
      var_def <- sub(paste0('^', nlpar, '\\.'), '',
                     rownames(nlpar_draws_summary))
      grp_var_frame <-
        as.data.frame(t(as.data.frame(strsplit(var_def, '__'))))
      names(grp_var_frame) <- grp_vars
      for (var in grp_vars) {
        grp_var_frame[[var]] <-
          sub(paste0('^', var, '_'), '', grp_var_frame[[var]])
      }
      grp_var_frame <- df_copy_classes(grp_var_frame, nlpar_draws)
      rownames(grp_var_frame) <- NULL
      nlpar_draws_summary <- cbind(grp_var_frame, nlpar_draws_summary)

      rownames(nlpar_draws_summary) <-
        sub(paste0('^', nlpar, '\\.'), paste0(nlpar, '('),
            sub('__', ',', sub('$', ')', rownames(nlpar_draws_summary)))
            )
    }

    return(nlpar_draws_summary)

  }
