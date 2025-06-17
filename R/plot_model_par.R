
#' Plot Posterior Predictions for a Nonlinear Parameter in a `brms` Model
#'
#' This function visualizes the posterior predictions (or linear predictions) of a specified
#' non-linear parameter (`nlpar`) in a `brmsfit` model. It infers variables that affect the
#' `nlpar`, constructs a prediction grid, and plots posterior distributions using `ggdist`.
#'
#' @param fit A `brmsfit` model object.
#' @param nlpar A string indicating the name of the nonlinear parameter (e.g., `"gamma"`).
#' @param formula A formula specifying which predictor(s) to vary in the plot (e.g., `~ condition`).
#' @param method One of `"epred"` or `"linpred"` to specify which type of prediction to plot.
#' @param re_formula Re formula passed to `posterior_epred()` / `posterior_linpred()`. Use `NA` to exclude random effects.
#' @param predictor_values A named list providing custom values for predictors in the prediction grid.
#' @param width Numeric vector of credible intervals passed to `ggdist::stat_halfeye()`.
#'
#' @return A `ggplot` object showing the posterior distributions of the specified nonlinear parameter.
#'
#'
#' @import ggplot2
#' @importFrom ggdist stat_halfeye
# @importFrom tidyr pivot_longer
#' @importFrom rlang syms
#' @export
plot_model_par <- function(
    fit,
    nlpar,
    by,
    method = c('epred', 'linpred', 'predict'),
    re_formula = NA,
    predictor_values = list(),
    width = c(0.5, 0.8, 0.95)
) {

  if (!inherits(fit, 'brmsfit')) {
    stop('Need brmsfit object to plot model pars')
  }
  model_is_empty <- !length(fit$fit@sim) || isTRUE(fit$fit@sim$iter <=
                                                        fit$fit@sim$warmup)
  if (model_is_empty) {
    stop('brmsfit object does not contain draws')
  }

  method <- match.arg(method)

  input_vars <- if (is.formula(by)) all.vars(by) else by
  if(!is.character(input_vars)) {
    stop('Invalid definition of by= as ', class(input_vars))
  }

  long_preds <-
    get_nlpar_draws(
      fit = fit,
      nlpar = nlpar,
      by = by,
      method = method,
      re_formula = re_formula,
      predictor_values = predictor_values)

  if(F) {
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
  message('nlpar_predictors: ', toString(nlpar_predictors))
  model_predictors <- get_all_model_predictors(model_formula, data_vars)
  all_valid_predictors <- union(nlpar_predictors, model_predictors)
  additional_model_predictors <- setdiff(model_predictors, nlpar_predictors)

  # Variables requested in formula
  input_vars <- all.vars(formula)
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

  re_vars <- all.vars(re_formula)
  model_re_vars_grp <-
    get_all_re_vars(model_formula, nlpar = nlpar, effect = F)
  model_re_vars_eff <-
    get_all_re_vars(model_formula, nlpar = nlpar, group = F)
  model_re_vars <- c(model_re_vars_grp, model_re_vars_eff)
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
        values[[var]] <- mean(x, na.rm = TRUE)
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

  new_data <- if (expand) {
    expand.grid(values_list, stringsAsFactors = FALSE)
  } else {
    as.data.frame(values_list, stringsAsFactors = FALSE)
  }

  # Retype to match original
  new_data <- df_copy_classes(new_data, data)

  # Posterior predictions
  pred_fun <-
    if (method == "epred") tidybayes::epred_draws else
      if (method == 'linpred') tidybayes::linpred_draws else
        tidybayes::predicted_draws
  long_preds <-
    pred_fun(fit, newdata = new_data,
             re_formula = re_formula, nlpar = nlpar, value = nlpar)
  long_preds <-
    long_preds[, !names(long_preds)%in%c('.chain', '.iteration', '.row')]
  # keep .draw to perform averaging within each draws
  # long_preds <-
  #   long_preds[, -grep('^\\.', names(long_preds))]

  long_preds <-
    average_over(long_preds %>% as.data.frame, nlpar, average_factors)
  #  pred_fun <- if (method == "epred") brms::posterior_epred else
  #   if (method == 'linpred') brms::posterior_linpred else
  #     brms::posterior_predict
  # preds <- pred_fun(fit, newdata = new_data, re_formula = re_formula, nlpar = nlpar)
  #
  #
  # # --- Reshape: assign .id to columns to avoid parsing strings ---
  # new_data$.id <- seq_len(nrow(new_data))  # unique ID per row
  # pred_df <- as.data.frame(preds)
  # colnames(pred_df) <- as.character(new_data$.id)
  # pred_df$.draw <- seq_len(nrow(pred_df))
  #
  # # Pivot longer and join original predictor values
  # library(tidyr)
  # library(dplyr)
  #
  # long_preds <- pred_df %>%
  #   pivot_longer(cols = -".draw", names_to = ".id", values_to = nlpar) %>%
  #   mutate(.id = as.integer(.id)) %>%
  #   left_join(new_data, by = ".id")
  }
  for (var in input_vars) {
    val <- long_preds[[var]]
    if (is.list(val)) val <- unlist(val)
    if (is.numeric(val) || is.numeric(predictor_values[[var]])) {
      long_preds[[var]] <- factor(round(as.numeric(val), 2))
    } else {
      long_preds[[var]] <- factor(val)
    }
  }

  plot_data <- long_preds[, c(nlpar, input_vars)]
  plot_data <- as.data.frame(plot_data)
  names(plot_data) <- c(nlpar, input_vars)
  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = !!rlang::sym(nlpar),
                               y =
                                 if (length(input_vars))
                                   interaction(!!!rlang::syms(input_vars))
                               else 'grand mean',
                               fill =
                                 if (length(input_vars))
                                   interaction(!!!rlang::syms(input_vars))
                               else 'grand mean'
                               )) +
    ggdist::stat_halfeye(.width = width, adjust = 0.7) +
    ggplot2::labs(
      x = paste(nlpar),
      y = "Condition",
      title = paste0(ifelse(method=='predict', 'Predicted', 'Expected'),
                    ' distribution', rep('s', min(1, length(input_vars))),
                    ' of ', nlpar)
    ) +
    ggplot2::scale_fill_brewer(guide = guide_legend(title =
      if (length(input_vars))
        paste(input_vars, collapse=':')
      else 'grand mean'), type = 'qual', palette = 'Set3'
      ) +
    ggplot2::theme_classic()
}
