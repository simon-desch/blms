#' Create a blmsformula object
#'
#' @param formula a `formula` defining response and prector variables (and
#'        optionally a block term)
#' @param ... other formula objects describing prediction of model parameters
#' @param data optional, if given, a `data.frame` or the like that contains
#'        the data for the model to be fitted with the formula
#' @param model_class character defining the model class
#' @param model_spec `model_spec` as returned by `get_model_spec(model_class)`
#' @param par_form (linear) formulas for the prediction of model parameters
#'        defined by the model specified by `model_class` and/or `model_spec`
#' @param par_transform `formula` objects defining the transformation aplied to
#'        the linear formula of the parameter named on the left-hand side of
#'        the formula. Other formulas for the respective parameter will be
#'        changed by appending `var` to the parameter name and the
#' @param model_func_has_blockgrp
#'
#' @returns object of class `blmsformula` which inherits from `brmsformula`
#' @export
#'
blmsformula <-
  function(formula,
           ...,
           data = NULL,
           model_class = NULL,
           model_spec = NULL,
           par_form = NULL,
           par_transform = NULL,
           model_func_has_blockgrp = F) {

    dots <- list(...)
    dots_is_formula <- sapply(dots, is.formula)

    dots <- dots[dots_is_formula]
    inputs <- c(
      list(formula = formula),
      dots,
      list(
        # data = data,
        model_class = model_class,
        # model_spec = model_spec,
        par_form = par_form,
        par_transform = par_transform,
        model_func_has_blockgrp = model_func_has_blockgrp
      )
    )

    mi <-
      do.call(parse_blms_model_inputs, inputs)
    model_form <- mi$brmsformula
    model_form$formula <- formula_replace_block_call(model_form$formula)
    # mi <- mi[!names(mi)%in%'brmsformula']
    mi[which(names(mi)%in%'brmsformula')] <- NULL
    model_form <- modifyList(model_form, mi)
    model_form$blmsformula_inputs <- inputs
    class(model_form) <- c('blmsformula', class(model_form)[!class(model_form)%in%'blmsmodelinfo'])
    return(model_form)
  }


#' default_prior implementation for blmsformula
#'
#' @importFrom brms default_prior
#' @method default_prior blmsformula
#' @export
default_prior.blmsformula <-
  function (object, data, family = gaussian(), autocor = NULL,
          data2 = NULL, knots = NULL, drop_unused_levels = TRUE, sparse = NULL,
          ...)
{
    message('default_prior.blmsformula')
  block_vars <- if('block_vars'%in%names(object)) object$block_vars else NULL
  if(!is.null(block_vars) && length(block_vars)>0) {
    data[[object$model_spec$block_var]] <-
      as.numeric(interaction(data[, block_vars]))
  }
  brms:::default_prior.default(
    object, data, family = gaussian(), autocor = NULL,
    data2 = NULL, knots = NULL, drop_unused_levels = TRUE, sparse = NULL,
    ...)
}
