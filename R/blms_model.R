#' blms_model
#'
#' @param formula response formula specifying the response and its prediction
#' @param ... You can pass further formulas to define predictions of parameters
#'        used in the response formula. Any non-formula objects passed via
#'        `...` are passed on to other functions (i.e., `brmsformula` and
#'        `brm`)
#' @param data the data containing the response variable and any variables used
#'        for prediction. Any variables that occur in formulas but cannot be
#'        found in `data` will be considered non-linear parameters.
#' @param formula_only Return the `brmsformula` created from inputs but do not
#'        (try to) compile or run the model
#' @param compile compile the model and return a `brmsfit` object. If `run` is
#'        `FALSE` `brm` is called with its `chains` argument set to `0` which
#'        will compile the model but will not sample
#' @param run calls `brm` with the `brmsformula` created from inputs, `data`,
#'        `stanvars` defined by `model_spec` or `stanvars` defined in `dots`,
#'        and any other arguments passed vie `...` that were not consumed for
#'        creating the formula
#' @param model_class character of length 1 specifying the class of the model
#'        (see `names(get_all_model_specs())` for names of implemented model
#'        classes)
#' @param model_spec list defining a model specification (see
#'        `get_all_model_specs()` for currently implemented models and their
#'        specifications; you can supply other model specifications that
#'        follow the same structure)
#' @param par_form `list()` object containing `formula` object(s) that specify
#'        prediction formulas for other parameters; formulas passed via `...`
#'        will override formulas passed via `par_form`
#' @param par_transform `list()` object containing transformation formulas for
#'        other formulas of the model; transformation formulas can e.g. be used
#'        to transform parameters that have upper and/or lower bounds. When
#'        passing transformation formulas via `par_transform` you can specify
#'        the parameter to transform on the left-hand side and the
#'        transformation formula - containing the same parameter - on the
#'        right-hand-side of the formula (e.g. `d ~ inv_logit(d)`). You can also
#'        pass transformation formulas via the `...` by using a
#'        transformation statement (`transform()`) on the left-hand side of the
#'        formula and the transformation statement on the right-hand side of
#'        the formula, e.g. `transform(d) ~ inv_logit(d)`. `blms` creates from
#'        this formula a non-linear formula in which the parameter on the
#'        right-hand side is replaced by a new parameter named after the
#'        original parameter with \code{'raw'} as suffix (e.g.
#'        \code{d ~inv_logit(draw)}). If in \code{par_form} there is a formula
#'        that has the respective parameter (`d` in the current example) on its
#'        left-hand side, then in that formula the parameter on the left-hand
#'        side is renamed by adding the suffix \code{'raw'} to the first term.
#'        If you want to use
#'        the same transformation for several parameters you can specify
#'        multiple parameters on the left-hand side combining them by `+` and
#'        use 'x' on the right-hand side of the formula. This will create as
#'        many transformation formulas as there are variables on the left-hand
#'        side and replace 'x' on the right-hand side.
#'
#' @returns If \code{formula_only} is \code{TRUE} then the return value is an
#'        object of class \code{blmsformula} (inheriting from class
#'        \code{brms::brmsformula}), else the return value is of class
#'        \code{blmsfit} (inheriting from class \code{brms::brmsfit})
#' @export
#' @importFrom brms brmsformula brm
#'

blms_model <-
  function(formula,
           ...,
           data = NULL,
           formula_only = !compile,
           compile = run,
           run = F,
           model_class = NULL,
           model_spec = NULL,
           par_form = NULL,
           par_transform = NULL
  ) {
    # message('hello')
    # return(NULL)
    if(is.null(model_class)&&is.null(model_spec)) {
      warning('No model specification provided, ',
      'some post-processing functions may not work.',
      'You can define a model specification using a valid model_class or ',
      'provide the model specification with the `model_spec` argument')
    }

    if (missing(formula)) {
      stop('formula must be specified')
    } else {
      # msg_var(formula)
    }
    dots <- list(...)
    # msg_var(dots)
    # extract formulas from dots
    dots_is_formula <- sapply(dots, is_formula)
    dots_formulas <- c()
    if(length(dots_is_formula)) {
      dots_formulas_idx <- which(dots_is_formula)
      if(length(dots_formulas_idx)){
        dots_formulas <- dots[dots_formulas_idx]
        dots_formulas <- unlist(lapply(dots_formulas, split_by_lhs))
        names(dots_formulas) <- sapply(dots_formulas,
                                       \(x) lhs_dp(x, remove_ws = T))
        dots <- dots[-dots_formulas_idx]
      }
    }
    # msg_var(dots_formulas)
    # msg_var(dots)

    model_form <-
      do.call(blmsformula,
              c(list(formula = formula),
                dots_formulas,
                list(model_class = model_class,
                     model_spec = model_spec,
                     par_form = par_form,
                     par_transform = par_transform,
                     model_func_has_blockgrp = T,
                     data = data
                )
              )
              )


    # up to here model_form is an ugly overloaded extension of the brmsformula
    # created from inputs
    # but it is sufficient to a) get_priors, b) get stancode, & c) get standata
    if(formula_only) return(model_form)

    # m$dots <- dots
    # valid_data <- brms_validate_data_2(model_form$brmsformula, data)
    # data2 <-list(
    #   blockgrp = as.numeric(interaction(valid_data[, block_vars]))
    # )
    # dots$data2 <- c(dots$data2, data2)
    # data2 <- NULL

    if(is.null(data)) {
      stop('cannot compile model without data')
    }
    data <- validate_data(data, model_form)

    # blm_frm <-
    #   do.call(brms::blmformula,
    #           c(formula = model_form$brmsformula$formula,
    #             model_form$brmsformula$pforms,
    #             nl = T, loop = F))
    # blm_frm_parsed <-
    #   parse_blocked_formulas(blm_frm, all_stanvars, data)

    # brm args
    chains <- if ('chains'%in%names(dots)) dots$chains else 4
    chains <-   ifelse(run, chains, 0)
    dots$chains <- chains
    all_stanvars <- model_form$model_spec$func_stanvar +  dots$stanvars
    # msg_var(all_stanvars)
    dots$stanvars <- all_stanvars
    # dots$data2 <- if(is.null(dots$data2)) data2 else c(dots$data2, data2)

    # message('model_form')
    # str(model_form)
    # message('model_form$formula')
    # str(model_form$formula)
    # message('model_form$brmsformula')
    # str(model_form$brmsformula)


    brmsformula <- model_form$brmsformula
    brmsformula$formula <- formula_replace_block_call(brmsformula$formula)
    # msg_var(brmsformula)
    brm_args <-
      c(list(formula =model_form,

             data = data,
             # family = model_form$model_spec$family,
             empty = F),
        dots
      )
    # msg_var(brm_args)
    # return(m)
    call_env <- parent.env(environment())
    brm_model <-
      brms::do_call(brms::brm, brm_args, envir = call_env)
    class(brm_model) <- c('blmsfit', class(brm_model))
    brm_model$blms_formula <- model_form

    return(brm_model)
  }


#' standata implementation for blmsfit
#'
#' @importFrom brms standata
#' @method standata blmsfit
#' @export
standata.blmsfit <-
  function (object, newdata = NULL, re_formula = NULL, newdata2 = NULL,
            new_objects = NULL, incl_autocor = TRUE, ...)
  {

    # message('standata.blmsfit')
    # NextMethod()
    # if(F){
    # message('missing(newdata): ', missing(newdata))
    # message('is.null(newdata): ', is.null(newdata))
    if(!missing(newdata) && !is.null(newdata)) {
      # print(head(newdata))
      # message('names(newdata):', toString(names(newdata)))
      newdata <- validate_data(newdata, object$blms_formula)
      NextMethod('standata', newdata = newdata)
    } else {
      NextMethod()
    }
    # }
  }

#' Print method for class blmsfit
#'
#' @param fit An object of class `blmsfit`
#' @param ...
#'
#' @method print blmsfit
#' @export
print.blmsfit <-
  function(x, digits = 2, ...) {
    model_form <- x$blms_formula
    model_pars <-
      names(model_form$model_spec$parameters)
    model_desc <- model_form$model_spec$description
    model_class <- model_form$model_spec$class
    model_func <- model_form$model_spec$func_name
    model_fixed <- model_form$model_spec$fixed_vars
    cat(paste0(model_desc, '\n\n'))

    cat(paste0('blms model', '\n'))
    cat(paste0('Model class: ', model_class, '\n'))
    cat(paste0('Model function: ', model_func, '\n'))
    cat(paste0('Model parameters: ', toString(model_pars), '\n'))
    cat(paste0('fixed :\n'))
    nil <- mapply(\(x, nm){cat(nm, ' = ', x, '\n')},
                  model_fixed, names(model_fixed))

    cat(paste0('\n'))
    parameter_summaries <-
      list()
    for (par in model_pars) {
      if(!par%in%names(x$formula$pforms)) next
      parameter_summaries[[par]] <- summarize_model_par(x, par, ...)
    }
    names(parameter_summaries) <- NULL
    parameter_summaries_df <-
      do.call(rbind, parameter_summaries)
    est_col <- which(names(parameter_summaries_df)%in%'Estimate')
    parameter_summaries_df <-
      parameter_summaries_df[, est_col:ncol(parameter_summaries_df)]
    cat(paste0('estimated :\n'))
    print(round(parameter_summaries_df, digits))
    cat('\n\n\n')
    NextMethod()

  }
