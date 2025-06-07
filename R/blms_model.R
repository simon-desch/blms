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
#' @param formula_only return the `brmsformula` created from inputs but do not
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
#' @returns If `formula_only` is `TRUE` then the return value is an object of
#'        class `blmsformula` (inheriting from class `brmsformula`), else the
#'        return value is of class `blmsfit` (inheriting from class `brmsfit`)
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
    dots_is_formula <- sapply(dots, is.formula)
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




    if(F) {
    mf <- do.call(parse_blms_model_inputs,
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
    # msg_var(mf)
    if (F) {
    if (missing(data)) {
      message('no data provided')
    }
    # msg_var(data)

    missing_model_class <- missing(model_class)
    missing_model_spec <- missing(model_spec)
    model_class <-
      as_single_char(model_class)
    model_spec <-
      if (missing(model_spec))
        get_model_spec(model_class)
    else
      model_spec

    if (is.null(model_spec)) {
      if(is.null(model_class)) {
        stop('no or invalid model class provided')
      } else if (!model_class%in%names(get_all_model_specs())) {
        stop('no model_spec found for class "', model_class, '". ',
             'please provide either a valid model class ',
             '(one of: ', toString(names(get_all_model_specs())), ') ',
             'or a valid model specification')
      }
    }
    msg_var(model_class)
    # msg_var(model_spec)
    model_pars <- get_parameter_names(model_spec)
    msg_var(model_pars)

    m <- model_spec
    # ToDo: implement validation of model_spec
    if(missing(par_form)) par_form <- m$par_form
    if(missing(par_transform)) par_transform <- m$par_transform


    # msg_var(formula)
    # msg_var(data)
    # msg_var(formula_only)
    # msg_var(compile)
    # msg_var(run)
    # msg_var(model_class)



    validate_not_blockgrp <-
      function(x) {
        if (x=='blockgrp') {
          stop('The variable name \'blockgrp\' is currently reserved in blms')
        }
        return(x)
      }

    resp_formula <- parse_response_formula(formula)
    resp_var <-
      sapply(
        resp_formula$resp_var, validate_var,
        par = 'response', data = data, valid_value = validate_not_blockgrp,
      )
    msg_var(resp_var)

    pred_vars <-
      sapply(
        resp_formula$pred_vars, validate_var,
        par = 'predictor', data = data, valid_value = validate_not_blockgrp,
      )
    msg_var(pred_vars)

    block_vars <-
      sapply(
        resp_formula$aterms$block$block_vars, validate_var,
        par = 'block', data = data, null_ok = T
      )
    msg_var(block_vars)



    dots_par_form_list <- list()
    dots_par_transform_list <- list()

    if(length(dots_formulas)) {
      # extract nlp formulas from dots
      dots_forms_in_model_pars <-
        names(dots_formulas)%in%model_pars
      dots_forms_in_model_pars_idx <-
        which(dots_forms_in_model_pars)
      if(length(dots_forms_in_model_pars_idx)) {
        dots_par_form_list <-
          dots_formulas[dots_forms_in_model_pars_idx]
      }

      # extract transform formulas from dots
      dots_forms_in_transform_forms <-
        sapply(dots_formulas, is_transform_formula)
      dots_forms_in_transform_forms_idx <-
        which(dots_forms_in_transform_forms)
      if(length(dots_forms_in_transform_forms_idx)) {
        dots_par_transform <-
          dots_formulas[dots_forms_in_transform_forms_idx]
        dots_par_transform_list <-
          unlist(lapply(dots_par_transform, split_by_lhs))
      }
    }
    msg_var(dots_par_form_list)
    msg_var(dots_par_transform_list)

    # default parameter formulas
    default_par_form_list <-
      unlist(lapply( m$par_form, split_by_lhs))
    msg_var(default_par_form_list)

    arg_par_form <-
      if(is.list(par_form)) par_form else list(par_form)
    arg_par_form_split <-
      unlist(lapply(arg_par_form, split_by_lhs))
    # msg_var(arg_par_form_split)
    arg_par_form_list <-
      arg_par_form_split[is.element(names(arg_par_form_split), model_pars)]
    # msg_var(arg_par_form_list)

    model_par_forms <-
      default_par_form_list
    # by default, par_form is the same as default_par_form
    # anything given via par_form argument
    # should override formulas from the defaults
    for (par in names(arg_par_form_list)) {
      model_par_forms[[par]] <- arg_par_form_list[[par]]
    }
    # any further formulas given as argument should also override defaults and
    # also formulas given by par_form
    for (par in names(dots_par_form_list)) {
      model_par_forms[[par]] <- dots_par_form_list[[par]]
    }
    msg_var(model_par_forms)


    # transforms of model parameters
    ## default transforms
    default_par_transform_split <-
      unlist(lapply(m$par_transform, split_by_lhs))
    default_par_transform_list <-
      lapply(default_par_transform_split, validate_transform, replace_x = T)
    # msg_var(default_par_transform_list)

    ## given transforms
    arg_par_transform <-
      if(is.list(par_transform)) par_transform else list(par_transform)
    # msg_var(arg_par_transform)
    arg_par_transform_split <-
      unlist(lapply(arg_par_transform, split_by_lhs))
    # msg_var(arg_par_transform_split)
    arg_par_transform_list <-
      lapply(arg_par_transform_split, validate_transform, replace_x = T)
    # msg_var(arg_par_transform_list)

    model_par_transforms <-
      default_par_transform_list
    # by default, par_transform is the same as default_par_transform
    # anything given via par_transform argument
    # should override formulas from the defaults
    for (par in names(arg_par_transform_list)) {
      model_par_transforms[[par]] <- arg_par_transform_list[[par]]
    }
    # any further formulas given as argument should also override defaults and
    # also formulas given by par_transform
    for (par in names(dots_par_transform_list)) {
      model_par_transforms[[par]] <- dots_par_transform_list[[par]]
    }
    msg_var(model_par_transforms)

    parameter_formulas <-
      parse_par_formulas(
        model_par_forms, model_par_transforms,
        model_pars = model_pars, data = data
      )
    parameter_formulas <- unlist(parameter_formulas)
    msg_var(parameter_formulas)
    # return()
    data_vars <- c(resp_var, pred_vars, block_vars)
    data_vars <- c(resp_var, pred_vars, 'blockgrp')
    model_func_inputs <- c(data_vars, model_pars)
    model_function <- m$func_name
    nl_formula <-
      as.formula(
        paste0(
          resp_var, ' ~ ',
          model_function, '(', paste(model_func_inputs, collapse = ', '), ')'
        )
      )
    msg_var(nl_formula)

    m$nl_formula <- nl_formula
    m$par_formulas <- parameter_formulas
    bf_args_list <-
      c(formula = nl_formula,
        # lapply(nlp_flist, lf, loop=F),
        parameter_formulas,
        list(
          family = m$model_family,
          nl = T,
          loop = F
        )
      )
    model_form <-
      do.call(brms::brmsformula, bf_args_list)
    # m$formula <- model_form
    msg_var(model_form)
    m <-
      modifyList(model_form, m)
    class(m) <- c('blmsformula', class(m))
    } # if (F)
    model_form <-
      mf$brmsformula
    model_form$formula <- formula_replace_block_call(model_form$formula)
    model_form <-
      modifyList(model_form, mf)
    class(model_form) <- c('blmsformula', class(model_form))
    }
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

    if(is.null(data)) {
      stop('cannot compile model without data')
    }
    block_vars <-
      if ('block_vars'%in%names(model_form)) {
        model_form$block_vars
      } else {NULL}
    data2 <- NULL
    if(!is.null(block_vars) && length(block_vars)>0) {
      data[[model_form$model_spec$block_var]] <-
        as.numeric(interaction(data[, block_vars]))
      # valid_data <- brms_validate_data_2(model_form$brmsformula, data)
      # data2 <-list(
      #   blockgrp = as.numeric(interaction(valid_data[, block_vars]))
      # )
    }
    # dots$data2 <- c(dots$data2, data2)


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
    cat(paste0(model_desc, '\n\n'))

    cat(paste0('blms model', '\n'))
    cat(paste0('Model class: ', model_class, '\n'))
    cat(paste0('Model function: ', model_func, '\n'))
    cat(paste0('Model parameters: ', toString(model_pars), '\n'))
    cat(paste0('\n'))
    parameter_summaries <-
      list()
    for (par in model_pars) {
      parameter_summaries[[par]] <- summarize_model_par(x, par, ...)
    }
    names(parameter_summaries) <- NULL
    parameter_summaries_df <-
      do.call(rbind, parameter_summaries)
    est_col <- which(names(parameter_summaries_df)%in%'Estimate')
    parameter_summaries_df <-
      parameter_summaries_df[, est_col:ncol(parameter_summaries_df)]
    print(round(parameter_summaries_df, digits))
    cat('\n\n\n')
    NextMethod()

  }
