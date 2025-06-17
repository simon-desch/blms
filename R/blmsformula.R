# model formula -----------------------------------------------------------

#' Parse inputs to \code{brmsformula} or \code{blms_model}
#'
#' @param formula A `formula` defining response and predictor variables (and
#'        optionally a block term)
#' @param ... Other formula objects describing prediction of model parameters
#' @param data Optional, if given, a `data.frame` or the like that contains
#'        the data for the model to be fitted with the formula
#' @param model_class A character defining the model class
#' @param model_spec `model_spec` as returned by `get_model_spec(model_class)`
#' @param par_form (linear) formulas for the prediction of model parameters
#'        defined by the model specified by `model_class` and/or `model_spec`
#' @param par_transform `formula` objects defining the transformation applied to
#'        the linear formula of the parameter named on the left-hand side of
#'        the formula. Other formulas for the respective parameter will be
#'        changed by appending `var` to the parameter name and the
#' @param model_func_has_blockgrp Currently not used.
#'
#'
#' @returns \code{list()} with slots \code{model_class}, \code{model_spec},
#'          \code{response_formula}, \code{parameter_formulas},
#'          \code{resp_var}, \code{pred_vars}, \code{block_vars},
#'          \code{trial_var}, \code{cond_var}, \code{dec_var},
#'          \code{brmsformula},
#' @export
parse_blms_model_inputs <-
  function(formula,
           ...,
           data = NULL,
           model_class = NULL,
           model_spec = NULL,
           par_form = NULL,
           par_transform = NULL,
           model_func_has_blockgrp = F) {
    message('=======================')
    message('parse_blms_model_inputs')
    message('=======================')
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
      msg_var(formula)
    }
    dots <- list(...)
    # msg_var(dots)

    if (missing(data)) {
      message('no data provided')
    }
    # msg_var(data)

    missing_model_class <- missing(model_class)
    missing_model_spec <- missing(model_spec)
    model_class <-
      as_single_char(model_class)
    # ToDo: validate model_spec

    # msg_var(model_spec)
    if (is.null(model_spec)) {
      if(is.null(model_class)) {
        stop('Neither model_class nor model_spec provided.',
             'Please provide either a valid model class ',
             '(one of: ', toString(names(get_all_model_specs())), ') ',
             'or a valid model specification.')
      } else if (!model_class%in%names(get_all_model_specs())) {
        stop('no model_spec found for class "', model_class, '". ',
             'please provide either a valid model class ',
             '(one of: ', toString(names(get_all_model_specs())), ') ',
             'or a valid model specification')
      }
      model_spec <- get_model_spec(model_class)
    }
    if (is.null(model_class)) {
      if(is.null(model_spec)) {
        # case already handled above
        stop('Neither model_class nor model_spec provided.',
             'Please provide either a valid model class ',
             '(one of: ', toString(names(get_all_model_specs())), ') ',
             'or a valid model specification.')
      }
      model_class = model_spec$class
    }
    msg_var(model_class)
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
        if(is.null(model_spec$block_var)) return(x)
        if (any(x==model_spec$block_var, na.rm=T)) {
          stop('The variable name \', ', block_var, '\' ',
               'is currently reserved as block_var in ', model_spec$class)
        }
        return(x)
      }

    validate_binary <-
      function(x) {
        if (any(x==model_spec$block_var, na.rm=T)) {
          stop('The variable name \', ', block_var, '\' ',
               'is currently reserved as block_var in ', model_spec$class)
        }
        return(x)
      }

    resp_formula <- parse_response_formula(formula)
    resp_var <-
      sapply(
        resp_formula$resp_var, validate_var,
        par = 'response', data = data,
        valid_value = validate_not_blockgrp
      )
    msg_var(resp_var)

    block_vars <-
      sapply(
        resp_formula$aterms$block$block_vars, validate_var,
        par = 'block', data = data, null_ok = T
      )
    msg_var(block_vars)

    dec_var = NULL
    if ('dec'%in%names(resp_formula$aterms)) {
      dec_vars <-
        sapply(
          resp_formula$aterms$dec$dec_vars, validate_var,
          par = 'decision', data = data, null_ok = T
        )
      if(length(dec_vars)) {
        if(length(dec_vars)>1L) {
          warning('Only a single variable may be defined as decision
                  variable. Expect brms to fail on this...')
        }
        dec_var <- dec_vars[1]
      }
    }
    if (length(dec_var)) msg_var(dec_var)

    trial_var <-
      sapply(
        resp_formula$aterms$block$trial_var, validate_var,
        par = 'trial', data = data, null_ok = T
      )
    if (length(trial_var)) msg_var(trial_var)

    cond_var <-
      sapply(
        resp_formula$aterms$block$cond_var, validate_var,
        par = 'cond', data = data, null_ok = T,
        valid_values = validate_binary
      )
    if (length(cond_var))  msg_var(cond_var)

    # extract formulas from dots

    dots_is_formula <- sapply(dots, is_formula)
    # mapply(\(dot, is_form){message('dot: "', dot, '" is formula: ', is_form)}, dots, dots_is_formula)
    dots_formulas <- c()
    if(length(dots_is_formula)) {
      dots_formulas_idx <- which(dots_is_formula)
      if(length(dots_formulas_idx)){
        dots_formulas <- dots[dots_formulas_idx]
        # dots_formulas <- unlist(lapply(dots_formulas, split_by_lhs))
        # names(dots_formulas) <- sapply(dots_formulas, \(x) deparse(x[[2]]))
        dots <- dots[-dots_formulas_idx]
      }
    }
    # msg_var(dots_formulas)
    # msg_var(dots)

    dots_par_form_list <- list()
    dots_par_transform_list <- list()

    if(length(dots_formulas)) {
      # extract nlp formulas from dots
      # dots_forms_in_model_pars <-
      #   names(dots_formulas)%in%model_pars
      # dots_forms_in_model_pars_idx <-
      #   which(dots_forms_in_model_pars)
      # if(length(dots_forms_in_model_pars_idx)) {
      #   dots_par_form_list <-
      #     dots_formulas[dots_forms_in_model_pars_idx]
      # }
      # dots_par_form_list <- dots_formulas

      # extract transform formulas from dots
      dots_forms_in_transform_forms <-
        sapply(dots_formulas, is_transform_formula)
      dots_forms_in_transform_forms_idx <-
        which(dots_forms_in_transform_forms)
      if(length(dots_forms_in_transform_forms_idx)) {
        dots_par_transform <-
          dots_formulas[dots_forms_in_transform_forms_idx]
        # dots_par_transform_split <-
        #   unlist(sapply(unname(dots_par_transform), split_by_lhs, simplify = F, USE.NAMES = F))
        dots_par_transform_split <-
          unlist(lapply(unname(dots_par_transform), split_by_lhs))
        dots_par_transform_list <-
          lapply(dots_par_transform_split, validate_transform,
                 replace_x = T, replace_call = T)
      }
      dots_forms_not_in_transform_forms_idx <-
        which(!dots_forms_in_transform_forms)

      dots_par_form <- dots_formulas[dots_forms_not_in_transform_forms_idx]
      # msg_var(dots_par_form)
      # dots_par_form_list <-
      #   unlist(sapply(unname(dots_par_form), norma))
      dots_par_form_list <-
        unlist(sapply(unname(dots_par_form), split_by_lhs))
    }
    # msg_var(dots_par_form_list)
    # msg_var(dots_par_transform_list)

    # default parameter formulas
    # msg_var(m$par_form)
    default_par_form_list <-
      unlist(lapply( m$par_form, split_by_lhs))
    # msg_var(default_par_form_list)

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
    # msg_var(model_par_forms)


    # transforms of model parameters
    ## default transforms
    default_par_transform_split <-
      unlist(lapply(m$par_transform, split_by_lhs))
    # msg_var(default_par_transform_split)
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
    # msg_var(model_par_transforms)

    parameter_formulas <-
      parse_par_formulas(
        model_par_forms, model_par_transforms,
        model_pars = model_pars, data = data
      )
    # msg_var(parameter_formulas)
    parameter_formulas <- unlist(parameter_formulas, recursive = F)
    # names(parameter_formulas) <- NULL
    # msg_var(parameter_formulas)

    # return()
    # data_vars <- c(resp_var, pred_vars, block_vars)
    # data_vars <- c(
    #   m$block_var,
    #   ifelse(is.null(dec_var), resp_var, dec_var),
    #   pred_vars
    #   )

    # model_func_inputs <- c(data_vars, model_pars)
    # model_function <- m$func_name
    response_formula <- formula
    # as.formula(
    #   paste0(
    #     lhs_dp(formula), ' ~ ',
    #     model_function, '(', paste(model_func_inputs, collapse = ', '), ')'
    #   )
    # )

    if ('fixed_vars'%in%names(model_spec)) {
      message('fixed_vars: ', toString(names(model_spec$fixed_vars)))
      for (fixed_var in names(model_spec$fixed_vars)) {
        if (fixed_var%in%names(parameter_formulas)) {
          param_form_vars <- lapply(parameter_formulas, \(x) all.vars(rhs(x)))
          fixed_form_par_pos <-
            Position(\(x) length(x) == 1L && fixed_var%in% x, param_form_vars)
          if(!is.na(fixed_form_par_pos)){
            distr_par_form <- parameter_formulas[[fixed_form_par_pos]]
            distr_par <- all.vars(lhs(distr_par_form))
            fixed_par <- all.vars(rhs(distr_par_form))
            message('Setting distributional parameter ', distr_par,
                    ' to ', model_spec$fixed_vars[[fixed_var]])
            parameter_formulas[[distr_par]] <-
              model_spec$fixed_vars[[fixed_var]]
            parameter_formulas[fixed_par] <- NULL
            next
          }

        }
        # if (grepl(paste0('\\b', fixed_var, '\\b'), deparse(response_formula))) {
        #   # message()
        # }
        response_formula <-
          formula_replace(response_formula, 'r',
                          paste0('\\b', fixed_var, '\\b'),
                          model_spec$fixed_vars[[fixed_var]]
          )
        for (i in 1:length(parameter_formulas)) {
          parameter_formulas[[i]] <-
            formula_replace(parameter_formulas[[i]], 'r',
                            paste0('\\b', fixed_var, '\\b'),
                            model_spec$fixed_vars[[fixed_var]]
            )
        }
      }
      fixed_params_idx <-
        which(names(model_spec$fixed_vars)%in%names(parameter_formulas))
      fixed_params_nms <- names(model_spec$fixed_vars)[fixed_params_idx]
      parameter_formulas[fixed_params_nms] <- NULL
      # parameter_formulas <- c(parameter_formulas, model_spec$fixed_vars)
    }

    # response_formula <- formula
    # if ('fixed_vars'%in%names(model_spec)) {
    #   message('fixed_vars: ', toString(names(model_spec$fixed_vars)))
    #   for (fixed_var in names(model_spec$fixed_vars)) {
    #     parameter_formulas[[length(parameter_formulas)+1]] <-
    #       as.formula(paste0(fixed_var, ' ~ ',
    #                         model_spec$fixed_vars[[fixed_var]]))
    #   }
    # }



    # msg_var(response_formula)
    # msg_var(parameter_formulas)


    bf_args_list <-
      c(formula = response_formula,
        parameter_formulas,
        list(
          family = m$family,
          nl = T,
          loop = F,
          unused =  as.formula(paste('~', paste(c(block_vars, trial_var, cond_var), collapse = ' + ')))
        )
      )
    # msg_var(bf_args_list)
    model_form <-
      brms::do_call(brms::brmsformula, bf_args_list)

    # msg_var(model_form)
    model_form_info <- parse_brms_blms(model_form, model_spec)
    model_form <- model_form_info$formula
    pred_vars <- model_form_info$pred_vars
    # msg_var(model_form)
    mf <- list(
      model_class = model_class,
      model_spec = model_spec,
      response_formula = resp_formula,
      parameter_formulas = parameter_formulas,
      resp_var = resp_var,
      pred_vars = pred_vars,
      block_vars = block_vars,
      trial_var = trial_var,
      cond_var = cond_var,
      dec_var = dec_var,
      brmsformula = model_form
    )
    class(mf) <- c('blmsmodelinfo', class(mf))
    message('=======================')
    return(mf)
  }


#' Create a \code{blmsformula} object
#'
#' @inheritParams parse_blms_model_inputs
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
    dots_is_formula <- sapply(dots, is_formula)

    dots_formulas <- c()
    if(length(dots_is_formula)) {
      dots_formulas_idx <- which(dots_is_formula)
      if(length(dots_formulas_idx)){
        dots_formulas <- dots[dots_formulas_idx]
        dots_formulas <- unlist(lapply(dots_formulas, split_by_lhs))
        names(dots_formulas) <- sapply(dots_formulas, \(x) deparse(x[[2]]))
        dots <- dots[-dots_formulas_idx]
      }
    }

    dots <- dots_formulas#dots[dots_is_formula]
    inputs <- c(
      list(formula = formula),
      dots,
      list(
        # data = data,
        model_class = model_class,
        model_spec = model_spec,
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


#' Validate data for fitting with blmsformula
#'
#' @param data A `data.frame`containing the data to be validated.
#' @param blmsformula A `blmsformula` object.
#'
#' @returns A validated `data.frame`based on `blmsformula`
#' @export
validate_data <-
  function(data, blmsformula) {
    # if(is.null(data)) return(NULL)
    if(is.null(data)) return(NULL)
    # ToDo implement checks for other data variables based on model
    block_vars <-
      if ('block_vars'%in%names(blmsformula)) {
        blmsformula$block_vars
      } else {NULL}
    cond_var <-
      if ('cond_var'%in%names(blmsformula)) {
        blmsformula$cond_var
      } else {NULL}
    data2 <- NULL
    missing_block_vars <- setdiff(block_vars, names(data))
    if(length(missing_block_vars)){
      if(length(missing_block_vars)==length(block_vars)) {
        warning('All variables defining the block structure (',
                toString(missing_block_vars), ') are missing in data.')
        block_vars <- NULL
      } else {
        block_vars <- intersect(block_vars, names(data))
        warning('Some variables defining the block structure (',
                toString(missing_block_vars), ') are missing in data. ',
                'Only ', toString(block_vars), ' will be used to ',
                'define block structure.')
      }
    } else {

      blckgrp <- 1
      if(!is.null(block_vars) && length(block_vars)>0) {
        blckgrp <- as.numeric(interaction(data[, block_vars]))
        if(!is.null(cond_var) && length(cond_var)>0) {
          if (!cond_var%in%names(data)) {
            warning('cond_var "', cond_var, '"not found in data. ',
                    'Conditioning will not be performed in block structure.')
          } else {
            # message('cond_var: ', cond_var, ' (org: ', toString(unique(data[, cond_var])), ')')
            cond_dat <- unlist(data[, cond_var])
            cond_num <- as.numeric(as.factor(cond_dat))-1
            # message('cond_var: ', cond_var, ' (org: ', toString(unique(data[, cond_var])), ', num: ', toString(unique(cond_num)), ')')
            blckgrp <- blckgrp*cond_num
          }
        }
      }
      data[[blmsformula$model_spec$block_var]] <-
        blckgrp
    }
    return(data)
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

#' standata implementation for blmsformula
#'
#' @importFrom brms standata
#' @method standata blmsformula
#' @export
standata.blmsformula <-
  function (object, data, family = gaussian(), prior = NULL, autocor = NULL,
            data2 = NULL, cov_ranef = NULL, sample_prior = "no", stanvars = NULL,
            threads = getOption("brms.threads", NULL), knots = NULL,
            drop_unused_levels = TRUE, ...)
  {
    message('standata.blmsformula')
    if(!missing(data)) {
      data <- validate_data(data, object)
    }
    brms:::standata.default(
      object, data, family = family, prior = prior, autocor = autocor,
      data2 = data2, cov_ranef = cov_ranef, sample_prior = sample_prior, stanvars = stanvars,
      threads = threads, knots = knots,
      drop_unused_levels = drop_unused_levels, ...
      )
  }


#' stancode implementation for blmsformula
#'
#' @importFrom brms stancode
#' @method stancode blmsformula
#' @export
stancode.blmsformula <-
  function (object, data, family = gaussian(), prior = NULL, autocor = NULL,
            data2 = NULL, cov_ranef = NULL, sparse = NULL, sample_prior = "no",
            stanvars = NULL, stan_funs = NULL, knots = NULL,
            drop_unused_levels = TRUE,
            threads = getOption("brms.threads", NULL),
            normalize = getOption("brms.normalize", TRUE),
            save_model = NULL, ...)
    {

    message('stancode.blmsformula')
    if(!missing(data)) {
      data <- validate_data(data, object)
    }

    brms:::stancode.default(
      object, data, family = gaussian(), prior = prior, autocor = autocor,
      data2 = data2, cov_ranef = cov_ranef, sparse = sparse, sample_prior = sample_prior,
      stanvars = stanvars, stan_funs = stan_funs, knots = knots,
      drop_unused_levels = drop_unused_levels,
      threads = threads,
      normalize = normalize,
      save_model = save_model, ...
      )
    }

