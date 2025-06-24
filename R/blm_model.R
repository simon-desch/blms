blm_model <-
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

    dots <- dots_formulas#dots[dots_is_formula]
    parse_inputs <- c(
      list(formula = formula),
      dots,
      list(
        # data = data,
        model_class = model_class,
        model_spec = model_spec,
        par_form = par_form,
        par_transform = par_transform,
        model_func_has_blockgrp = F
      )
    )

    mi <-
      do.call(parse_blms_model_inputs, parse_inputs)
    model_form <- mi$brmsformula

    data <- validate_data(data, mi)

    model_form_parsed <- parse_blocked_formulas(model_form, model_spec$func_stanvar, data)
    return(model_form_parsed)

    model_form$formula <- formula_replace_block_call(model_form$formula)
    # mi <- mi[!names(mi)%in%'brmsformula']
    mi[which(names(mi)%in%'brmsformula')] <- NULL
    model_form <- modifyList(model_form, mi)
    model_form$blmsformula_inputs <- inputs
    class(model_form) <- c('blmsformula', class(model_form)[!class(model_form)%in%'blmsmodelinfo'])



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
