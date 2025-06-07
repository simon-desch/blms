## formula helper ----------------------------------------------------------

#' Remove whitespaces from a string
#'
#' @param str A \code{character()}
#' @param replace_ws \code{logical}. Replaces all kinds of extended whitespaces
#'        (e.g. \code{\t}, \code{\n}, or multiple whitespaces) by a single space
#' @param trim_ws removes whitespaces from the beginniing and the end of a
#'        string
#' @param remove_ws removes any whitespaces from the string
#'
#' @returns The input \code{character} with whitespaces replaced or removed
rm_ws <-
  function(str, replace_ws = T, trim_ws = T, remove_ws = F) {

    if (replace_ws) str <- gsub('\\t|\\n|\\s+', ' ', str)
    if (trim_ws) str <- gsub('^\\s+', '', gsub('\\s+$', '', str))
    if (remove_ws) str <- gsub('\\s+', '', str)
    return(str)
  }

#' Test if input is a formula
#'
#' @param x Any object
#'
#' @returns \code{TRUE} if \code{x} is a formula, else \code{FALSE}
is.formula <-
  function(x) {is.call(x) && x[[1]] == quote(`~`)}

#' Get the left-hand side of a formula
#'
#' @param f A \code{formula} object
#'
#' @returns The left-hand side of the formula, if it has one,
#'          otherwise \code{NULL}
lhs <-
  function(f) {
    if (length(f)==3) return(f[[2]])
    return(NULL)
  }


#' Get the right hand side of a formula
#'
#' @param f
#'
#' @returns The right-hand side of a formula
rhs <-
  function(f) {
    if (length(f)==3) return(f[[3]])
    if (length(f)==2) return(f[[2]])
    return(NULL)
  }

#' Get the deparsed version of left-hand side of a formula
#'
#' @param f A \code{formula} object
#' @inheritParams rm_ws
#' @returns The left-hand side of a formula, possibly with whitespaces
#'          replaced or removed
lhs_dp <-
  function(f, replace_ws = T, trim_ws = T, remove_ws = F) {
    lhs_ <- lhs(f)
    if (is.null(lhs_)) return(NA)
    dp1_lhs <- rm_ws(deparse1(lhs_), replace_ws, trim_ws, remove_ws)
    return(dp1_lhs)
  }

#' Get the deparsed version of left-hand side of a formula
#'
#' @param f A \code{formula} object
#' @inheritParams rm_ws
#' @returns The right-hand side of a formula, possibly with whitespaces
#'          replaced or removed
rhs_dp <-
  function(f, replace_ws = T, trim_ws = T, remove_ws = F) {
    rhs_ <- rhs(f)
    if (is.null(rhs_)) return(NA)
    dp1_rhs <- rm_ws(deparse1(rhs_), replace_ws, trim_ws, remove_ws)
    return(dp1_rhs)
  }

rhs_terms <-
  function(formula, deparsed = F) {
    if (!inherits(formula, "formula")) stop("Input must be a formula.")
    rhs <- rhs(formula)#formula[[3]]
    cterms <- list()
    math_operators <- c("+", "-", "*", "/", "^", ":", "|", "~", "$", "[", "[[")
    traverse_top_level <- function(expr) {
      if (is.call(expr) && as.character(expr[[1]]) %in%math_operators) {
        lapply(expr[-1], traverse_top_level)
      } else {
        cterms[[length(cterms) + 1]] <<- expr
      }
    }
    traverse_top_level(rhs)
    return(cterms)
  }
#' Test whether a formula is two-sided
#'
#' @param form The \code{formula}
#'
#' @returns \code{TRUE} if the \code{formula} is two-sided, that is,
#'          if it has a left-hand side
is.twosided <-
  function(form) {
    is_form <- is.formula(form)
    if(!is_form) {
      stop(form, ' is not a formula')
    }
    length(as.list(form))==3
  }

#' Split a formula into multiple formulas based on variables defined in the
#' left-hand side of the formula
#'
#' @param form
#'
#' @returns \code{list()} containing \code{formula} objects created
#' @details
#' Variables on the left-hand side of the formula are extracted using
#' \code{all.vars}
#'
split_by_lhs <-
  function(form) {
    lh_vars <- all.vars(lhs(form))
    names(lh_vars) <- lh_vars
    rhs_str <- rhs_dp(form)
    lapply(
      lh_vars,
      \(x) {
        outform <- as.formula(paste0(x, ' ~ ', rhs_str))
        attributes(outform) <- attributes(outform)
        return(outform)
      }
      )
  }

#' Find and return terms of a formula that (do not) contain a bar (|)
#'
#' @param form A \code{formula} object
#' @param invert Return term that do not contain a \code{|}
#'
#' @returns A \code{character} vector of the selected terms
find_bar_terms <-
  function(form, invert = F) {
    # msg_var(form)
    # message(class(form))
    # grep("|", labels(terms(form)), fixed = T, value = T, invert = invert)
    grep("|", rhs_terms(form, deparsed = T), fixed = T, value = T, invert = invert)
  }

#' Replace patterns in part of a formula
#'
#' @param form A \code{formula} object.
#' @param side \code{'left'}, \code{'right'}, or \code{'both'} for the
#'        left-hand side, the right-hand side, or both sides of a formula,
#'        respectively. Can be abbreviated.
#' @inheritParams base::gsub
#' @returns A \code{formula} object with \code{pattern} replaced by
#'          \code{replacement} on the side of the \code{formula} defined by
#'          \code{side}.
formula_replace <-
  function(form, side, pattern, replacement, perl = T, ...) {
    form_lhs_str <- lhs_dp(form)
    form_rhs_str <- rhs_dp(form)

    if(side>=0||grepl('^[rb]', side)) {
      form_rhs_str <-
        gsub(
          pattern, replacement, form_rhs_str,
          perl = perl, ...
        )
    }
    if(side<=0||grepl('^[lb]', side)) {
      form_lhs_str <-
        gsub(
          pattern, replacement, form_lhs_str,
          perl = perl, ...
        )
    }

    # msg_var(paste0(form_lhs_str, '~', form_rhs_str))
    form_replaced <-
      as.formula(
        paste0(form_lhs_str, '~', form_rhs_str)
      )
    # attr(form_replaced, '.Environment') <- attr(form, '.Environment')
    attributes(form_replaced) <- attributes(form)
    return(form_replaced)
  }



find_tilde_expressions <- function(expr_str) {
  results <- list()
  seen <- character()

  is_tilde <-
    function(x) is.call(x) && identical(x[[1]], as.name("~"))


  walk <- function(e, parent_is_tilde = FALSE) {
    if (is.call(e)) {
      call_name <- as.character(e[[1]])
      args <- as.list(e)[-1]

      # Detect tilde expressions
      if (!is_tilde(e) && any(vapply(args, is_tilde, logical(1)))) {
        tilde_idx <- which(vapply(args, is_tilde, logical(1)))
        tilde_expr <- args[[tilde_idx]]
        full_str <- deparse1(e)
        if (full_str %in% seen) return()
        seen <<- c(seen, full_str)

        lhs_str <- lhs_dp(tilde_expr)
        rhs_str <- rhs_dp(tilde_expr)

        results[[length(results) + 1]] <<- list(
          name = if (call_name == "(") NULL else call_name,
          full = full_str,
          inner = deparse1(tilde_expr),
          inner.lhs = if (!is.na(lhs_str)) lhs else NA,
          inner.rhs = if (!is.na(rhs_str)) rhs else NA,
          nested = parent_is_tilde,
          tilde.arg.pos = tilde_idx,
          all.args = lapply(args, deparse1)
        )

        # recurse inside tilde expression
        walk(tilde_expr, parent_is_tilde = TRUE)
        lapply(
          args[-tilde_idx],
          \(arg) walk(arg, parent_is_tilde = TRUE)
        )
      } else {
        # recurse deeper for all arguments, keep parent_is_tilde flag
        lapply(
          as.list(e),
          \(arg) walk(arg, parent_is_tilde = parent_is_tilde)
        )
      }
    }
  }

  exprs <- parse(text = expr_str)
  lapply(exprs, function(e) walk(e, parent_is_tilde = FALSE))
  return(results)
}
# s <- "d ~ ilogit(lb, d~c + (1 | s(dd~x*y)), 1.5) + log(1/s) + (c~a + b)"
# res <- find_tilde_expressions(s)
# str(res)
# str(Filter(\(x) !x$nested, res))

#' Validate if a variable is suitable to be used for parameter in a model
#'
#' @param var The variable. If \code{length(variable)>1} only the first element
#'            is used.
#' @param par Name of the parameter for which the variable is validated
#'        (only used for error messages)
#' @param data Optional. A \code{data.frame} (or the like) in which to look for a
#'        variable named \code{var}
#' @param valid_values Only applies if \code{data} is not \code{NULL} and a
#'        variable named \code{var} was found in \code{data}.
#'        If \code{valid_values} is a vector (i.e.
#'        \code{is.vector(valies_values)} is \code{TRUE}), then values of
#'        \code{var} are checked against values in \code{valid_values} using
#'        \code{setdiff}. \code{valid_values} can also be a function that takes
#'        values of \code{var} as input. If all values are deemed to be valid, it
#'        should return those values. Otherwise, the function can raise an
#'        error or return an error message as a \code{character} of length 1
#'        with a class attribute of 'error_message' (if that is not the case
#'        values are considered to be valid).
#' @param null_ok If \code{is.null(var)} return \code{NULL} if
#'        \code{null_ok = TRUE}, else (the default) raise an error
#'
#' @returns \code{var}, unless an error is raised.
#'
validate_var <-
  function(var, par, data = NULL, valid_values = NULL, null_ok = F) {

    out <- NULL
    if (is.formula(var)) {
      is_twosided <- is.twosided(var)
      out <- all.pars(rhs(var))[1]
    } else if (is.character(var)) {
      out <- var[1]
      out <- sub('^([^~]+~)', '', out)
      out <- strsplit(out, '\\s')[[1]][1]
    } else if (is.null(var)) {
      if(null_ok) return(NULL)
      stop('invalid definition of ', par, ': must not be NULL!')
    } else {
      stop('invalid pariable definition: ', var, '. ',
           'Must be either character of length 1 ',
           'or right hand sided formula ',
           'specifying the pariable coding the ', par)
    }

    if(!is.null(data)) {
      in_data <- out%in%names(data)
      if(!in_data) {
        stop('variable `', out, '` not found in data')
      }
      if (!is.null(valid_values)) {
        data_values <- unique(data[[out]])
        if (is.vector(valid_values)) {
          value_diff <- base::setdiff(data_values, valid_values)
          values_valid <- length(value_diff)>1L
          error_msg <- paste0('Must be one of ', toString(valid_values), '.')
        }
        if (is.function(valid_values)) {
          values_valid <- valid_values(data_values)
          if(inherits(values_valid, 'error_message')) {
            error_msg <- values_valid
            values_valid <- F
          }
        }
        if(isFALSE(values_valid)) {
          stop('invalid values for ', par, ' ',
               'found in data$', out, '. ', error_msg)
        }
      }

    }
    return(out)

  }


## response formula --------------------------------------------------------

parse_block_term <-
  function(block_term, warn_missing_trial = T) {
    bar_pos <-
      gregexpr("\\s*\\|+\\s*", block_term) # allows multiple consecutive bars
    block_call_parts <-
      regmatches(block_term, bar_pos, invert = T)[[1]]

    trial_var <- NULL
    block_vars <- NULL
    # message('length(block_call_parts): ', length(block_call_parts))
    # msg_var(block_call_parts)
    if(length(block_call_parts)==0) {
      warning('missing block variable')
    } else if(length(block_call_parts)==1) {
      warning('no trial variable provided in block term, ',
              'assuming consecutive trials within each block.')
      block_vars <-
        all.vars(as.formula(paste0('~', block_call_parts[1]))[[2]])
    } else if(length(block_call_parts)>1) {
      trial_var <-
        all.vars(as.formula(paste0('~', block_call_parts[1]))[[2]])
      if (length(trial_var)==0 && warn_missing_trial) {
        warning('no trial variable provided in block term, ',
                'assuming consecutive trials within each block.')
        trial_var <- NULL
      }
      block_vars <-
        all.vars(as.formula(paste0('~', block_call_parts[2]))[[2]])
      if(length(block_call_parts)>2) {
        warning('too many terms provided for block and trial definition, ',
                'the following terms will be ignored: ',
                toString(block_call_parts[3:length(block_call_parts)]))
      }
    }
    if (length(trial_var)>1L) {
      warning('multiple variables listed for trial definition ',
              'only the first, ', trial_var[1], ', will be used')
      trial_var <- trial_var[1]
    }
    type <- 'block'
    brms:::nlist(type, trial_var, block_vars, term = block_term)
  }

parse_block_call <-
  function(block_call, warn_missing_trial = T) {
    block_call <-
      gsub('\\s', '', block_call)
    block_call_fun_pos <-
      regexpr("(block\\([^\\)]*\\))", block_call)
    block_call_fun_match <-
      regmatches(block_call, block_call_fun_pos)[[1]]
    if(block_call!=block_call_fun_match) {
      stop('invalid block function call, "', block_call, '" ',
           'does not match block(.*)')
    }
    block_term_pos <-
      regexpr('(?<=block\\()([^\\)]+)(?=\\))', block_call, perl = T)
    block_term <- regmatches(block_call, block_term_pos)

    block <-
      parse_block_term(block_term, warn_missing_trial = warn_missing_trial)
    block$call <- block_call
    return(block)
  }

parse_dec_term <-
  function(dec_term) {
    dec_var <-
      all.vars(as.formula(paste0('~', dec_term))[[2]])
    if(length(dec_var)>1L) {
      warning('multiple variables listed for definition of decision variable, ',
              'only the first, ', dec_var[1], ', will be used')
      dec_var <- dec_var[1]
    }
    type <- 'dec'
    brms:::nlist(type, dec_var, term = dec_term)
  }

parse_dec_call <-
  function(dec_call) {
    dec_call <-
      gsub('\\s', '', dec_call)
    dec_call_fun_pos <-
      regexpr("(dec\\([^\\)]*\\))", dec_call)
    dec_call_fun_match <-
      regmatches(dec_call, dec_call_fun_pos)[[1]]
    if(dec_call!=dec_call_fun_match) {
      stop('invalid dec function call, "', dec_call, '" ',
           'does not match dec(.*)')
    }
    dec_term_pos <-
      regexpr('(?<=dec\\()([^\\)]+)(?=\\))', dec_call, perl = T)
    dec_term <- regmatches(dec_call, dec_term_pos)

    dec <- parse_dec_term(dec_term)
    dec$call <- dec_call
    return(dec)
  }

parse_aterm_call <-
  function(type, call, warn_missing_trial = T) {
    if (type=='block') {
      return(parse_block_call(call, warn_missing_trial = warn_missing_trial))
    }
    if (type=='dec') {
      return(parse_dec_call(call))
    }
    return(NULL)
  }

parse_response_formula <-
  function(form) {
    if(!is.formula(form)) {
      stop('Please provide formula for model secification')
    }
    has_lhs <- is.twosided(form)
    if(!is.twosided(form)) {
      stop('formula must have lhs to define repsonse variable')
    }
    lhs_str <- lhs_dp(form)

    lhs_first_bar_pos <-
      regexpr("\\s*\\|\\s*", lhs_str)
    lhs_parts <-
      regmatches(lhs_str, lhs_first_bar_pos, invert = T)[[1]]
    # message('lhs_parts: ', toString(lhs_parts))
    resp_parts <-
      all.vars(as.formula(paste('~', lhs_parts[1]))[[2]])
    if(length(resp_parts)!=1L) {
      stop('only one variable may be defined as response in the formula')
    }
    resp_var <- resp_parts

    aterms <-
      list(block = NULL, dec = NULL)
    if (length(lhs_parts)>1L) {
      aterms_str <- if (length(lhs_parts)>1L) lhs_parts[2] else NULL
      # msg_var(aterms_str)
      aterms_fun_pos <-
        gregexpr("([a-z_]+\\([^\\)]*\\))", aterms_str)
      aterms_fun_calls <-
        regmatches(aterms_str, aterms_fun_pos, invert = F)[[1]]
      aterms_fun_names_pos <-
        regexpr('([a-z_]+)(?=\\()', aterms_fun_calls, perl = T)
      aterms_fun_names <-
        regmatches(aterms_fun_calls, aterms_fun_names_pos)
      names(aterms_fun_calls) <- aterms_fun_names
      valid_aterms_funs <- c('block', 'dec')
      invalid_funs <- setdiff(aterms_fun_names, valid_aterms_funs)
      if (length(invalid_funs)>0) {
        warning('allowed funs in aterms: ',
                toString(paste0(valid_aterms_funs, '()')), '. ',
                'Other funs (', toString(paste0(invalid_funs, '()')), ' ',
                'will be ignored.')
      }
      aterms_fun_calls <-
        aterms_fun_calls[intersect(aterms_fun_names, valid_aterms_funs)]
      # msg_var(aterms_fun_calls)
      aterms_parsed <-
        mapply(
          parse_aterm_call,
          names(aterms_fun_calls),
          aterms_fun_calls,
          SIMPLIFY = F
        )
      # msg_var(aterms_parsed)
      aterms_parsed <- aterms_parsed[!sapply(aterms_parsed, is.null)]
      # msg_var(aterms_parsed)
      aterms <-
        modifyList(
          aterms,
          aterms_parsed,
          keep.null = T
        )
      # msg_var(aterms)
    }

    # rhs_str <- rhs_dp(form)

    pred_vars <- find_bar_terms(form, invert = T)
    # msg_var(pred_vars)

    bterm <- find_bar_terms(form)
    # message('bterm: ', toString(bterm), ', length: ', length(bterm), ', is.na:', is.na(bterm), ', is.null: ', is.null(bterm))

    bterm_parsed <- if (length(bterm)>=1L) parse_block_term(bterm) else NULL
    # msg_var(bterm_parsed)

    f <-
      brms:::nlist(
        formula = form, resp_var, aterms, pred_vars, bterm = bterm_parsed
      )
    return(f)
  }

# unused?
remove_lhs_term <- function(formula, term_to_remove) {

  formula_str <- deparse(formula)
  term_to_remove <- trimws(term_to_remove)
  # Split the formula into LHS and RHS
  # Split formula at '~'
  parts <- strsplit(formula_str, "~")[[1]]
  lhs <- trimws(parts[1])
  rhs <- trimws(parts[2])

  # Split LHS at '|', if present
  lhs_parts <- strsplit(lhs, "\\|")[[1]]
  response <- trimws(lhs_parts[1])

  if (length(lhs_parts) == 1) {
    # No modifiers to remove
    return(formula_str)
  }

  modifiers_str <- lhs_parts[2]
  # Split modifiers by '+' and clean them
  modifiers <- strsplit(modifiers_str, "\\+")[[1]]
  modifiers <- trimws(modifiers)

  # Remove the target term
  modifiers <- modifiers[modifiers != term_to_remove]

  # Reconstruct LHS
  if (length(modifiers) == 0) {
    new_lhs <- response
  } else {
    new_lhs <- paste(response, "|", paste(modifiers, collapse = " + "))
  }

  # Return the cleaned formula string
  as.formula(paste(new_lhs, "~", rhs))
}



## parameter formula -------------------------------------------------------

is_transform_formula <-
  function(form, model_pars) {
    # message('is_transform_formula')
    # msg_var(form)
    if (!is.formula(form)) {
      return(F)
    }
    if(!is.twosided(form)) {
      return(F)
    }
    lhs_str <- lhs_dp(form, replace_ws = T)
    transform_func_pos <-
      regexpr('transform\\([^\\)]+\\)', lhs_str)
    transform_func_match <-
      regmatches(lhs_str, transform_func_pos)
    if(length(transform_func_match)) {
      transform_func_match <- transform_func_match[[1]]
    } else {return(F)}
    is_transform_form <-
      lhs_str==transform_func_match
    return(is_transform_form)
  }

normalize_transform_formula <-
  function(form) {
    if (!is.formula(form)) {
      stop(form, ' is not a valid formula, ',
           'and hence, not a valid transform formula.')
    }
    if(!is.twosided(form)) {
      stop(form, ' is not a valid transform formula, ',
           'make sure to define a lhs in the formula.')
    }
    lhs_str <- lhs_dp(lhs)
    lhs_str <- gsub('\\s', '', lhs_str)
    is_transform_form <-
      is_transform_formula(form)
    if(!is_transform_form) {
      stop(form, ' is not a valid transform formula, ',
           'lhs must of the form `transform(var1[+var2...]])`')
    }
    transform_vars_pos <-
      gregexpr('transform\\(([^\\)]+)\\)', lhs_str)
    transform_vars_match <-
      regmatches(lhs_str, transform_fun_pos)[[1]][1]
    form_normalized <-
      as.formula(paste0(transform_vars_match, '~', form[[3]]))
    return(form_normalized)
  }

validate_transform <-
  function(transform, replace_x = T, add_raw = T) {
    has_lhs <- is.twosided(transform)
    if (!has_lhs) {
      stop('Transforms must have a single variable on the lhs ',
           'indicating the parameter to transform')
    }
    lhs_var <- all.vars(transform[[2]])
    if (length(lhs_var)!=1L) {
      stop('Transforms must have a single variable on the lhs ',
           'indicating the parameter to transform')
    }
    rhs_vars <- all.vars(transform[[3]])
    lhs_in_rhs <- lhs_var%in%rhs_vars
    if(!lhs_in_rhs && !replace_x) {
      stop('Invalid transform sytax! ',
           'lhs value must be part of the rhs of the transform formula')
    }
    lhs_chr <- deparse(transform[[2]])
    rhs_chr <- deparse(transform[[3]])
    x_in_rhs <- 'x'%in%all.vars(transform[[3]])
    if(x_in_rhs&&replace_x) {
      rhs_chr_replaced <-
        gsub('(?<=[^A-z_\\.])x(?=[^A-z\\(_\\.])',
             lhs_var,
             rhs_chr,
             perl = T)
      transform <- as.formula(paste0(lhs_chr, ' ~ ', rhs_chr_replaced))
    }
    return(transform)
  }

#' Parse parameter and transformation formulas
#'
#' @param par_forms A \code{list()} of \code{formula} objects
#' @param par_transforms A \code{list()} of \code{formula} objects
#' @param model_pars A character vector defining the names of model parameters.
#'        Currently not used.
#' @param data A \code{data.frame} (or the like) used to check whether all the
#'        variables on the right-hand side of a formula in \code{par_forms} is
#'        present in the data. If not, the formula is returned as a non-linear
#'        formula
#'
#' @returns A \code{list()} of linear and non-linear formulas to be passed to
#'          \code{brms::brmsformula}
#' @importFrom brms nlf
parse_par_formulas <-
  function(par_forms, par_transforms, model_pars = NULL, data = NULL) {

    par_formulas <- list()
    for (form_nm in names(par_forms)) {
      # msg_var(form_nm)
      form <- par_forms[[form_nm]]
      form_env <- attr(form, '.Environment')
      transform <-
        if (form_nm%in%names(par_transforms)) {
          par_transforms[[form_nm]]
        } else {
          NULL
        }
      form_lhs_str <- lhs_dp(form)
      transform_lhs_str <- lhs_dp(transform)
      form_rhs_str <- rhs_dp(form)
      transform_rhs_str <- rhs_dp(transform)
      # message('form: ', deparse1(form), ' (lhs: ', form_lhs_str, ')')
      # message('transform: ', deparse1(transform), ' (lhs: ', transform_lhs_str, ')')

      if(!is.null(transform)) {
        par_transforms[[form_nm]] <- NULL

        transform_lhs_str <- lhs_dp(transform)
        transform_replacement_pattern <-
          paste0('(?<=[^A-z_\\.])(', transform_lhs_str, ')(?=[^A-z\\(_\\.])')
        transform <-
          formula_replace(transform, 'r',
                          transform_replacement_pattern, '\\1raw')

        form_replacement_pattern <-
          paste0(
            '(?:(?<=^)|(?<=[^A-Za-z_\\.]))(',
            form_lhs_str,
            ')(?:(?=$)|(?=[^A-z\\(_\\.]))'
            )
        form <-
          formula_replace(form, 'l',
                          form_replacement_pattern, '\\1raw')
        transform_lhs_str <- lhs_dp(transform)
        if(transform_lhs_str%in%names(par_formulas)) {
          warning('formula with key ', transform_lhs_str, ' ',
                  'does already exist: ',
                  par_formulas[[transform_lhs_str]], '! ',
                  'Now overriding with ', deparse1(transform), ' ...')
        }
        # message('transformed transform: ',
        #         deparse1(transform),
        #         ' (key: ', transform_lhs_str, ')')
        attr(transform, '.Environment') <- form_env
        par_formulas[[transform_lhs_str]] <-
          brms::nlf(transform, loop = F)
      }


      form_lhs_str <- lhs_dp(form)
      # message('transformed form: ',
      #         deparse1(form),
      #         ' (key: ', form_lhs_str, ')')

      if (!is.null(data)) {
        if (!all(all.vars(rhs(form))%in%names(data))) {
          warning(form, ' has variables not found in data, ',
                  'adding as non-linear formula')
          form <-
            brms::nlf(form, loop = F)

        }
      }
      if(form_lhs_str%in%names(par_formulas)) {
        warning('formula with key ', form_lhs_str, ' ',
                'already does already exist: ',
                par_formulas[[form_lhs_str]], '! ',
                'Now overriding with ', form, ' ...')
      }
      attr(form, '.Environment') <- form_env
      # msg_var(form)
      par_formulas[[form_lhs_str]] <-
        form
      # message('')
    }


    # message('par_transforms after for loop:')
    # str(par_transforms)

    for (form_nm in names(par_transforms)) {
      transform <- par_transforms[[form_nm]]
      transform_lhs_str <- lhs_dp(transform)
      if(transform_lhs_str%in%names(par_formulas)) {
        warning('formula with key ', transform_lhs_str, ' ',
                'already does exist: ',
                par_formulas[[transform_lhs_str]], '! ',
                'Now overriding with ', transform, ' ...')
      }
      par_formulas[[transform_lhs_str]] <-
        brms::nlf(transform, loop = F)
    }
    par_formulas <-
      lapply(par_formulas, \(x) {environment(x) <- NULL; x})
    return(par_formulas)
  }





# model formula -----------------------------------------------------------

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
        if(is.null(model_spec$block_var)) return(x)
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

    pred_vars <-
      sapply(
        resp_formula$pred_vars, validate_var,
        par = 'predictor', data = data,
        valid_value = validate_not_blockgrp
      )
    msg_var(pred_vars)

    block_vars <-
      sapply(
        resp_formula$aterms$block$block_vars, validate_var,
        par = 'block', data = data, null_ok = T
      )
    msg_var(block_vars)

    trial_var <-
      sapply(
        resp_formula$aterms$block$trial_var, validate_var,
        par = 'trial', data = data, null_ok = T
      )
    msg_var(trial_var)

    # extract formulas from dots
    dots_is_formula <- sapply(dots, is.formula)
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
    # msg_var(dots_formulas)
    # msg_var(dots)

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
    # msg_var(dots_par_form_list)
    # msg_var(dots_par_transform_list)

    # default parameter formulas
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
    data_vars <- c(resp_var, pred_vars, block_vars)
    data_vars <- c(resp_var, pred_vars, m$block_var)
    model_func_inputs <- c(data_vars, model_pars)
    model_function <- m$func_name
    response_formula <-
      as.formula(
        paste0(
          lhs_dp(formula), ' ~ ',
          model_function, '(', paste(model_func_inputs, collapse = ', '), ')'
        )
      )
    msg_var(response_formula)
    msg_var(parameter_formulas)


    bf_args_list <-
      c(formula = response_formula,
        parameter_formulas,
        list(
          family = m$family,
          nl = T,
          loop = F
        )
      )
    # msg_var(bf_args_list)
    model_form <-
      brms::do_call(brms::brmsformula, bf_args_list)
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
      brmsformula = model_form
    )
    class(mf) <- c('blmsmodelinfo', class(mf))
    message('=======================')
    return(mf)
  }
