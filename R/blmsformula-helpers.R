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
#' @export
#'
is.formula <-
  function(x) {is.call(x) && x[[1]] == quote(`~`)}

#' Test if input is \code{formula} or \code{list}
#' (i.e. \code{brms::nlf, brms::lf}) defining one
#'
#' @param x An to be tested for its \code{formula}
#'
#' @returns \code{TRUE} if \code{x} is a formula (also returns \code{TRUE} for
#'          \code{brms::nlf} and \code{brms::lf} objects), else \code{FALSE}
#' @export
#'
is_formula <- function(x) {
  if (is.formula(x)) return(T)
  if (is.list(x) && class(x[[1]]) == 'formula') return(T)
  return(F)
}

#' Get the \code{formula} from. \code{x}
#'
#' @param x
#'
#' @returns a \code{formula} object if \code{is_formula(x) == TRUE} else
#'          \code{NULL}
#' @export
#'
get_formula <-
  function(x) {
    if (is.formula(x)) return(x)
    if (is.list(x) && class(x[[1]]) == 'formula') return(x[[1]])
    return(NULL)
  }

#' Set the \code{formula} of \code{x}
#'
#' @param x \code{formula}
#' @param new_x new \code{formula}
#'
#' @returns \code{new_x} with attributes of \code{x}
#' @export
#'
set_formula <-
  function(x, new_x) {
    attributes(new_x) <- attributes(x)
    if (is.formula(x)) return(new_x)
    if (is.list(x) && class(x[[1]]) == 'formula') {
    attributes(new_x) <- attributes(x[[1]])
      x[[1]] <- new_x
      return(x)
    }
    return(new_x)
  }

#' Get the left-hand side of a formula
#'
#' @param f A \code{formula} object
#'
#' @returns The left-hand side of the formula, if it has one,
#'          otherwise \code{NULL}
lhs <-
  function(f) {
    f <- get_formula(f)
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
    f <- get_formula(f)
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
#' @param form a formula, possibly containing multiple variables (typically
#'        combined by \code{+}) on its left-hand side
#'
#' @returns A \code{list()} containing \code{formula} objects created by
#'          splitting the input formula into multiple formulas if multiple
#'          variables are listed on it left-hand side
#' @export
#'
#' @details
#' Variables on the left-hand side of the formula are extracted using
#' \code{all.vars}
#'
#' @examples split_by_lhs(a + b ~ c)
#'
split_by_lhs <-
  function(form) {
    # print(form)
    form_in <- form
    form <- get_formula(form)
    is_transform <- is_transform_formula(form)
    lh_vars <- all.vars(lhs(form))
    names(lh_vars) <- lh_vars
    # msg_var(lh_vars)
    rhs_str <- rhs_dp(form)
    out_forms <-
    lapply(
      lh_vars,
      \(x) {
        outform <- as.formula(paste0(ifelse(is_transform,
                                            paste0('transform(', x, ')'), x),
                                     ' ~ ', rhs_str))
        attributes(outform) <- attributes(form_in)
        return(set_formula(form_in, outform))
      }
      )
    # if(is.null(names(out_forms))) names(out_forms) <- lh_vars
    return(out_forms)
  }

#' Split each formula in a list into multiple formulas based on variables
#' defined in the left-hand side of the formula
#'
#' @param flist A \code{list()} containing formulas, each possibly containing
#'        multiple variables (typically combined by \code{+}) on its
#'        left-hand side
#'
#' @returns A \code{list()} containing \code{formula} objects created by
#'          splitting the input formulas into multiple formulas if multiple
#'          variables are listed on their left-hand side
#' @export
#'
#' @details
#' Variables on the left-hand side of the formula are extracted using
#' \code{all.vars}
#'
#' @examples split_flist_by_lhs(list(a + b ~ c, d ~ e))
#'
split_flist_by_lhs <-
  function(flist) {
    unlist(lapply(unname(flist), split_by_lhs))
  }

#' Set specific environment for an object
#'
#' @param obj An object.
#' @param envir An \code{environment} object.
#'
#' @returns \code{obj} with attribute \code{'.Environment'} set to \code{envir}
#' @export
#'
obj_set_env <-
  function(obj, envir){
    attr(obj, '.Environment') <- envir
    return(obj)
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
      stop('Invalid variable definition: ', var, '. ',
           'Must be either character of length 1 ',
           'or right hand sided formula ',
           'specifying the pariable coding the ', par)
    }

    if(!is.null(data)) {
      in_data <- out%in%names(data)
      if(!in_data) {
        stop('Variable `', out, '` not found in data.')
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
    cond_var <- NULL
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

      if(length(block_call_parts)==3) {
        cond_var <-
          all.vars(as.formula(paste0('~', block_call_parts[3]))[[2]])
      }
      if(length(block_call_parts)>3) {
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
    if (length(cond_var)>1L) {
      warning('multiple variables listed for condition definition ',
              'only the first, ', cond_var[1], ', will be used')
      cond_var <- cond_var[1]
    }
    type <- 'block'
    brms:::nlist(type, trial_var, block_vars, cond_var, term = block_term)
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
      nlist(
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
    lhs_str <- lhs_dp(form, replace_ws = T)
    rhs_str <- rhs_dp(form)
    is_transform_form <-
      is_transform_formula(form)
    if(!is_transform_form) {
      stop(form, ' is not a valid transform formula, ',
           'lhs must of the form `transform(var1[+var2...]])`')
    }
    lhs_var <- all.vars(lhs(form))
    if (length(lhs_var)!=1L) {
      message('form: ', form)
      message('lhs_var: ', lhs_var)
      stop('Transforms must have a single variable on the lhs ',
           'indicating the parameter to transform.')
    }
    new_lhs_str <- gsub('transform\\(([^\\)]+)\\)', '\\1', lhs_str, perl = T)
    form_normalized <-
      as.formula(paste0(new_lhs_str, '~', rhs_str))
    return(form_normalized)
  }

validate_transform <-
  function(transform, replace_x = T, add_raw = T, replace_call = F) {
    has_lhs <- is.twosided(transform)
    if (!has_lhs) {
      stop('Transforms must have a single variable on the lhs ',
           'indicating the parameter to transform')
    }
    lhs_var <- all.vars(lhs(transform))
    if (length(lhs_var)!=1L) {
      stop('Transforms must have a single variable on the lhs ',
           'indicating the parameter to transform.')
    }
    rhs_vars <- all.vars(rhs(transform))
    lhs_in_rhs <- lhs_var%in%rhs_vars
    if(!lhs_in_rhs && !replace_x) {
      stop('Invalid transform syntax! ',
           'lhs value must be part of the rhs of the transform formula')
    }
    lhs_chr <- lhs_dp(transform)
    rhs_chr <- rhs_dp(transform)
    x_in_rhs <- 'x'%in%rhs_vars
    if(x_in_rhs&&replace_x) {
      transform <- formula_replace(transform, 'r', '\\bx\\b', lhs_var)
    }
    if (replace_call) {
      transform <- normalize_transform_formula(transform)
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
      form_rhs_str <- rhs_dp(form)
      # message('form: ', deparse1(form), ' (lhs: ', form_lhs_str, ')')

      if(!is.null(transform)) {
        transform_lhs_str <- lhs_dp(transform, remove_ws = T)
        transform_rhs_str <- rhs_dp(transform, remove_ws = T)
        # message('transform: ', deparse1(transform),
        #         ' (lhs: ', transform_lhs_str, ')')

        par_transforms[[form_nm]] <- NULL

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
      # lapply(par_formulas, \(x) {environment(x) <- NULL; x})
    return(par_formulas)
  }

pred_vars_model_func_call <-
  function(call_info, model_spec) {
    if (length(call_info)==1) call_info <- call_info[[1]]
    args <- call_info$args
    if (length(args) != length(model_spec$func_input)) return(NULL)
    unlist(args[2:(length(model_spec$fun_data)+1)])
  }

substitute_model_func <-
  function(in_form, model_spec, resp_var, dec_var) {

    # resp_var <- all.vars(lhs(in_form))[1]

    pred_vars <- NULL
    n_pred_vars <- length(model_spec$pred_vars)
    rhs_call_info <- call_info_lhs(in_form)
    rhs_call_model_func <- Filter(\(x)x$fun_name == model_spec$func_name,
                                  rhs_call_info)
    if (length(rhs_call_model_func)) {

      if (length(rhs_call_model_func)==1) {
        pred_vars <- pred_vars_model_func_call(rhs_call_model_func, model_spec)
        if(!is.null(pred_vars)) {
          message('Parameter formula for parameter already contains model ',
                  'function.')
          out_form <- in_form
          attributes(out_form) <- attributes(in_form)
          attr(out_form, 'loop') <- F
          attr(out_form, 'nl') <- T
          return(list(formula = out_form, pred_vars = pred_vars))
        } else {
          stop('Model func has wrong number of input arguments.')
        }
      } else {
        stop('Cannot call model func twice.')
      }
    } else {
      pred_vars <- all.vars(rhs(in_form))
      # msg_var(pred_vars)
      if(length(pred_vars)!=n_pred_vars) {
        stop('Number of arguments for model func \'', model_spec$func_name,
             '()\' (', length(pred_vars), ') specified in formula \n',
             toString(in_form), '\ndoes not match ',
             'the number of expected arguments defined in model_spec (',
             'n=', n_pred_vars, ': ', toString(model_spec$pred_vars), ').')
      }
      use_blm <- 'use_blm'%in%names(model_spec) && model_spec$use_blm
      # msg_var(use_blm)
      block_var <- NULL
      if(!use_blm && 'use_blm'%in%names(model_spec)) {
        block_var <- model_spec$block_var
      }
      data_vars <- c(
        block_var,
        ifelse(is.null(dec_var), resp_var, dec_var),
        pred_vars
      )
      # msg_var(data_vars)
      model_pars <- model_spec$func_params
      model_func_inputs <- c(data_vars, model_pars)

      out_form <- as.formula(paste0(lhs_dp(in_form), ' ~ ',
                                    model_spec$func_name, '(',
                                    paste(model_func_inputs, collapse = ', '),
                                    ')'))
      attributes(out_form) <- attributes(in_form)
      attr(out_form, 'loop') <- F
      attr(out_form, 'nl') <- T
      return(list(formula = out_form, pred_vars = pred_vars))
    }
  }

parse_brms_blms <-
  function(brmsformula, model_spec) {
    resp_var <- all.vars(lhs(brmsformula))[1]
    lhs_call_info <- call_info_lhs(brmsformula$formula)
    dec_call <- Filter(\(x)x$fun_name == 'dec', lhs_call_info)
    n_pred_vars <- length(model_spec$pred_vars)
    dec_var <- NULL
    pred_vars <- NULL
    if (length(dec_call)>0) {
      if (length(dec_call)>1L) {
        warning('Only single definition of decision variable by dec() ',
                'is allowed. Further calls will be ignored (and might ',
                'trigger an error in brms).')
      }
      dec_call <- dec_call[[1]]
      dec_var <- all.vars(dec_call$arguments[[1]])[1]
    }
    if (model_spec$func_par == 'mu') {
      in_form <- brmsformula$formula
      form_info <-
        substitute_model_func(in_form, model_spec, resp_var, dec_var)
      out_form <- form_info$formula
      pred_vars <- form_info$pred_vars
      brmsformula$formula <- out_form
    } else {
      func_par_available <- model_spec$func_par %in% names(brmsformula$pforms)
      if(!func_par_available) {
        stop('Parameter formula for parameter \'', model_spec$func_par, '\', ',
             'supposed to contain the model function, not found.')
      }
      func_par <- model_spec$func_par
      fform <- NULL
      fform_rhs_vars <- NULL
      while(T) {
        fform <- brmsformula$pforms[[func_par]]
        # msg_var(fform)
        if(is.null(fform)) break
        fform_rhs_vars <- all.vars(rhs(fform))
        # msg_var(fform_rhs_vars)
        #ToDo:
        # double check, may be better to not allow this
        if (paste0(func_par, 'raw')%in%fform_rhs_vars) {
          func_par <- paste0(func_par, 'raw')
        } else {
          break
        }
      }
      if(is.null(fform)) {
        stop('Parameter formula for parameter \'', model_spec$func_par, '\', ',
             'supposed to contain the model function, not found.')
      }
        # msg_var(fform)
      in_form <- fform
      form_info <-
        substitute_model_func(in_form, model_spec, resp_var, dec_var)
      out_form <- form_info$formula
      pred_vars <- form_info$pred_vars
      brmsformula$pforms[[func_par]] <- out_form
    }
    return(list(formula = brmsformula, pred_vars = pred_vars))
  }


