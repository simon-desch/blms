lhs_call_info <-
  function(form, substitute_block_term = T) {
    if(!is.formula(form)) {
      stop('Please provide formula for model secification')
    }
    has_lhs <- is.twosided(form)
    if(!is.twosided(form)) {
      stop('The formula must have a lhs to define response variable')
    }
    lhs_str <- lhs_dp(form)

    lhs_first_bar_pos <-
      regexpr("\\s*\\|\\s*", lhs_str)
    lhs_parts <-
      regmatches(lhs_str, lhs_first_bar_pos, invert = T)[[1]]
    # msg_var(lhs_parts)
    resp_parts <-
      all.vars(as.formula(paste('~', lhs_parts[1]))[[2]])
    if(length(resp_parts)!=1L) {
      stop('Only one variable may be defined as response in the formula')
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
        regmatches(aterms_str, aterms_fun_pos, invert = F)#[[1]]
      aterms_fun_calls <- unlist(aterms_fun_calls, recursive = F)
      # msg_var(aterms_fun_calls)
      aterms_fun_names_pos <-
        regexpr('([a-z_]+)(?=\\()', aterms_fun_calls, perl = T)
      aterms_fun_names <-
        regmatches(aterms_fun_calls, aterms_fun_names_pos)
      # msg_var(aterms_fun_names)
      # msg_var(aterms_fun_calls)
      names(aterms_fun_calls) <- aterms_fun_names
      # msg_var(aterms_fun_calls)
      valid_aterms_funs <- c('block', 'dec')
      invalid_funs <- setdiff(aterms_fun_names, valid_aterms_funs)
      if (length(invalid_funs)>0) {
        warning('allowed funs in aterms: ',
                toString(paste0(valid_aterms_funs, '()')), '. ',
                'Other funs (', toString(paste0(invalid_funs, '()')), ' ',
                'will be ignored.')
      }
      actual_intersect <-
        aterms_fun_names[
          match(seq_along(aterms_fun_names),
                which(aterms_fun_names %in% valid_aterms_funs))
        ]
      # msg_var(actual_intersect)
      if (any(table(actual_intersect)>1)) {
        warning('Currently only one call to dec() or block() are allowed. ',
                'Doubled calls will be dropped.')
      }
      actual_intersect_idx <-
        which(actual_intersect%in%names(aterms_fun_calls))
      aterms_fun_calls <-
        # aterms_fun_calls[intersect(aterms_fun_names, valid_aterms_funs)]
        # aterms_fun_calls[actual_intersect]
        aterms_fun_calls[actual_intersect_idx]
      # msg_var(aterms_fun_calls)
      aterms_parsed <-
        mapply(
          parse_aterm_call,
          names(aterms_fun_calls),
          aterms_fun_calls,
          SIMPLIFY = F,
          USE.NAMES = T
        )
      # msg_var(aterms_parsed)
      aterms_parsed <- aterms_parsed[!sapply(aterms_parsed, is.null)]
      # msg_var(aterms_parsed)
      aterms_parsed <-
        mapply(\(x) aterms_parsed[names(aterms_parsed)%in%x],
               valid_aterms_funs)
      aterms <-
        modifyList(
          aterms,
          aterms_parsed,
          keep.null = T
        )
      # msg_var(aterms)
    }

    pred_vars <- all.vars(rhs(form))

    fixed_terms <- find_bar_terms(form, invert = T)
    # msg_var(pred_vars)

    group_terms <- find_bar_terms(form)
    msg_var(group_terms)
    # message('bterm: ', toString(bterm),
    #         ', length: ', length(bterm),
    #         ', is.na:', is.na(bterm),
    #         ', is.null: ', is.null(bterm))

    bterm_parsed <- NULL
    if (length(group_terms)>=1L) {
      stripped_group_terms <-
        lapply(group_terms, \(x) {
          block_term_pos <-
            regexpr('(?<=\\()([^\\)]+)(?=\\))', x, perl = T)
          block_term <- regmatches(x, block_term_pos)
        })
      bterm_parsed <-
        lapply(stripped_group_terms, parse_block_term, warn_missing_trial = F)
    }
    # msg_var(bterm_parsed)

    form_sub <- form
    if(!is.null(aterms$block) && substitute_block_term) {
      new_lhs <-
        paste(resp_var,
              ifelse(length(aterms$dec), paste('|', aterms$dec[[1]]$call), ''))
      form_sub <- as.formula(paste(new_lhs, '~', rhs_dp(form)))
      attr(form_sub, ".Environment") <- attr(form, ".Environment")
    }

    f <-
      brms:::nlist(
        formula = form_sub, resp_var, aterms, pred_vars,
        bterm = bterm_parsed, group_terms = group_terms, formula_org = form
      )
    return(f)
  }

parse_single_bbf <-
  function(form) {
    resp_parsed_form <- parse_response_formula2(form)
  }

parse_call <- function(
    expr, recursive = T, include_names = T, include_math_operators = F
) {
  if(!is.call(expr)&& !is.name(expr)) stop('expr must be a call or a name')
  # msg_var(expr)

  prev_call <- NULL
  # if (inherits(expr, 'formula')) prev_call <- expr[-1]
  call_info <- list()

  math_operators <- c("+", "-", "*", "/", "^", ":", "|", "~", "$", "[", "[[")
  group_calls <- c('(')
  brms_aterms_calls <- c('se', 'weights', 'subset', 'rate',
                         'cens', 'trunc', 'trials', 'thres', 'cat', 'dec')
  brms_ac_terms <- c( 'arma', 'ar', 'ma', 'cosy', 'unstr', 'sar', 'car', 'fcor')
  brms_ac_terms <- c(brms_ac_terms, paste0('cor_', brms_ac_terms))
  brms_pterms_calls <- c('s', 't2', 'mo', 'me', 'sd', 'mi', 'cs', brms_ac_terms)
  brms_gterms_calls <- c('gr', 'mm')
  brms_terms_calls <- c(brms_aterms_calls, brms_pterms_calls, brms_gterms_calls)

  has_nested_calls <- function(expr) {
    any(sapply(expr[-1], is.call))
  }

  is_exact_stan_match <- function(fn_name) {
    if(fn_name%in%c(math_operators, group_calls)) return(FALSE)
    if (!requireNamespace("rstan", quietly = TRUE)) return(NULL)
    matches <- tryCatch(rstan::lookup(fn_name), error = function(e) NULL)
    if (is.null(matches) || !is.data.frame(matches)) return(FALSE)
    any(matches$StanFunction == fn_name)
  }

  pick_first_or_null <-
    function(l) {
      if (length(l)>0) return(l[[1]]) else return(NULL)
    }

  parse_next_call <-
    function(expr, prev_call,
             to_call_info = T, recursive = T, include_math_operators = F) {
      this_call_info <-
        list(
          class = class(expr),
          expr = expr,
          prev_call = prev_call
        )
    if (is.call(expr) && as.character(expr[[1]]) %in%math_operators) {
      next_calls <-
        lapply(expr[-1], parse_next_call,
               prev_call = expr[1], recursive = recursive)
      this_call_info$next_calls <- pick_first_or_null(next_calls)
      this_call_info$is_math_operator <- T
      to_call_info <- include_math_operators
    } else if (is.call(expr) && is.symbol(expr[[1]])) {
      fn_name <- as.character(expr[[1]])
      # msg_var(expr)
      # msg_var(as.list(expr))
      # msg_var(fn_name)
      # msg_var(expr[-1])

      next_calls <-
        lapply(expr[-1], parse_next_call,
               prev_call = expr[1], to_call_info = F, recursive = F)
      this_call_info <-
        list(
          class = class(expr),
          expr = expr,
          prev_call = prev_call,
          next_calls = pick_first_or_null(next_calls),
          fun_name = fn_name,
          arguments = as.list(expr[-1]),
          has_nested_calls = has_nested_calls(expr),
          is_call = is.call(expr),
          is_math_operator = fn_name %in% math_operators,
          is_group_call = fn_name %in% group_calls,
          is_known_r = exists(fn_name, mode = "function", inherits = TRUE),
          is_known_stan = is_exact_stan_match(fn_name),
          is_brms_aterms_call = fn_name %in% brms_aterms_calls,
          is_brms_pterms_call = fn_name %in% brms_pterms_calls,
          is_brms_gterms_call = fn_name %in% brms_gterms_calls,
          is_block_call = fn_name == 'block',
          is_loop_call = fn_name == 'loop')
      this_call_info$is_brms_terms_call <-
        any(vapply(c('is_brms_aterms_call',
                     'is_brms_pterms_call',
                     'is_brms_gterms_call'),
                   \(x) isTRUE(this_call_info[[x]]), logical(1)))
      this_call_info$is_brms_rhs_call <-
        any(vapply(c('is_brms_pterms_call',
                     'is_brms_gterms_call'),
                   \(x) isTRUE(this_call_info[[x]]), logical(1)))
      this_call_info$is_unknown <-
        all(vapply(c('is_math_operator',
                     'is_known_r',
                     'is_known_stan'),
                   \(x) !isTRUE(this_call_info[[x]]), logical(1)))

    } else {
      # msg_var(expr)
      # msg_var(class(expr))
    }
    if (to_call_info)
      call_info[[length(call_info) + 1]] <<- this_call_info
    return(invisible(this_call_info))
  }

  parse_next_call(expr, prev_call)
  if (!include_names) {
    call_info <-
      Filter(\(x) x$class == 'call', call_info)
  }
  return(call_info)
}

formula_call_info <-
  function(formula, side = 'b',
           recursive = T,
           include_names = F,
           include_math_operators = F) {
    if (!inherits(formula, "formula")) stop("Input must be a formula.")
    side_expr <- formula
    if (grepl('^l', side)) side_expr <- lhs(formula)
    if (grepl('^r', side)) side_expr <- rhs(formula)
    parsed <- parse_call(side_expr,
                         recursive = recursive,
                         include_names = include_names,
                         include_math_operators = include_math_operators)
    return(parsed)
  }

call_info_rhs <-
  function(formula,
           recursive = T,
           include_names = F,
           include_math_operators = F) {
    formula_call_info(formula, 'r',
                      recursive = recursive,
                      include_names = include_names,
                      include_math_operators = include_math_operators)
  }

call_info_lhs <-
  function(formula,
           recursive = T,
           include_names = F,
           include_math_operators = F) {
    formula_call_info(formula, 'l',
                      recursive = recursive,
                      include_names = include_names,
                      include_math_operators = include_math_operators)
  }

replace_rhs_calls <- function(
    formula,
    call_info,
    pattern = NULL,
    replacement = NULL,
    regex_fun = identity,
    replace_all = TRUE
) {
  if (!inherits(formula, "formula")) stop("Input must be a formula.")

  rhs <- formula[[3]]
  lhs <- formula[[2]]
  replaced_count <- 0

  match_fn <- function(fn_name) {
    which(vapply(call_info, function(ci) identical(ci$fun_name, fn_name), logical(1)))
  }

  replace_fn_names <- function(expr) {
    if (is.call(expr) && as.character(expr[[1]]) == "+") {
      return(as.call(lapply(expr, replace_fn_names)))
    } else if (is.call(expr) && is.symbol(expr[[1]])) {
      fn_name <- as.character(expr[[1]])
      idx <- match_fn(fn_name)

      if (length(idx) == 0) return(expr)

      if (!replace_all && replaced_count >= 1) return(expr)

      new_fn <- if (!is.null(pattern)) {
        sub(pattern, replacement, fn_name, perl = TRUE)
      } else {
        regex_fun(fn_name)
      }

      expr[[1]] <- as.name(new_fn)
      replaced_count <<- replaced_count + 1
    }
    return(expr)
  }

  new_rhs <- replace_fn_names(rhs)
  new_formula <- as.formula(call("~", lhs, new_rhs))
  return(new_formula)
}

form_add_attr <-
  function(form) {
    lhs_info <- lhs_call_info(form)
    has_block <- length(lhs_info$aterms$block)>=1L
    if (has_block) {
      attr(form, 'has_block') <- T
      rhs_info <- call_info_rhs(form)
      rhs_info <-
        Filter(\(x) !any(isTRUE(x$is_math_operator),
                         isTRUE(x$is_brms_rhs_call),
                         isTRUE(x$is_group_call),
                         isTRUE(x$is_block_call)),
               rhs_info)
      if(length(rhs_info)) {
        attr(form, 'has_funcs') <- T
      }
    }
    return(form)
  }
brms_validate_data_2 <-
  function(formula, data, additional_vars = NULL) {
    bterms <- brms:::brmsterms(formula)
    all_vars_form <- bterms$allvars
    all_vars_form_ <- all_vars_form
    if(!is.null(additional_vars)) {
      all_vars_form_ <-
        formula_replace(bterms$allvars, 'r', '$',
                        paste('+', paste(additional_vars, collapse = '+')))
    }
    bterms$allvars <- all_vars_form_
    m_dat <-
      brms:::validate_data(data,
                           bterms = bterms,
                           data_name = substitute(data))
    return(m_dat)
  }

get_data_stanvars_for_block <-
  function(data, base_form, block_info, idx) {
    # should change to check whether it can be coerced to data.frame
    if(!inherits(data, 'data.frame')) {
      stop('invalid data argument')
    }
    if(!inherits(base_form, 'brmsformula')) {
      stop('need brmsformula to validate data')
    }
    if(!'block_vars'%in%names(block_info)) {
      stop('block_info need to provide at least block_vars')
    }
    if(idx%%1!=0 || idx <= 0 ) {
      stop('idx must be a positive integer')
    }
    block_vars <- c('condition', 'subject')
    block_vars <- block_info$block_vars #c('condition', 'subject')
    block_vars_rev <- rev(block_vars)
    trial_var <- # 'trial_no'
      if('trial_var'%in%names(block_info))
        block_info$trial_var else NULL
    order_vars <- c(block_vars_rev, trial_var)
    bterms <- brms:::brmsterms(base_form)
    all_vars_form <- bterms$allvars
    all_vars_form_ <-
      blms:::formula_replace(bterms$allvars, 'r', '$',
                      paste('+', paste(order_vars, collapse = '+')))
    bterms$allvars <- all_vars_form_
    m_dat <-
      brms:::validate_data(data,
                           bterms = bterms,
                           data_name = substitute(data))
    m_dat$N <- 1:nrow(m_dat)
    o_dat <-
      m_dat[do.call(what = order, args = m_dat[, order_vars]), ]
    o_dat <-
      order_df(m_dat, order_vars)

    o_dat_split <-
      split(o_dat, o_dat[, block_vars_rev], drop = T, lex.order = T)
    names(o_dat_split)
    G <- length(o_dat_split)
    GN <- sapply(o_dat_split, nrow)
    GN_max <- max(GN)
    gn_idx <- lapply(o_dat_split, \(x) x$N)
    GN_idx <-
      array(0,
            dim = c(G, GN_max),
            dimnames = list(g=paste0('g', 1:G), n=paste0('n', 1:GN_max))
            )
    for (g in 1:G) {
      GN_idx[g, 1:GN[g]] <- gn_idx[[g]]
    }

    svars <-
      stanvar(
        x = G, name = paste0('G_', idx),
        scode =
          paste0('G_', idx, '; // number of grouping levels')
        ) +
      stanvar(
        x = GN, name = paste0('GN_', idx),
        scode =
          paste0('array[G] GN_', idx, '; // number of N per level')
        ) +
      stanvar(
        x = GN_max, name = paste0('GN_max_', idx),
        scode =
          paste0('GN_max_', idx, '; // max N per level')
        ) +
      stanvar(
        x = GN_idx, name = paste0('GN_idx_', idx),
        scode =
          paste0('array[G_', idx, ', GN_max_', idx, '] int GN_idx_', idx, '; ',
                 '// row numbers per level')
        )
    list(G = G, GN = GN, GN_max = GN_max, GN_idx = GN_idx,
         idx = idx, stanvars = svars)
  }

replace_form <-
  function(to_replace, with_form) {
    out <- with_form
    attributes(out) <- attributes(to_replace)
    return(out)
  }
parse_stan_functions <- function(code_string) {
  # Remove single-line comments
  code_lines <- unlist(strsplit(code_string, "\n"))
  code_no_comments <- sub("//.*$", "", code_lines)
  code_clean <- paste(code_no_comments, collapse = "\n")

  # extract a balanced block
  extract_block <- function(text, open = "(", close = ")") {
    depth <- 1
    result <- ""
    for (i in seq_len(nchar(text))) {
      ch <- substr(text, i, i)
      if (ch == open) depth <- depth + 1
      if (ch == close) depth <- depth - 1
      result <- paste0(result, ch)
      if (depth == 0) {
        return(list(content = substr(result, 1, nchar(result) - 1), len = i))
      }
    }
    return(NULL)
  }

  # Find function-like declarations
  pattern <- "\\b([a-zA-Z0-9_\\[\\],() ]+)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\("
  matches <- gregexpr(pattern, code_clean, perl = TRUE)[[1]]
  if (matches[1] == -1) return(list())

  results <- list()

  for (i in seq_along(matches)) {
    start <- matches[i]
    header <- substr(code_clean, start, min(nchar(code_clean), start + 300))
    m <- regexec(pattern, header, perl = TRUE)
    parts <- regmatches(header, m)[[1]]

    if (length(parts) < 3 || any(is.na(attr(m[[1]], "match.length")))) next

    return_type <- trimws(parts[2])
    func_name <- trimws(parts[3])
    if (func_name %in% c("if", "for", "while", "else")) next

    match_len <- attr(m[[1]], "match.length")[1]
    after_fn <- substr(code_clean, start + match_len, nchar(code_clean))

    # Extract argument list
    arg_block <- extract_block(after_fn, "(", ")")
    if (is.null(arg_block)) next
    arg_string <- arg_block$content

    # Extract body
    after_args <- substr(after_fn, arg_block$len + 1, nchar(after_fn))
    brace_pos <- regexpr("\\{", after_args)[[1]]
    if (brace_pos == -1) next
    body_block <- extract_block(substr(after_args, brace_pos + 1, nchar(after_args)), "{", "}")
    if (is.null(body_block)) next

    full_func <- substr(code_clean, start, start + match_len + arg_block$len + brace_pos + body_block$len)

    # NEW: Robust argument splitter
    split_args <- function(arg_str) {
      args <- list()
      buf <- ""
      depth <- 0
      for (i in seq_len(nchar(arg_str))) {
        ch <- substr(arg_str, i, i)
        if (ch %in% c("(", "[")) depth <- depth + 1
        if (ch %in% c(")", "]")) depth <- depth - 1
        if (ch == "," && depth == 0) {
          args <- append(args, list(trimws(buf)))
          buf <- ""
        } else {
          buf <- paste0(buf, ch)
        }
      }
      if (nchar(buf) > 0) args <- append(args, list(trimws(buf)))
      return(args)
    }

    arg_list <- split_args(arg_string)
    parsed_args <- list()
    for (arg in arg_list) {
      arg <- trimws(arg)
      arg_match <- regexec("^(.+?)\\s+([a-zA-Z_][a-zA-Z0-9_]*)$", arg)
      parts <- regmatches(arg, arg_match)[[1]]
      if (length(parts) == 3) {
        parsed_args <- append(parsed_args, list(list(
          type = parts[2],
          name = parts[3]
        )))
      } else {
        parsed_args <- append(parsed_args, list(list(
          type = "<could_not_parse>",
          name = arg
        )))
      }
    }

    results[[length(results) + 1]] <- list(
      return_type = return_type,
      name = func_name,
      args = parsed_args,
      code = full_func
    )
  }

  return(results)
}

get_functions_block_stanvars <-
  function(svar) {
    fun_svar <-
      Filter(\(x) x$block == 'functions', svar)
    if(length(svar)==0) return(NULL)
    return(fun_svar)
  }

get_stan_fun_defs <-
  function(svars) {
    func_stanvars <-
      get_functions_block_stanvars(svars)
    stan_fun_defs <-
      lapply(lapply(stavars, '[[', 'scode'), parse_stan_functions)
    stan_fun_defs <-
      unlist(stan_fun_defs, recursive = F)
    return(stan_fun_defs)
  }

get_stanfun_header <-
  function(fun_def) {
    paste0(
      fun_def$return_type, ' ', fun_def$name, '(',
      paste(
        sapply(fun_def$args, paste, collapse = ' '),
        collapse = ', '),
      ')'
      )
  }

formula_replace_block_call <-
  function(form) {
    form <- formula_replace(form, side = 'r',
                                pattern = '\\+*\\s*block\\([^\\]]+\\)',
                                replacement = '')
    form <- formula_replace(form, side = 'r',
                                pattern = '^\\s*\\+*\\s*',
                                replacement = ' ')
    form <- formula_replace(form, side = 'r',
                                pattern = '\\s*\\+*\\s*$',
                                replacement = ' ')
    form <- formula_replace(form, side = 'l',
                                pattern = '[\\+|]*\\s*block\\([^\\]]+\\)',
                                replacement = '')
    form <- formula_replace(form, side = 'l',
                                pattern = '^\\s*\\+*\\s*',
                                replacement = ' ')
    form <- formula_replace(form, side = 'l',
                                pattern = '\\s*[\\+\\|]*\\s*$',
                                replacement = ' ')
  }

block_fun_stanvar_for_fun_def <-
  function(fun_def) {
    fun_name <- fun_def$name
    new_fun_name <- paste0(fun_name, '_group')
    new_fun_def <- fun_def
    new_fun_def$name = new_fun_name
    new_fun_def$args <-
      c(list(
        list(type = 'array[] int', name = 'GN'),
        list(type = 'array[,] int', name = 'GN_idx')
      ), new_fun_def$args)
    new_fun_header <- get_stanfun_header(new_fun_def)
    vector_index_str <- '[GN_idx[g, 1:GN[g]]]'
    indexed_args_str <-
      paste0(
        sapply(fun_def$args, '[[', 'name'),
        vector_index_str,
        collapse = ', ')
    fun_args <-
      new_fun_lines <- c(
        paste0('  ', new_fun_header, ' {'),
        paste0('    int G = size(GN);'),
        paste0('    int N = sum(GN);'),
        paste0('    vector [N] rv = rep_vector(0.0, N);'),
        paste0('    for g in 1:G {'),
        paste0('      vector[GN[g]] grv;'),
        paste0('      grv = ', fun_name, '(', indexed_args_str, ');'),
        paste0('      rv', vector_index_str, ' = grv;'),
        paste0('    }'),
        paste0('    return rv;'),
        paste0('  }')
      )
    new_fun_str <- paste(new_fun_lines, collapse = '\n')
    fun_stanvar <- stanvar(scode = new_fun_str, block = 'functions')
    return(fun_stanvar)
  }


parse_block_formula <-
  function(form, svars, data, base_form, block_count) {

    out <-
      list(formula = form,
           stanvars = NULL,
           block_count = block_count)
    func_stanvars <-
      get_functions_block_stanvars(svars)
    stan_fun_defs <-
      lapply(lapply(svars, '[[', 'scode'), parse_stan_functions)
    stan_fun_defs <-
      unlist(stan_fun_defs, recursive = F)
    msg_var(form)

    block_info <- NULL
    lhs_info <- lhs_call_info(form)
    has_block <- length(lhs_info$aterms$block)>=1L
    if(length(lhs_info$aterms$block)>1L) {
      warning('multiple block calls are not supported, ',
              'only the first one will be used')
    }
    if(has_block) block_info <- lhs_info$aterms$block[[1]]

    lhs_info <- call_info_lhs(form)
    rhs_info <- call_info_rhs(form)
    rhs_calls_n <- length(rhs_info)

    rhs_block_call <- NULL
    rhs_block_calls <-
      Filter(\(x) any(isTRUE(x$is_block_call)),
             rhs_info)
    rhs_block_calls_n <- length(rhs_block_calls)
    has_block_call <- rhs_block_calls_n>0
    if (has_block_call)
      rhs_block_call <- rhs_block_calls[[1]]
    msg_var(rhs_block_call)
    if(has_block_call)
      block_info <- parse_block_call(deparse(rhs_block_call$expr))
    if(!any(has_block, has_block_call))
      return(out)

    rhs_fun_info <-
      Filter(\(x) !any(isTRUE(x$is_math_operator),
                       isTRUE(x$is_brms_rhs_call),
                       isTRUE(x$is_group_call),
                       isTRUE(x$is_block_call)
                       ), rhs_info)
    names(rhs_fun_info) <- unlist(Map(\(x) x$fun_name, rhs_fun_info))

    # add info from svars if a match is found for the function
    rhs_fun_info <-
      Map(\(x) {
        matching_funs <- Filter(\(sv) sv$name == x$fun_name, stan_fun_defs)
        # should be only one,
        # but Stan allows function overloading, double check!!
        x$stan_fun_def <- matching_funs
        x
      }, rhs_fun_info)
    # give warning for non-matched functions
    funs_not_found <- Filter(\(x) length(x$stan_fun_def)==0, rhs_fun_info)
    for (fun in funs_not_found) {
      warning('Couldn\'t find definition for ', fun$fun_name,
              '() in stanvars. ',
              'Blocking of functions is currently only supported for ',
              'functions defined in stanvars. You can wrap other functions ',
              'in a wrapper function and add this to stanvars.')
    }
    # remove functions without match definition in svars
    rhs_fun_info <- Filter(\(x) length(x$stan_fun_def)>0, rhs_fun_info)

    # check whether function definitions are valid, warn and remove
    rhs_fun_info <- Map(\(x) {
      invalid_funs_idx <- c()
      for (i in 1:length(x$stan_fun_def)) {
        fun_def <- x$stan_fun_def[[i]]
        if(fun_def$return_type!='vector') {
          warning('Cannot use function definition\n',
                  get_stanfun_header(fun_def), '\n',
                  'Currently only functions with return type \'vector\' ',
                  'are supported (as this is what\'s usually used in brms). ')
          invalid_funs_idx <- c(invalid_funs_idx, i)
        }
        if(length(fun_def$args)!=length(x$arguments)) {
          warning('Cannot use function definition\n',
                  get_stanfun_header(fun_def), '\n',
                  'Number of input arguments in Stan function (',
                  length(fun_def$args), ') does not match the number of ',
                  'input arguments of the function defined in the formula.')
          invalid_funs_idx <- c(invalid_funs_idx, i)
        }
      }
      if (length(invalid_funs_idx))
        x$stan_fun_def <- x$stan_fun_def[-invalid_funs_idx]
      return(x)}, rhs_fun_info)
    funs_not_found <- Filter(\(x) length(x$stan_fun_def)==0, rhs_fun_info)
    for (fun in funs_not_found) {
      warning('Couldn\'t find valid definition for ', fun$fun_name,
              '() in stanvars.')
    }
    # remove functions without valid match definition in svars
    rhs_fun_info <- Filter(\(x) length(x$stan_fun_def)>0, rhs_fun_info)

    rhs_fun_calls_n <- length(rhs_info)
    new_form <- form
    msg_var(new_form)
    new_form <- formula_replace_block_call(new_form)
    msg_var(new_form)
    new_svars <- list()
    class(new_svars) <- 'stanvars'
    if (rhs_fun_calls_n>0) {
      block_count = block_count + 1
      base_form2 <- base_form
      base_form2$formula <- formula_replace_block_call(base_form2$formula)
      block_info_svar <-
        get_data_stanvars_for_block(data, base_form2, block_info, block_count)
      new_svars <- new_svars + block_info_svar$stanvars

      for (fun_info in rhs_fun_info) {
        fun_name <- fun_info$fun_name
        new_fun_name <- paste0(fun_info$fun_name, '_block')
        new_form <-
          formula_replace(new_form, 'r',
                          pattern = paste0(fun_name, '\\('),
                          replacement = paste0(fun_name, '(GN_', block_count,
                                               ', GN_idx_', block_count, ', '))
        new_form <-
          replace_rhs_calls(new_form, rhs_fun_info,
                            pattern = '$', replacement = '_block')
        for (fun_def in fun_info$stan_fun_def) {
          fun_stanvar <-
            block_fun_stanvar_for_fun_def(fun_def)
          new_svars <- new_svars + fun_stanvar
        }
      }
    }
    attr(new_form, '.Environment') <- attr(form, '.Environment')
    ret_val <-
      list(
        formula = new_form,
        stanvars = new_svars,
        block_count = block_count
      )
    return(ret_val)
  }

parse_blocked_formulas <-
  function(form, svars, data) {
    if(!inherits(form, 'brmsformula')) {
      stop('parsing of blocked formulas should be done on brmsformula objects')
    }
    if(!inherits(form, 'blmformula')) {
      form <- blmformula(form)
    }
    if(length(svars)>0 &&!inherits(svars, 'stanvars')) {
      stop('svars needs to be a list of class \'stanvars\' ',
           'to parse blocked formulas')
    }

    block_count <- 0
    new_svars <- svars

    formula_block_info <-
      parse_block_formula(form$formula, svars, data, form, block_count)
    block_count <- formula_block_info$block_count
    if (length(formula_block_info$stanvars) > 0)
      new_svars <- new_svars + formula_block_info$stanvars
    form$formula <- formula_block_info$formula
    form$formula_block_info <- formula_block_info
    pforms_block_infos <- list()
    for (i in 1:length(form$pforms)) {
      pform_block_info <-
        parse_block_formula(form$pform[[i]], svars, data, form, block_count)
      block_count <- pform_block_info$block_count
      if (length(pform_block_info$stanvars) > 0)
        new_svars <- new_svars + pform_block_info$stanvars
      form$pform[[i]] <- pform_block_info$formula
      pforms_block_infos[length(pforms_block_infos)+1] <-
        pform_block_info
    }
    form$pforms_block_info <- formula_block_info

  }
parse_blm_formula <-
  function(form) {
    if(!inherits(form, 'blmformula')) {
      stop('form must be of class \'blmformula\'')
    }

  }
