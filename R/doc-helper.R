
#' Create md formatted table from \code{data.frame}
#'
#' @details Copied from roxygen2 documentation
#'
#' @param df A \code{data.frame}
#' @param ... Formatting options.
#'
#' @returns Formatted text representation of the input \code{data.frame}.
#'
#' @examples cat(tabular(mtcars[1:5, 1:5]))
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'   ")))

  paste0("#' \\tabular{", paste(col_align, collapse = ""), "}{\n#'   ",
         paste0("\\strong{", names(df), "}", sep = "", collapse = " \\tab "),
         " \\cr\n#'   ", contents, "\n#' }\n")
}

#' Create md formatted table from \code{data.frame}
#'
#' @details Adapted from roxygen2 documentation
#'
#' @param df A \code{data.frame}
#' @param ... Formatting options.
#'
#' @returns Formatted text representation of the input \code{data.frame}.
#'
#' @examples cat(tabular(mtcars[1:5, 1:5]))
tabular2 <- function(df, ...) {
  stopifnot(is.data.frame(df))
  col_names <- names(df)
  df <-
    data.frame(
      sapply(rownames(df),
             \(x)paste0('\\strong{\\eqn{', parse_param_name(x), '}}')),
      df)
  names(df) <- c('', col_names)
  cols <- lapply(df, format, ...)
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n")))
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n",
        paste0("\\strong{", names(df), "}", sep = "", collapse = " \\tab "),
        " \\cr\n", contents, "\n   }\n"
  )
}

#' Parse parameter names of a model to Latex code
#'
#' @param x A \code{character}.
#'
#' @returns \code{x} with greek letters and subscripts formatted for
#'          \code{"\\eqn{}"}
#'
#' @examples parse_param_name('alphapos')
parse_param_name <-
  function(x) {
    greeks <-
      c('alpha', 'Alpha',
        'beta', 'Beta',
        'gamma', 'Gamma',
        'delta', 'Delta',
        'epsilon', 'Epsilon',
        'zeta', 'Zeta',
        'eta', 'Eta',
        'theta', 'Theta',
        'iota', 'Iota',
        'kappa', 'Kappa',
        'lambda', 'Lambda',
        'mu', 'Mu',
        'nu', 'Nu',
        'xi', 'Xi',
        'omicron', 'Omicron',
        'pi', 'Pi',
        'rho', 'Rho',
        'sigma', 'Sigma',
        'tau', 'Tau',
        'upsilon', 'Upsilon',
        'phi', 'Phi',
        'chi', 'Chi',
        'psi', 'Psi',
        'omega', 'Omega')
    lowered <- c('pos', 'neg')
    parsed <-
      gsub(paste0('(', paste(greeks, collapse = '|'), ')'), '\\\\\\1', x)
    parsed <-
      gsub(paste0('(', paste(lowered, collapse = '|'), ')'), '_{\\1}', parsed)
    return(parsed)
    }

#' Create string to display parameter names using Latex equation
#'
#' @param model_spec A \code{model_spec} object.
#'
#' @returns A string with formatting for parameter names.
#'
#' @examples parameter_names_parsed_string(get_model_spec('ql_a_it'))
parameter_names_parsed_string <-
  function(model_spec) {
    parsed_names <-
      sapply(names(get_model_spec('ql_2a_it')$parameters), parse_param_name)
    eqned_names <-
      sapply(parsed_names, \(x) paste('\\eqn{', x, '}'))
    toString(eqned_names)
  }


#' Get list of model parameters for details section of roxygen2 md
#'
#' @param ms A \code{model_spec} object.
#'
#' @returns Formatted list for detals section of roxygen2 md
#'
#' @examples
#' parameters_md_list(model_spec)
parameters_md_list <-
  function(ms) {
    lines <- c('\\itemize{')
    for (par in names(ms$parameters)) {
      par_def <- ms$parameters[[par]]
      line <- paste0('\\item \\strong{\\eqn{', parse_param_name(par), '}}')
      line <- paste0(line, ' \\[', sprintf('%0.1f', par_def$lb), ', ',
                     sprintf('%0.1f', par_def$ub), ']')
      if ('desc'%in%names(par_def)) {
        line <- paste0(line, ': ', par_def$desc)
      }
      # line <- paste0(line, '}')
      lines <- c(lines, line)
    }
    lines <- c(lines, '}')
    paste(lines, collapse = '\\cr')
  }

#' Get title for function documentation
#'
#' @param ms A \code{model_spec} object.
#'
#' @returns Title string formatted for roxygen2 md
#'
#' @examples
#' get_doc_title_md(model_spec)
get_doc_title_md <-
  function(ms) {
    if('doc_helper'%in%names(ms)) {
      dh <- ms$doc_helper
      if('title'%in%names(dh)) {
        title <- dh$title
        if(!is.null(title) && !is.na(title) && title != '') {
          return(title)
        }
      }
    }
    model_type <-
      if(grepl('^ql_', ms$class)) {
        'RL model'
      } else if (grepl('^qlddm', ms$class)) {
        'RLDDM model'
      } else {
        'Model'
      }
    paste0(model_type, ' with parameters ', parameter_names_parsed_string(ms))
  }

#' Get description for function documentation
#'
#' @param ms A \code{model_spec} object.
#'
#' @returns Description string formatted for roxygen2 md
#'
#' @examples
#' get_doc_description_md(model_spec)
get_doc_description_md <-
  function(ms) {
    ms$description
  }


#' Create default description for \code{formula} argument of a
#' \code{blms_model}
#'
#' @param model_class \code{character}. Class name.
#' @param response_desc \code{character}. Description of how the response
#'        variable needs to be coded.
#' @param pred_desc \code{character}. Description of how the prediction
#'        variable(s) need to be coded.
#' @param block_restart \code{character}. Description of what happens when a
#'        block is restarted
#'
#' @returns A (formatted) \code{character} string containing the description
#'          of the \code{formula} argument for e \code{blms_model}.
#' @examples create_doc_formula_arg()
create_doc_formula_arg <-
  function(
    model_class = '',
    response_desc = '',
    pred_desc = '',
    block_restart = ''
  ) {
    arg_desc <-
      paste0('Response formula specifying the response and its prediction. ',
             ifelse(model_class=='', 'Usually, ',
                    paste0('For models of class \'', model_class, '\',')),
             'this formula needs to define the response on the ',
             'left-hand side',
             response_desc,
             'and the feedback (aka outcome) on the right-hand side',
             pred_desc,
             'If \\code{model_spec$func_par} is not \\code{mu} then the ',
             'prediction variables should be defined on the formula for ',
             '\\code{model_spec$func_par} instead.',
             'In addition, you can define the block structure on the ',
             'left-hand side of the formula. That is, a formula like ',
             '\\code{choice|block(id) ~ reward} would update the (expected) ',
             'values based on each trial\'s choice and reward starting from ',
             'the first row in data until \\code{id[n] != id[n-1]}.',
             block_restart)
  }
#' Get \code{formula} argument description for function documentation
#'
#' @param ms A \code{model_spec} object.
#'
#' @returns Description string for \code{formula} argument
#'          formatted for roxygen2 md
#'
#' @examples
#' get_doc_formula_arg_md(model_spec)
get_doc_formula_arg_md <-
  function(ms) {
    arg_desc <-
      paste0('Response formula specifying the response and its prediction. ',
             'Usually ',
             'this formula needs to define the response on the ',
             'left-hand side',
             ', a variable coded as \\code{1} or \\code{2}, ',
             'and the feedback (aka outcome) on the right-hand side',
             ', coded as \\code{1} or \\code{-1}. ',
             'If \\code{model_spec$func_par} is not \\code{mu} then the ',
             'prediction variables should be defined on the formula for ',
             '\\code{model_spec$func_par} instead.',
             'In addition, you can define the block structure on the ',
             'left-hand side of the formula. That is, a formula like ',
             '\\code{choice|block(id) ~ reward} would update the (expected) ',
             'values based on each trial\'s choice and reward starting from ',
             'the first row in data until \\code{id[n] != id[n-1]}. ',
             'In that case, updating of Q values starts again from ',
             'starting values \\code{c(0.0, 0.0)}.'
             )
    if ('doc_helper'%in%names(ms)) {
      dh <- ms$doc_helper
      if ('formula_arg'%in%names(dh)) {
        form_arg <- dh$formula_arg
        if (!is.null(form_arg) && !is.na(form_arg) && form_arg != '') {
          arg_desc <- form_arg
        }
      }
    }
    arg_desc
  }



#' Details string formatted for roxygen2 md
#'
#' @param ms A \code{model_spec} object.
#'
#' @returns Details string formatted for roxygen2 md
#'
#' @examples
#' get_doc_details_md(model_spec)
get_doc_details_md <-
  function(ms) {
    lines <- c('## Parameters:')
    lines <- c(lines, parameters_md_list(ms))
    # lines <- c(lines, '\\cr\n')

    if ('doc_helper'%in%names(ms)) {
      dh <- ms$doc_helper
      if('notes'%in%names(dh)) {
        # lines <- c(lines, '\\cr\n')
        notes_header <-
          paste0('## Note',
                 ifelse(length(dh$notes)>1, 's', ''), ':')
        lines <- c(lines, notes_header)
        for (note in dh$notes) {
          lines <- c(lines, note)
        }
        # lines <- c(lines, '\\cr\n')
      }
      if('update_formulas'%in%names(dh)) {
        # lines <- c(lines, '\\cr\n')
        form_header <-
          paste0('## Update formula',
                 ifelse(length(dh$update_formulas)>1, 's', ''), ':')
        lines <- c(lines, form_header)
        for (form in dh$update_formulas) {
          lines <- c(lines, form)
        }
        # lines <- c(lines, '\\cr\n')
      }
      if('link_formula'%in%names(dh)) {
        # lines <- c(lines, '\\cr\n')
        form_header <-
          paste0('## Link formula',
                 ifelse(length(dh$link_formula)>1, 's', ''), ':')
        lines <- c(lines, form_header)
        for (form in dh$link_formula) {
          lines <- c(lines, form)
        }
        # lines <- c(lines, '\\cr\n')
      }
      if('references'%in%names(dh)) {
        # lines <- c(lines, '\\cr\n')
        ref_header <-
          paste0('# References')
        lines <- c(lines, ref_header)
        for (ref in dh$references) {
          lines <- c(lines, ref)
        }
        # lines <- c(lines, '\\cr\n')
      }
    }
    paste(lines, collapse = '\\cr\n')
  }
