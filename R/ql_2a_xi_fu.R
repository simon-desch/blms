#' `r get_doc_title_md(get_model_spec('ql_2a_xi_fu'))`
#'
#' @description `r get_doc_description_md(get_model_spec('ql_2a_xi_fu'))`
#'
#' @param formula `r get_doc_formula_arg_md(get_model_spec('ql_2a_xi_fu'))`
#' @inheritParams blms_model
#'
#' @returns If \code{formula_only} is \code{TRUE} then the return value is an
#'          object of class \code{blmsformula} (inheriting from class
#'          \code{brms::brmsformula}), else the return value is of class
#'          \code{blmsfit} (inheriting from class \code{brms::brmsfit})
#'
#' @details
#' `r get_doc_details_md(get_model_spec('ql_2a_xi_fu'))`
#'
#'
#'
#' @export
ql_2a_xi_fu <-
  function(formula,
           ...,
           data = NULL,
           formula_only = !compile,
           compile = run,
           run = F,
           model_class = 'ql_2a_xi_fu',
           model_spec = get_model_spec(model_class),
           par_form = model_spec[['par_form']],
           par_transform = model_spec[['par_transform']]
  ) {
    return(
      blms_model(formula,
                 ...,
                 data = data,
                 formula_only = formula_only,
                 compile = compile,
                 run = run,
                 model_class = model_class,
                 model_spec = model_spec,
                 par_form = par_form,
                 par_transform = par_transform
      )
    )
  }
