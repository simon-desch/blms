blmformula <-
  function(formula, ..., flist = NULL, family = NULL,
           autocor = NULL, nl = NULL, loop = NULL,
           center = NULL, cmc = NULL, sparse = NULL,
           decomp = NULL, unused = NULL) {

    out <- brms::brmsformula(
      formula, ..., flist = NULL, family = NULL,
      autocor = NULL, nl = NULL, loop = NULL,
      center = NULL, cmc = NULL, sparse = NULL,
      decomp = NULL, unused = NULL
    )
    # new_base_form <- form_add_attr(out$formula)
    # msg_var(new_base_form)
    out$formula <- form_add_attr(out$formula)
    # message('pforms:')
    # lapply(out$pforms, \(x) {pform <- form_add_attr(x); msg_var(pform)})
    out$pforms <- lapply(out$pforms, form_add_attr)
    class(out) <- c('blmformula', class(out))
    return(out)
  }
