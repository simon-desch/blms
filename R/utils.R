rm_ws <-
  function(str, replace_ws = T, trim_ws = T, remove_ws = F) {
    if (replace_ws) str <- gsub('\\t|\\n|\\s+', ' ', str)
    if (trim_ws) str <- gsub('^\\s+', '', gsub('\\s+$', '', str))
    if (remove_ws) str <- gsub('\\s+', '', str)
    return(str)
  }

msg_var <-
  function(var, show_attr = F) {
    # message(substitute(var), ': ', toString(deparse(var)))
    if(is.character(var)||is.formula(var)||is.numeric(var)||is.logical(var)) {
      message(substitute(var), ': ', toString(deparse1(var)))
      return()
    }
    message(substitute(var), ': ')
    str(var, give.attr = show_attr)
  }


as_single_char <-
  function(arg) {
    is_single_char <-
      is.character(arg) && length(arg) == 1 && !is.na(arg)
    if (is_single_char) return(arg)
    return(NULL)
  }

order_df <-
  function(df, cols, decreasing = F) {
    if(!inherits(df, 'data.frame')) {
      stop('df must be a data.frame')
    }
    if(!is.character(cols)) {
      stop('cols must be a character vector')
    }
    cols_diff <- setdiff(cols, names(df))
    if(length(cols_diff)>0) {
      stop('the following columns were not found in df: ', toString(cols_diff))
    }
    if(!is.logical(decreasing)) {
      stop('decreasing must be a logical vector')
    }
    decreasing <- rep(decreasing, length.out = length(cols))

    ordering_vars <- Map(function(col, dec) {
      v <- df[[col]]
      if (dec) {
        if (is.factor(v)) {
          factor(v, levels = rev(levels(v)))  # reverse factor levels
        } else if (is.numeric(v)) {
          -v                                  # negate numeric values
        } else {
          -xtfrm(v)                           # reverse character/other order
        }
      } else {
        v
      }
    }, cols, decreasing)

    out_df <- df[do.call(order, ordering_vars), ]
  }
