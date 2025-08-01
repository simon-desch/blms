% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ql_a_2xi.R
\name{ql_a_2xi}
\alias{ql_a_2xi}
\title{RL model with learning rate and dual lapse parameter}
\usage{
ql_a_2xi(
  formula,
  ...,
  data = NULL,
  formula_only = !compile,
  compile = run,
  run = F,
  model_class = "ql_a_2xi",
  model_spec = get_model_spec(model_class),
  par_form = model_spec[["par_form"]],
  par_transform = model_spec[["par_transform"]]
)
}
\arguments{
\item{formula}{Response formula specifying the response and its prediction. Usually this formula needs to define the response on the left-hand side, a variable coded as \code{1} or \code{2}, and the feedback (aka outcome) on the right-hand side, coded as \code{1} or \code{-1}. If \code{model_spec$func_par} is not \code{mu} then the prediction variables should be defined on the formula for \code{model_spec$func_par} instead.In addition, you can define the block structure on the left-hand side of the formula. That is, a formula like \code{choice|block(id) ~ reward} would update the (expected) values based on each trial's choice and reward starting from the first row in data until \code{id[n] != id[n-1]}. In that case, updating of Q values starts again from starting values \code{c(0.0, 0.0)}.}

\item{...}{You can pass further formulas to define predictions of parameters
used in the response formula. Any non-formula objects passed via
\code{...} are passed on to other functions (i.e., \code{brmsformula} and
\code{brm})}

\item{data}{the data containing the response variable and any variables used
for prediction. Any variables that occur in formulas but cannot be
found in \code{data} will be considered non-linear parameters.}

\item{formula_only}{Return the \code{brmsformula} created from inputs but do not
(try to) compile or run the model}

\item{compile}{compile the model and return a \code{brmsfit} object. If \code{run} is
\code{FALSE} \code{brm} is called with its \code{chains} argument set to \code{0} which
will compile the model but will not sample}

\item{run}{calls \code{brm} with the \code{brmsformula} created from inputs, \code{data},
\code{stanvars} defined by \code{model_spec} or \code{stanvars} defined in \code{dots},
and any other arguments passed vie \code{...} that were not consumed for
creating the formula}

\item{model_class}{character of length 1 specifying the class of the model
(see \code{names(get_all_model_specs())} for names of implemented model
classes)}

\item{model_spec}{list defining a model specification (see
\code{get_all_model_specs()} for currently implemented models and their
specifications; you can supply other model specifications that
follow the same structure)}

\item{par_form}{\code{list()} object containing \code{formula} object(s) that specify
prediction formulas for other parameters; formulas passed via \code{...}
will override formulas passed via \code{par_form}}

\item{par_transform}{\code{list()} object containing transformation formulas for
other formulas of the model; transformation formulas can e.g. be used
to transform parameters that have upper and/or lower bounds. When
passing transformation formulas via \code{par_transform} you can specify
the parameter to transform on the left-hand side and the
transformation formula - containing the same parameter - on the
right-hand-side of the formula (e.g. \code{d ~ inv_logit(d)}). You can also
pass transformation formulas via the \code{...} by using a
transformation statement (\code{transform()}) on the left-hand side of the
formula and the transformation statement on the right-hand side of
the formula, e.g. \code{transform(d) ~ inv_logit(d)}. \code{blms} creates from
this formula a non-linear formula in which the parameter on the
right-hand side is replaced by a new parameter named after the
original parameter with \code{'raw'} as suffix (e.g.
\code{d ~inv_logit(draw)}). If in \code{par_form} there is a formula
that has the respective parameter (\code{d} in the current example) on its
left-hand side, then in that formula the parameter on the left-hand
side is renamed by adding the suffix \code{'raw'} to the first term.
If you want to use
the same transformation for several parameters you can specify
multiple parameters on the left-hand side combining them by \code{+} and
use 'x' on the right-hand side of the formula. This will create as
many transformation formulas as there are variables on the left-hand
side and replace 'x' on the right-hand side.}
}
\value{
If \code{formula_only} is \code{TRUE} then the return value is an
object of class \code{blmsformula} (inheriting from class
\code{brms::brmsformula}), else the return value is of class
\code{blmsfit} (inheriting from class \code{brms::brmsfit})
}
\description{
Rescorla-Wagner delta learning model with single learning rate and separate lapse parameters based on the outcome of the previous trial
}
\details{
\subsection{Parameters:\cr}{

\itemize{\cr\item \strong{\eqn{\alpha}} [0.0, 1.0]: Learning rate\cr\item \strong{\eqn{\xi_{pos}}} [0.0, 1.0]: Lapse parameter after receiving a positive outcome\cr\item \strong{\eqn{\xi_{neg}}} [0.0, 1.0]: Lapse parameter after receiving a negative outcome\cr}\cr
}

\subsection{Update formula:\cr}{

\deqn{Q_{a,t+1} = Q_{a,t} + \alpha \times (R_{t} - Q_{a,t})}\cr
}

\subsection{Link formula:\cr}{

\deqn{p(a_{i}) = \left(\frac{e^{Q_{a_{i}}}}{\sum_{j=1}^{K} e^{Q_{a_{j}}}}\right) \times (1 - \xi) + \frac{\xi}{2},\quad \text{where } \xi = \begin{cases} \xi_{pos} & \text{if } R_{t-1} \geq 0 \\ \xi_{neg} & \text{if } R_{t-1} \lt 0\end{cases} }\cr
}
}
\section{References\cr}{
Watkins, C. J. C. H., & Dayan, P. (1992). Q-learning. \emph{Machine Learning}, 8(3), 279–292. \url{https://doi.org/10.1007/BF00992698}
}

