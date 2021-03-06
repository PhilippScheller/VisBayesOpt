% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MboPlotRuntime.R
\name{MboPlotRuntime}
\alias{MboPlotRuntime}
\title{MboPlotRuntime}
\description{
This class plots the runtime of a mlrMbo run.
}
\section{Super class}{
\code{\link[VisBayesOpt:MboPlot]{VisBayesOpt::MboPlot}} -> \code{MboPlotRuntime}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MboPlotRuntime$new()}}
\item \href{#method-plot}{\code{MboPlotRuntime$plot()}}
\item \href{#method-clone}{\code{MboPlotRuntime$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="VisBayesOpt" data-topic="MboPlot" data-id="set_param_vals">}\href{../../VisBayesOpt/html/MboPlot.html#method-set_param_vals}{\code{VisBayesOpt::MboPlot$set_param_vals()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MboPlotRuntime$new(opt_state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{opt_state}}{(\link{OptState}).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot"></a>}}
\subsection{Method \code{plot()}}{
Plots minimal value of model (y) of mbo run over the iterations.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MboPlotRuntime$plot(highlight_iter = self$param_vals$highlight_iter)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{highlight_iter}}{(\code{integer(1) | NULL})\cr
Specifies the iteration to be highlighted. The default \code{NULL} does not highlight any iteration.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
(\link{ggplot}).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MboPlotRuntime$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
