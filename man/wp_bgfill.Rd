% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wpplot.R
\name{wp_bgfill}
\alias{wp_bgfill}
\title{Fill the background of gene with color according to amount of gene expression.}
\usage{
wp_bgfill(
  p,
  value,
  high = "red",
  low = "blue",
  legend = TRUE,
  legend_x = 0.001,
  legend_y = 0.94
)
}
\arguments{
\item{p}{p is}

\item{value}{value is the amount of expression.}

\item{high}{The color of highest gene.}

\item{low}{The color of lowest gene.}

\item{legend}{Whether you need legend.}

\item{legend_x}{horizontal position of the legend}

\item{legend_y}{vertical position of the legend}
}
\value{
A 'wpplot' object
}
\description{
Generate a color array.Fill the gene then generate the legend.
}
