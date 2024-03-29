% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_bootstrap.R
\name{qtt_pte_aggregations}
\alias{qtt_pte_aggregations}
\title{qtt_pte_aggregations}
\usage{
qtt_pte_aggregations(attgt.list, ptep, extra_gt_returns)
}
\arguments{
\item{attgt.list}{list of attgt results from \code{compute.pte}}

\item{ptep}{\code{pte_params} object}
}
\value{
\code{pte_emp_boot} object
}
\description{
Aggregate group-time distributions into qtt versions of
overall, group, and dynamic effects.
}
