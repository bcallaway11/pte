% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pte_params.R
\name{pte_params}
\alias{pte_params}
\title{pte_params}
\usage{
pte_params(
  yname,
  gname,
  tname,
  idname,
  data,
  glist,
  tlist,
  cband,
  alp,
  boot_type,
  anticipation = NULL,
  base_period = NULL,
  weightsname = NULL,
  control_group = "notyettreated",
  gt_type = "att",
  ret_quantile = 0.5,
  biters,
  cl
)
}
\arguments{
\item{yname}{Name of outcome in \code{data}}

\item{gname}{Name of group in \code{data}}

\item{tname}{Name of time period in \code{data}}

\item{idname}{Name of id in \code{data}}

\item{data}{balanced panel data}

\item{glist}{list of groups to create group-time average treatment effects
for}

\item{tlist}{list of time periods to create group-time average treatment
effects for}

\item{alp}{significance level; default is 0.05}

\item{boot_type}{which type of bootstrap to use}

\item{anticipation}{how many periods before the treatment actually takes
place that it can have an effect on outcomes}

\item{control_group}{Which group is used as the comparison group.  For
backwards compatibility, the default choice is "notyettreated", but
different estimation strategies can implement their own choices
for the control group}

\item{gt_type}{which type of group-time effects are computed.
For backward compatiblity, the default choice is "att"}

\item{biters}{number of bootstrap iterations; default is 100}

\item{cl}{number of clusters to be used when bootstrapping; default is 1}
}
\description{
Objects that contain pte parameters
}
