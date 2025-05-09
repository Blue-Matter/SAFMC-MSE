% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_OMs.R
\name{get_stock_status}
\alias{get_stock_status}
\alias{get_F}
\alias{get_landings_discards}
\alias{get_selectivity_retention}
\alias{get_rec_devs}
\title{Extracts stock status (SSB/MSST and F/MFMT) from BAM output}
\usage{
get_stock_status(BAM_dir = "BAM_Objects")

get_F(om = "BaseCase", Hist_dir = "Hist_Objects")

get_landings_discards(hist_dir = "Hist_Objects")

get_selectivity_retention(dir = "Hist_Objects")

get_rec_devs(dir = "OM_Objects")
}
\arguments{
\item{BAM_dir}{Directory with BAM files}
}
\value{
A data.frame
}
\description{
Also returns the MSST and MFMT reference points
}
\section{Functions}{
\itemize{
\item \code{get_F()}: Return fishing mortality by fleet

\item \code{get_landings_discards()}: Return landings and discards by fleet

\item \code{get_selectivity_retention()}: Return selectivity and retention by fleet

\item \code{get_rec_devs()}: Return recruitment deviations by stock

}}
