% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_functions.R
\name{Add_Spatial_to_OM}
\alias{Add_Spatial_to_OM}
\title{Add Spatial Distribution to an Operating Model}
\usage{
Add_Spatial_to_OM(MOM, Rel_Abun_Region, average_prob = 0.05)
}
\arguments{
\item{MOM}{An object of class \code{MOM}}

\item{Rel_Abun_Region}{A data.frame with the biomass distribution by area in the terminal year}

\item{average_prob}{The mean probability of staying within an area in a given year}
}
\value{
An object of class \code{MOM} with the movement matrix and fishing effort
distribution provided in custom parameters
}
\description{
Add Spatial Distribution to an Operating Model
}
