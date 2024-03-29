% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_engineering.R
\name{rr_overlap_length}
\alias{rr_overlap_length}
\title{Gets the overlap between two linear segments}
\usage{
rr_overlap_length(seg1_start, seg1_end, seg2_start, seg2_end)
}
\arguments{
\item{seg1_start}{'start' or 'from' location for the first segment}

\item{seg1_end}{'end' or 'to' location for the first segment}

\item{seg2_start}{'start' or 'from' location for the second segment}

\item{seg2_end}{'end' or 'to' location for the second segment}
}
\value{
length of the overlap
}
\description{
\code{rr_overlap_length} takes the start and end locations for two linear
segments and returns the length of the overlap between them in the same
units as the start and end locations
}
