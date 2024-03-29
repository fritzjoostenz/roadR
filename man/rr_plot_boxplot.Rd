% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_utils.R
\name{rr_plot_boxplot}
\alias{rr_plot_boxplot}
\title{Gets optimal subsections on a set of Treatment Lengths}
\usage{
rr_plot_boxplot(
  data,
  x_col,
  y_col,
  y_min,
  y_max,
  y_inc,
  x_label,
  y_label,
  show_jitter = TRUE,
  obs_size = 5,
  obs_offset = 1,
  font_name = "serif",
  x_font_size = 12,
  y_font_size = 12,
  x_lab_halign = 0.5,
  x_lab_rot_angle = 0,
  y_grid_colour = "lightgray",
  y_grid_size = 0.2,
  y_grid_linetype = "dashed"
)
}
\arguments{
\item{data}{data frame containing a the data to plot}

\item{x_col}{name of the column holding the X-axis data. Should be factor or
character}

\item{y_col}{name of the column holding the Y-axis data.}

\item{y_min}{minimum for y-axis}

\item{y_max}{maximum for y-axis}

\item{y_inc}{increment for y-axis}

\item{x_label}{label for X-axis, or pass empty string to omit}

\item{y_label}{label for Y-axis, or pass empty string to omit}

\item{show_jitter}{Should points be shown using jitter?}

\item{obs_size}{Size of labels showing number of observations (values should
be in range of 3-6, these sizes are not font sizes). Enter zero to omit the
number of observations}

\item{obs_offset}{Percentage of Y-axis maximum at which observations should
be shown. To show labels for number of observations right at the top, pass
in 1.0}

\item{font_name}{Name of the font. Default is 'serif'. You can try other
fonts handled by ggplot such as 'sans'}

\item{x_font_size}{Size of font for X-axis label and ticks}

\item{y_font_size}{Size of font for Y-axis label and ticks}

\item{x_lab_halign}{Horizontal alignment for X-axis tick labels. Use 0 for
left-aligned, 0.5 for centred and 1 for right aligned. Generally, use 0.5 but
when rotating labels, you can experiment with left and right aligned.}

\item{x_lab_rot_angle}{Rotation angle for X-axis labels. Default is zero (no
rotation). You can experiment with negative values, such as -35 for longer
labels}

\item{y_grid_colour}{colour for Y-axis grid}

\item{y_grid_size}{Line size for Y-axis grid. Experiment with values in the
range 0.15 to 0.25. Default is 0.2.}

\item{y_grid_linetype}{ggplot line type code. Default is 'dashed'. See web
for valid values which are: 'blank', 'solid', 'dashed', 'dotted', 'dotdash',
'longdash', 'twodash'}
}
\description{
\code{rr_get_subsecs_on_sections} Generates a set of optimal sub-sections
for each section within a larger set. For example, you can use this function
to generate sub-sections for an entire network when the set of sections
provided as the first parameter represent all sections on your network.
}
\details{
Note: This method DOES provide a full set of sub-segments representing the
entire network defined by the set of treatment lengths you provide.

Also note that this method, unlike \code{\link{rr_get_subsecs_on_sections}}
IS lane specific. If your data and/or treatment lengths are not
lane-specific, then use lane code 'all' in your deficit data or treatment
length definitions. If your data and treatment length definitions ARE
lane-specific, then note that the comparison is case-sensitive!
}
