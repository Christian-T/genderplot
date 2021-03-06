% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gender_plot.R
\name{gender_plot}
\alias{gender_plot}
\title{Plotting function for more comprehensible gender effects}
\usage{
gender_plot(
  data,
  varname,
  gender = "gender",
  color = NA,
  lb = NA,
  ub = NA,
  varname_title = NA
)
}
\description{
Plotting function for more comprehensible gender effects
}
\examples{
library(AER)
data("PhDPublications")

genderplot::gender_plot(PhDPublications, varname="prestige")
genderplot::gender_plot(PhDPublications, varname="prestige", lb=-1) #need to adjust lower bound to depict larger gender differences

data(STAR) #data on effect of reducing class size on test scores in the early grades
genderplot::gender_plot(STAR, varname="read1") #read1 = reading in first grade
genderplot::gender_plot(STAR, varname="math3") #math3= math in 3rd grade

data("TeachingRatings") #Data on course evaluations, course characteristics, and professor characteristics (beauty)
genderplot::gender_plot(TeachingRatings, varname="age") #more old professors are male
genderplot::gender_plot(TeachingRatings, varname="age", lb=-1, ub=2.5) #need to adjust lower and upper bound
genderplot::gender_plot(TeachingRatings, varname="beauty", lb=-1) #more beautiful professors are female
genderplot::gender_plot(TeachingRatings, varname="eval") # higher evaluated professors are more often male than female
}
\keyword{flat;}
\keyword{violin}
