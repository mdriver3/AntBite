\name{bite_size_scaling}
\alias{bite_size_scaling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bite Force Scaling
}
\description{
This function caculate the scaling relationships between the btie force and size of individuals within a species using linear regression. It will also plot these relationships.
}
\usage{
bite_size_scaling(bite_summary, species = all, min_n = 10, morph_traits, plot = TRUE, combine_plots = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bite_summary}{
The bite summary dataframe created in the file_cleaner function.
}
  \item{species}{
Species to be included in the plot, arguments are "all", a value or vector of species names, or "all_with_n" all species that have a minimum number of individuals represented in bite summary (value determined by min_n)
}
  \item{min_n}{
The minumum number of individuals to allow being included in the plot via species.
}
  \item{morph_traits}{
A value or vector of the mophological traits to be plotted, possiable values/vector are "width", "lendth", "mandi".
}
  \item{plot}{
Logical should the scaling relationships be plotted.
}
  \item{combine_plots}{
Logical, should multiple morphological traits be combined into a single figure (seperate plots)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael Rivera 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

example_file_path <- system.file("extdata", package = "AntBite")
list.files(example_file_path)
setwd(example_file_path)
output <- file_cleaner(folder = example_file_path, size_data = TRUE, size_file = "test_size_data.xlsx")

bite.summary <- output[[1]]

bite_size_scaling(bite.summary, species = "all", min_n = 1, morph_traits = c("width", "length", "mandi"), plot = TRUE, combine_plots = TRUE)
}
