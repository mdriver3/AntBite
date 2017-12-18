\name{bite_variation}
\alias{bite_variation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bite variation tester
}
\description{
This function alerts the user to individuals with highly (or no variabality) 
variable max bite forces which may indicate an error in the measurment process.
This uses the bite.summary dataframe generated from the file_cleaner function. If choosen 
this also will produce boxplots of individuals' max bite forces and indicate the number
of bites an individual produced. 
}
\usage{
bite_variation(bite.summary, var.threshold = 2, to_plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{bite.summary}{
The bite summary dataframe produced by file_cleaner.
}
  \item{var.threshold}{
The user defined number of standard deviations, above which the user should be notified.
}
  \item{to_plot}{
Logical, should a plot be produced?
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a warning if any individuals fall above the threshold or are 0. Will also return a plot if plot == T.
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
#Use the file_cleaner function to create a bite.summary dataframe

example_file_path <- system.file("extdata", package = "AntBite")
list.files(example_file_path)
setwd(example_file_path)
output <- file_cleaner(folder = example_file_path, size_data = TRUE, size_file = "test_size_data.xlsx")

bite.summary <- output$bite.summary
bite_variation(bite.summary, var.threshold = 2, to_plot = TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
