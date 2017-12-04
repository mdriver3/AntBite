\name{file_cleaner}
\alias{file_cleaner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import and Format Flexiforce(r) Force-Timeseries Data}
\description{
This function takes in all Flexiforce CSV files in a folder and creates two dataframes. 1) Bite_data: this is a condensced version of the raw data. 2) Bite_summary: this dataframe contains summary stats (max bite, max bite number, number of bites) along with size and taxonomy data from supplied file. 
}
\usage{
file_cleaner(folder, size_data = T, size_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
The path to the folder containing the csv files.
}
  \item{size_data}{
Logical, if T the function adds size and taxonomy data from supplied size_file to the bite.summary dataframe. 
}
  \item{size_file}{
An excel or csv file containing matching individual names, species identification, and morpholoigcal measurments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of two dataframes 1) bite_data which is a condencsed version of the raw data and 2) bite_summary which contains summary statistics and (if argument is choosen) size and taxonomical information. 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line