\name{file_cleaner}
\alias{file_cleaner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import and Format Flexiforce Force-Timeseries Data
}
\description{
This function opens all the CSV files in a selected folder, reads them into the R environment, and removes the header. It then creates 2 different data frames. 1) bite_data This is a condensed version of the raw data minus the extraneous header information. The first column will be a single time series with additional columns holding force information for every bite record in the file. Column names will be taken from file names. This will check to make sure that all the files have the same time binning and will issue an error with the incorrect file if not. 2) bite_summary This data frame will have a row for each individual ant (i.e. unique substrings in file names) and then will create summary statistics for those individuals (max force among all of that individual’s files, which bite the max force occurred in, and the range of max forces across that individual’s files). An optional size_file (a csv or excel file containing columns with individual names, species identification, head width/length and mandible length measurments) can be specified to add size data to the bite_summary dataframe. 
}
\usage{
file_cleaner(folder, size_data = TRUE, size_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder}{
The path to the folder containing the Flexiforce csv files.
}
  \item{size_data}{
Logical, if T the function adds size and taxonomy data from supplied size_file to the bite.summary dataframe. 
}
  \item{size_file}{
An excel or csv file containing matching individual names, species identification, and morpholoigcal measurments. 
Column headers should be as follows "individual, speies, head_width, head_length, mandi_length". Individual names
should match identically to the prefixes of the Flexiforce csv files.
}
}
\details{

  bite.summary - This will contain, by individual, the number of bites that indivudal performed, 
  the maximum force produced by that individual across all bites, which bite
  the maximum force was produced in, the standard deviation of max bite forces. 
  This is followed by the max force produced in each bite for that individual. 
  
  bite.data - This condenced data set contains the time series data for ever Flexiforce csv file
  in the folder sharing a single time column. Each file is represeted as a column and is named 
  for the individual and bite number. 

}
\value{
A list of two dataframes:
  \item{bite.summary}{
  Summary statistics and (if argument is choosen) size and taxonomical information
  }
  \item{bite.data}{a condencsed ersion of the raw data
  }
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
head(output$bite.summary)
head(output$bite.data)
}