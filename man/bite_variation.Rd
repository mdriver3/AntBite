\name{bite_variation}
\alias{bite_variation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bite variation tester
}
\description{
This function caculates the variation in max bites within individuals and alerts the user to highly variable or invariable records. It also can plot the variation in force across individuals. 
}
\usage{
bite_variation(bite.summary, var_threshold = 2, to_plot = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
The bite summary dataframe created by file_cleaner.
}
  \item{var_threshold}{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (bite.summary, var_threshold = 2, to_plot = T) 
{
    for (i in 1:length(bite.summary$individual)) {
        if (bite.summary$sd_max[i] == 0) {
            cat(paste(bite.summary$individual[i], " has no variation in max bites", 
                sep = ""), fill = T)
        }
        if (bite.summary$sd_max[i] >= var_threshold) {
            cat(paste(bite.summary$individual[i], " excedes the variation threshold in max bites: ", 
                bite.summary$sd_max[i], sep = ""), fill = T)
        }
    }
    if (to_plot == T) {
        individual <- vector()
        force <- vector()
        for (i in 1:length(bite.summary$individual)) {
            temp <- as.numeric(bite.summary[i, 6:10])
            temp <- temp[!is.na(temp)]
            force <- append(force, temp)
            individual <- append(individual, rep(bite.summary[i, 
                1], length(temp)))
        }
        df <- data.frame(individual, force)
        freq <- count(df, "individual")
        boxplot(force ~ individual, data = df, xlab = "Individual", 
            ylab = "Max force across bites")
        mtext("# Bites", side = 3, line = 1, at = 0.5, cex = 1, 
            padj = 1.6)
        mtext(freq$freq, side = 3, line = 1, at = 1:length(freq$freq), 
            padj = 1.6)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
