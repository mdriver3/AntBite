#Final project working draft - R Programing
#Michael Rivera - November 11, 2017


#Functions
#File cleaner gf----
#This function opens all csv files in a folder, removes header, and creates 2 dataframes
file_cleaner <- function(folder, size_data = TRUE, size_file){
#   This function opens all the files in the folder, and creates 2 dataframes
#   one which contains all the raw data and one with a summary stats per individual.
#
#   Args:
#       folder: this is the folder location which contains the csv files
#       size_data: this adds size information to the bite summary dataframe
#       size_folder: the folder which has the excel or csv file of size data, including individual, species, and morph measurments
#
#   Returns:
#       a named list 1) bite.summary summary stats about each individuals bites and 2) bite.data a condensed verson of the raw data
#
#
# Setup
  # Creates path to file, reads them into the environment,
  # and inatlized a blank dataframe for bite.data
  files <- list.files(path = folder, pattern = "*.csv")
  dat <- lapply(files, read.delim, sep = ",", skip = 2, header = F, col.names = c("time", "force", "blank"))
  bite.data <- data.frame(time = rep(0, 3000))

  # Checks each data file to ensure they are proper length
  # and have the proper recording frequency; issues warning
  # and skips file if not compliant 
  for(i in 1:length(dat)){ 
    frame <- dat[[i]]
    name <- gsub("*.csv", "", files[i]) 
    
    if(length(frame$force) != 3000){
      warning("This file is misisng force measurments")
      print(name)
      next
    }
    if(frame$time[2] !=  0.006667){
      warning("This file has incorrect time spacing")
      print(name)
      next
    }
  # Fills in the raw data into the bite.data dataframe
    if(i == 1){ 
      bite.data$time <- frame[,"time"] 
    } 
    bite.data[,name] <- frame[, "force"]
  }
  
  # Creates a blank bite.summary dataframe via number of individuals
  # in the bite.data dataframe
  bite.summary <- stats::setNames((data.frame(matrix(nrow = (ncol(bite.data) - 1), ncol = 10))),
                                  c("individual", "num_bites", "max_bite", "max_bite_num", "sd_max", "bite1", "bite2", "bite3", "bite4", "bite5"))
  indiv_count <- 1
  
  # Gets substings of individual names and bite number
  for(i in 2:ncol(bite.data)){
    splt <- as.vector(strsplit(colnames(bite.data[i]), "_")) 
    indiv <- paste(splt[[1]][1], splt[[1]][2], splt[[1]][3], sep = "_")
    bite <- splt[[1]][4]
  
  # Adds individual names to bite.summary; indiv_count tallies
  # number of unique individuals to index on bite.summary
    if(any(indiv %in% bite.summary$individual) == F){ 
      bite.summary[indiv_count, "individual"] <- indiv
      indiv_count <- indiv_count + 1
    }
  # Adds bite data to bite summary, indexed by indiv name and bite number;
  # this is the max force per bite, not overall max per individual
    bite.summary[bite.summary$individual == indiv & is.na(bite.summary$individual) == F, 
                 colnames(bite.summary) == bite] <- max(bite.data[,i]) 
  }
  
  # Removes blank rows from bite summary, adds number of bites per individual,
  # fills in max bite, and fills in which bite had highest force per indiv
  bite.summary <- bite.summary[rowSums(is.na(bite.summary)) != ncol(bite.summary),] 
  bite.summary$num_bites <- rowSums(!apply(bite.summary[6:10],2, is.na))
  bite.summary$max_bite <- apply(bite.summary[6:10], 1, max, na.rm = TRUE)
  for(i in 1:length(bite.summary$individual)){bite.summary$max_bite_num[i] <- which(bite.summary[i, 6:10] == max(bite.summary[i, 6:10], na.rm = T))[1]}
  bite.summary$sd_max <- apply(bite.summary[,6:10], 1, sd, na.rm = TRUE)
  
  # Logical control if size data should be added
  # into the bite.summary object
  if(size_data == FALSE){
    stop()
  }
  if(size_data == TRUE){
  } 
  
  # Load in the size data as excel or csv file
  if(tools::file_ext(size_file) == "xlsx"){
    sizes <- gdata::read.xls(size_file)
  } 
  if(tools::file_ext(size_file) == "csv"){
    sizes <- read.table(size_file)
  } 

  # Check that size.data column names are correct 
  if(all(colnames(sizes) != c("individual", "species", "head_width", "head_length", "mandi_length"))){
    warning("Warning: size_data column names may be off, check or proceed with caution")
  }

  # Adds size data to bite.summary by indexing individual names
  for(i in 1:length(sizes)){
    a <- which(sizes[i, 1] == bite.summary$individual)
    bite.summary[a, "head_width"] <- sizes[i, "head_width"]
    bite.summary[a, "head_length"] <- sizes[i, "head_length"]
    bite.summary[a, "mandi_length"] <- sizes[i, "mandi_length"]
    bite.summary[a, "species"] <- sizes[i, "species"]
  }
  
  # Returns a named list
  return(list(bite.summary = bite.summary, bite.data = bite.data))
}

#Variation_tester----
#This function caculates the variation in max bite and creates a figure and alerts the user to highly variable records
#It uses the bite summary data frame from file_cleaner
#Variation_tester test area
bite_variation <- function(bite.summary, var.threshold = 2, to_plot = TRUE){
#   This function alerts the user to high/low variation in max bite force of an individual
#   by using the bite.summary dataframe produced by file_cleaner
#
#   Args:
#       bite.summary: the bite  summary dataframe produced by file_cleaner, missing data allowed
#       var_threshold: at what standard deviation should the user be notified, defalult is 2
#       to_plot: logical, should a boxplot of bite forces across individuals be produced
#
#   Returns:
#       if any sd in bites are 0 or exceedes the var_threshold a warning is produced, additonally
#       it plots distributions of bite forces per individual 
#
#
  # Prints warning if there is no varation among an indiv's
  # bites or if the varation excedes var_threshold
  for(i in 1:length(bite.summary$individual)){
    if(bite.summary$sd_max[i] == 0){cat(paste(bite.summary$individual[i], " has no variation in max bites", sep = ""), 
                                        fill = TRUE)}
    if(bite.summary$sd_max[i] >= var.threshold){
      cat(paste(bite.summary$individual[i], " excedes the variation threshold in max bites: ", bite.summary$sd_max[i], sep = ""), 
          fill = TRUE)}
  }
  
  # Produces a boxplot of indviduals' max bite forces
  if(to_plot == TRUE){
  individual <- vector()
  force <- vector()
  
  for(i in 1:length(bite.summary$individual)){
    temp <- as.numeric(bite.summary[i, 6:10])
    temp <- temp[!is.na(temp)]
    force <- append(force, temp)
    individual <- append(individual, rep(bite.summary[i,1], length(temp)))
  }
  
  df <- data.frame(individual, force)
  freq <- plyr::count(df, "individual")
  
  # Plots boxplot and adds number of bites per individual above the plot
  boxplot(force ~ individual, data = df, xlab = "Individual", ylab = "Max force across bites")
  mtext("# Bites", side = 3, line = 1, at = .5, cex = 1, padj = 1.6)
  mtext(freq$freq, side = 3, line = 1, at = 1:length(freq$freq), padj = 1.6)
  } 
}

#Bite_size_scaling ----


bite_size_scaling <- function(bite_summary, species = "all", min_n = 10, morph_traits, plot = TRUE, combine_plots = TRUE){
#   #This function uses the bite summary data to perform regression on specified species and morph traits and has
#   the option to plot it or not. It uses 4 internal functions (one which ws obtained from Cookbook R)
#   
#
#   Args:
#       bite_summary: a bite summary dataframe created by the file_cleaner function
#       species: species to be included in plot and regression; all = all species in both data sets, species = value or vector of species, all_with_n = all species with minimum number of records
#       min_n: minimum number of records for species to be included in plot and regression, only used if species == all_with_n
#       morph_trait: the morphological trait(s) to be used to create scaling relationships, can be value or vector; width, length, mandi
#       combine_plots: combine plots into single figure if more than one trait is selected
#
#   Returns:
#       
#
#   Internal functions:
  # Creates a subset of the bite summary dataframe
  # based on the specifed species or min number of individuals
  subset_bite_summary <- function(bite.summary, species, min_n){
    if(species == "all"){
      return(bite.summary)
    } 
    if(species == "all_with_n"){
      tab <- plyr::count(bite.summary$species)
      return(bite.summary[which(bite.summary$species == tab[which(tab$freq >= min_n),1]),])
    }
  if(species != "all" | species != "all_with_n" & is.vector(species)){
    return(bite.summary[which(bite.summary$species == species),])
  }
  }
  # Creates scatter plot with correlation line
  bite_plot <- function(data, trait){
    ggplot2::ggplot(data = data, ggplot2::aes(x = data[,which(colnames(data) == trait)], y = bite_summary$max_bite, color = species))+
      ggplot2::geom_point(shape = 1)+
      ggplot2::geom_smooth(method = lm, se = F)+
      ggplot2::labs(x = paste(sub("_", " ", tools::toTitleCase(trait)), "(mm)", sep = " "), y = "Max bite force (g)")+
      ggplot2::theme_bw()}
  # Creates a linear model of size ~ trait 
  bite_lm <- function(data, trait){
    lm1 <- stats::lm(bite_summary$max_bite ~ data[,which(colnames(data) == trait)], data = data)
  }
  # Allows for the plotting of multiple plots in one
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
 

  # Creates a boxplot of force ~ head width
  # if width is in the morph_traits vector
  if("width" %in% morph_traits){
  temp.trait <- "head_width"
  a <- bite_lm(bite_summary, temp.trait)
  b <- summary(a)
    plot.w <- bite_plot(subset_bite_summary(bite_summary,species, min_n), temp.trait)
  }
  
  # Creates a boxplot of force ~ head length
  # if length is in the morph_traits vector
  if("length" %in% morph_traits){
    temp.trait <- "head_length"
    a <- bite_lm(bite_summary, temp.trait)
    b <- summary(a)
    plot.l <- bite_plot(subset_bite_summary(bite_summary,species, min_n), temp.trait)
  }
  
  # Creates a boxplot of force ~ mandible length
  # if mandi is in the morph_traits vector
  if("mandi" %in% morph_traits){
    temp.trait <- "mandi_length"
    a <- bite_lm(bite_summary, temp.trait)
    b <- summary(a)
    plot.m <- bite_plot(subset_bite_summary(bite_summary,species, min_n), temp.trait)
  } 
  
  # If logical combine_plots is TRUE then all
  # figures are plotted in the same plot
  if(combine_plots == TRUE){
    multiplot(plot.w, plot.l, plot.m)
    return()
  }
  if(plot == TRUE){
    if(exists("plot.w")){
    plot(plot.w)}
    if(exists("plot.l")){
      plot(plot.l)}
    if(exists("plot.m")){
      plot(plot.m)}
    return()
  }
  if(plot == F){
    return()
  }
  
  }

