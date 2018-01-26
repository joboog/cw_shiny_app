summary_table2 <- function(df,col_n, ByFactor, ByFactorCol){
  # df: inout data frame containing numeric data and a factor to summarize by
  # col_n: a numeric vector specifying the column numbers fo the numeric variables to subset the data frame
  # ByfactorName: Name of the Factor by which to summarize
  # ByFactorCol: Column number of the summarizing Factor
  
  # Own summary table using 
  #
  library(dplyr)
  # initialise function to count non NA values =========================================
  countValues <- function(x){
    as.integer(length(!is.na(x)[!is.na(x)==TRUE]))
  }
  
  # df <- lRaw_new2
  # ByFactor <- "SamplePoint"
  # ByFactorCol <- 1
  # col_n <- 4:length(colnames(lRaw_new2))
  
  #  compute summary stats =============================================================
  df <- df[,c(ByFactorCol,col_n)]
  # compute means values
  means <- df %>% group_by_(ByFactor) %>% summarise_all(funs(mean), na.rm=TRUE)
  means <- cbind(means[,1], round(select(means, -1), digits = 1))
  means <- format(means, digits = 1, nsmall = 1)
  colnames(means) <- c(ByFactor, colnames(select(means, -1)))
  
  # compute standard deviations
  sds <- df %>% group_by_(ByFactor) %>% summarise_all(funs(sd), na.rm=TRUE)
  sds <- cbind(sds[,1], round(select(sds, -1), digits = 1))
  sds <- format(sds, digits = 1, nsmall = 1)
  colnames(sds) <- c(ByFactor, colnames(select(sds, -1)))
  # set value of first columnof sds to NA, just to make the table look better in the end
  sds[,1] <- NA
  
  # count non NA values
  counts <- df %>% group_by_(ByFactor) %>% summarise_all(funs(countValues))
  counts <- format(as.data.frame(counts), digits = 0, nsmall = 0)
  colnames(counts) <- c(ByFactor, colnames(select(counts, -1)))
  counts[,1] <- NA
  
  
  
  # creates a summary table with means values ==========================================
  # standards deviations and counts
  
  # create the data frame
  df1 <- rbind(means[1,], sds[1,], counts[1,])
  
  # if more than one SamplePoint, combine mean values, standard deviations, counts using a loop
  if (length(means[,1])>1) {
    for (i in 2:length(means[,1])){
      df1 <- rbind(df1, means[i,], sds[i,], counts[i,])
    }
  }
  
  # create a parameter vector which will be implemented in the summary table
  Parameter <- rep(c("Mean", "StDev", "Count"), times=length(means[,1]))
  df1 <- cbind(df1,Parameter)
  # reorder df1, put parameters into 2nd column
  df1 <- df1[,c(1,length(df1),3:length(df1)-1)]
  # rename ByFactorColumn
  
}


# ===============================================================================
summary_table2_old <- function(df,col_n, ByFactor, ByFactorCol){
  # df: inout data frame containing numeric data and a factor to summarize by
  # col_n: a numeric vector specifying the column numbers fo the numeric variables to subset the data frame
  # ByfactorName: Name of the Factor by which to summarize
  # ByFactorCol: Column number of the summarizing Factor
  
  # Own summary table using 
  #
  library(dplyr)
  # initialise function to count non NA values =========================================
  countValues <- function(x){
    as.integer(length(!is.na(x)[!is.na(x)==TRUE]))
  }
  
  # df <- lRaw_new2
  # ByFactor <- "SamplePoint"
  # ByFactorCol <- 1
  # col_n <- 3:length(colnames(lRaw_new2))
  
  #  compute summary stats =============================================================
  # compute means values
  means <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=mean, na.rm=TRUE, simplify = TRUE)
  means <- cbind(means[,1], round(select(means, -ByFactor), digits = 1))  #means[,2:length(means[1,])],digits=1))
  means <- format(means, digits = 1, nsmall = 1)
  colnames(means) <- c(ByFactor, colnames(select(means, -1))) #(means)[2:length(means)])
  # compute standard deviations
  sds <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=sd, na.rm=TRUE, simplify = TRUE)
  sds <- cbind(sds[,1], round(select(sds, -ByFactor),digits=1))
  sds <- format(sds, digits = 1, nsmall = 1)
  colnames(sds) <- c(ByFactor, colnames(select(sds, -1)))
  # set value of first columnof sds to NA, just to make the table look better in the end
  sds[,1] <- NA
  # count no NA values
  counts <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=countValues,  simplify = TRUE)
  counts <- format(counts, digits = 0, nsmall = 0)
  colnames(counts) <- c(ByFactor, colnames(select(counts, -1)))
  counts[,1] <- NA
  
  
  
  # creates a summary table with means values ==========================================
  # standards deviations and counts
  
  # create the data frame
  df1 <- rbind(means[1,], sds[1,], counts[1,])
  
  # if more than one SamplePoint, combine mean values, standard deviations, counts using a loop
  if (length(means[,1])>1) {
    for (i in 2:length(means[,1])){
      df1 <- rbind(df1, means[i,], sds[i,], counts[i,])
    }
  }
  
  # create a parameter vector which will be implemented in the summary table
  Parameter <- rep(c("Mean", "StDev", "Count"), times=length(means[,1]))
  df1 <- cbind(df1,Parameter)
  # reorder df1, put parameters into 2nd column
  df1 <- df1[,c(1,length(df1),3:length(df1)-1)]
  # rename ByFactorColumn
  
}


# =================================================================================


summary_table3 <- function(df,col_n, ByFactor, ByFactorCol){
  # input: data frame containing numeric data and a factor to summarize by
  # col_n: a vector specifying the column numbers to subset the data frame
  # ByfactorName: Name of the Factor by which to summarize
  # ByFactorCol: Column number of the summarizing Factor
  
  # Own summary table using 
  #
  # initialise function to count values =========================================
  countValues <- function(x){
    as.integer(length(!is.na(x)[!is.na(x)==TRUE]))
  }
  
  # =============================================================================
  # compute means values
  means <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=mean, na.rm=TRUE, simplify = TRUE)
  means <- cbind(means[,1], round(means[,2:length(means),1]))
  means <- format(means, digits = 1, nsmall = 1)
  colnames(means) <- c(ByFactor, colnames(means)[2:length(means)])
  # compute standard deviations
  sds <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=sd, na.rm=TRUE, simplify = TRUE)
  sds <- cbind(sds[,1], round(sds[,2:length(sds)],1))
  sds <- format(sds, digits = 1, nsmall = 1)
  colnames(sds) <- c(ByFactor, colnames(sds)[2:length(sds)])
  # set value of first columnof sds to NA, just to make the table look better in the end
  sds[,1] <- NA
  # count no NA values
  counts <- aggregate(df[,col_n], by=list(ByFactor=df[,ByFactorCol]), FUN=countValues,  simplify = TRUE)
  count <- format(counts, digits = 0, nsmall = 0)
  colnames(counts) <- c(ByFactor, colnames(counts)[2:length(counts)])
  counts[,1] <- NA
  # ===============================================================================
  
  
  # creates a summary table with means values, 
  # standards deviations and counts
  # ==========================================
  # create the data frame
  df1 <- rbind(means[1,], sds[1,], counts[1,])
  # combine mean values, standard deviations, counts using a loop
  for (i in 2:length(means[,1])){
    df1 <- rbind(df1, means[i,], sds[i,], counts[i,])
  }
  # create a parameter vector which will be implemented in the summary table
  Parameter <- rep(c("Mean", "StDev", "Count"), times=length(means[,1]))
  df1 <- cbind(df1,Parameter)
  # reorder df1, put parameters into 2nd column
  df1 <- df1[,c(1,length(df1),3:length(df1)-1)]
  # rename ByFactorColumn
  
}



# ==== Pore water plots ===============================================================

myGGPoreWQPlot <- function(df, facet_var, plottitle){
  plot <- ggplot(data = na.omit(df), aes(x=dist_axial, y=mean, color=System)) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +geom_point() 
  if (length(unique(df$Parameter))>1)
  {
    plot <- plot + facet_grid(facet_var, scales = "free_y") + labs(title=plottitle) 
  }
  return(plot)
  #geom_smooth(aes(fg=System, group=Parameter), method="loess", span=1 , se=FALSE)
}

mySummaryDf <- function(df){
  df <- ddply(na.omit(df), .(SamplePoint, System, Parameter, dist_axial),summarize,
              mean= mean(value),
              sd= sd(value))
  return(df)
}


