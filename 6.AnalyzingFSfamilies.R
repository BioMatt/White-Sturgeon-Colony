# Script written by M.Thorstensen

# This script analyzes a directory of BestFSFamily output files from Colony, summarizing low, and high Ns values and true Ns success rate (if applicable, eg when validating accuracy with simulated crosses)
library(data.table)
library(rlist)

# Set your working directory to the one where you've dumped your FSFamily files
setwd("C:/Users/mattt/Dropbox/Colony/FS Families/BLCJ2016 FSfamily files")


# A function to analyze real data with no particular naming scheme, and no true Ns value known. 
real_FSfamilies <- function(inc.prob, exc.prob.low, exc.prob.high){
  # Calculating number of breeder values assuming 2 breeders per family, based on inclusive and exclusive probability thresholds that you can set. Furthermore, there are two such loops for low and high Ns because we chose to use 0.15 and 0.20 as exclusive probability thresholds, and calculated Ns based on both thresholds.
  FSfiles <- list.files(getwd(), recursive = TRUE, pattern=".BestFSFamily")
  
  Est.Low <- list()
  Est.High <- list()
  
  # Setting up lists for the number of individuals in each family, used for calculating average family sizes. 
  exc.low.fam.size <- list()
  exc.high.fam.size <- list()

  for (file in FSfiles)
  {  
    table <- read.table(file, header = TRUE, sep = "")
    
    # This for loop returns lists of family sizes for each exclusive probability, used for calculating average family size later. It splits families into single individuals when a family's inclusive probability is lower than the threshold, and ignores families with exclusive probabilities below chosen thresholds. 
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        for (j in 1:length(sibs))
        {
          exc.high.fam.size <- list.append(exc.high.fam.size, 1)
        }
        rm(sibs)
      } else if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] >= inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        exc.high.fam.size <- list.append(exc.high.fam.size, length(sibs))
        rm(sibs)
      }
      
      if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        for (j in 1:length(sibs))
        {
          exc.low.fam.size <- list.append(exc.low.fam.size, 1)
        }
        rm(sibs)
      } else if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] >= inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        exc.low.fam.size <- list.append(exc.low.fam.size, length(sibs))
        rm(sibs)
      }
    }
    
    # This is the loop that counts up high, and then low Ns estimates from the full-sibling family table. 
    
    Nshigh <- 0
    
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        Nshigh <- Nshigh + length(sibs)
        rm(sibs)
      }  else if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] >= inc.prob){
        Nshigh <- Nshigh + 1
      }
    }
    
    Nshigh <- Nshigh * 2
    
    
    Nslow <- 0
    
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        Nslow <- Nslow + length(sibs)
        rm(sibs)
      }  else if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] >= inc.prob){
        Nslow <- Nslow + 1
      }
    }
    Nslow <- Nslow * 2
    
    
    Est.Low <- list.append(Est.Low, Nslow)
    
    Est.High <- list.append(Est.High, Nshigh)
  }
  
  
  # Calculating error from a normal distribution to 95% confidence for both the high and low Ns estimates
  High.mean <- mean(as.numeric(Est.High))
  High.sd <- sd(as.numeric(Est.High))
  High.n <- length(as.numeric(Est.High))
  High.error <- qnorm(0.975)*High.sd/sqrt(High.n)
  
  
  Low.mean <- mean(as.numeric(Est.Low))
  Low.sd <- sd(as.numeric(Est.Low))
  Low.n <- length(as.numeric(Est.Low))
  Low.error <- qnorm(0.975)*Low.sd/sqrt(Low.n)
  
  # Calculating average family size and 95% confidence intervals
  avg.high.exc.fam <- mean(as.numeric(exc.high.fam.size))
  high.fam.sd <- sd(as.numeric(exc.high.fam.size))
  high.fam.n <- length(as.numeric(exc.high.fam.size))
  high.fam.error <- qnorm(0.975)*high.fam.sd/sqrt(high.fam.n)
  
  avg.low.exc.fam <- mean(as.numeric(exc.low.fam.size))
  low.fam.sd <- sd(as.numeric(exc.low.fam.size))
  low.fam.n <- length(as.numeric(exc.low.fam.size))
  low.fam.error <- qnorm(0.975)*low.fam.sd/sqrt(low.fam.n)
  
  
  # Binding all the data into r objects
  parameters <- cbind(inc.prob, exc.prob.low, exc.prob.high)
  summary <- cbind(Low.mean, Low.error, High.mean, High.error, avg.low.exc.fam, low.fam.error, low.fam.n, avg.high.exc.fam, high.fam.error, high.fam.n)
  data <- cbind(Est.Low, Est.High)
  
  
  # Writing these r objects with our analyzed data to a csv
  
  file.name <- paste("Nsinfo","_","real",".csv", sep = "")
  
  write.table(parameters, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
  
  write("", file = file.name, append = TRUE)
  
  write.table(summary, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
  
  write("", file = file.name, append = TRUE)
  
  write.table(data, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
}



# A function to analyze simulated data, with the specific format of "parent1xparent2-ID#". It returns many more measures than the real data function, but should only be used for exploring inclusive and exclusive probabilities in simulated data. It calculated Ns in an identical way to the real data function. 
sim_FSfamilies <- function(inc.prob, exc.prob.low, exc.prob.high, trueNs){
  # Setting the number of different kinds of families. full.sibs represent properly reconstructed full sibling families, excluding full sib families of size 1. half.sibs represent half sibling families represented as full sibs by Colony, and non.sibs represents totally unrelated families. Total just counts up all the rows in every one of the FSFamily files for determining various success or failure rates.
  FSfiles <- list.files(getwd(), recursive = TRUE, pattern=".BestFSFamily")
  Est.Low <- list()
  Est.High <- list()
  successes <- list()
  
  # Setting up lists for the number of individuals in each family, used for calculating average family sizes. 
  exc.low.fam.size <- list()
  exc.high.fam.size <- list()
  
  
  # This is the meat of the script. They are a bunch of if/else statements that search for number of breeder values assuming 2 breeders per family, based on inclusive and exclusive probability thresholds that you can set. Furthermore, there are two such loops for low and high Ns because we chose to use 0.15 and 0.20 as exclusive probability thresholds, and calculated Ns based on both thresholds.
  
  full.sibs <- 0
  half.sibs <- 0
  non.sibs <- 0
  singletons <- 0
  total <- 0  
  split.families <- 0
  
  for (file in FSfiles)
  {  
    table <- read.table(file, header = TRUE, sep = "")
    # A loop to investigate the number of full, half, and non-sib families in simulated data only! Requires the "Parent1xParent2-ID" format for individuals.
    for (i in 1:nrow(table))
    {
      # Total is the number of families finds. Distinct from singletons, which are the number of single individuals that we calculate based on inclusive probability and Colony's single individuals.
      total <- total + 1
      # Split up our family rows into the individual siblings, then remove the meaningless "-number" id's added to give Colony unique individuals to work with.
      row.data <- tstrsplit(as.vector(table[i,4]), ",")
      row.data <- gsub("-.*", "", row.data)
      row.data <- t(row.data)
      # Create an empty matrix, where rows are the offspring and columns are parent 1 and parent 2
      parent.list <- matrix(nrow = length(row.data), ncol = 2, dimnames = list(c(),c("Parent1","Parent2")))
      # For every 2 parents for each sibling in a family
      for (h in 1:ncol(row.data))
      {
        # Take one parent at a time and split the list, then append the matrix we've made with that parent we've split 
        for (j in 1:ncol(parent.list))
        {
          split.parents <- unlist(strsplit(row.data[,h], "x", fixed = TRUE))
          parent.list[h,j] <- split.parents[j]
        }
      }
      # If the inclusive probability is lower than the threshold AND if there are more than 1 sibling in the list, split the list and count how many siblings there are to get an increased number of single siblings. 
      full.relatedness <- 0
      half.relatedness <- 0
      no.relatedness <- 0
      if (isTRUE(table$Prob.Inc..[i] < inc.prob) & isTRUE(nrow(parent.list) > 1)){
        singletons <- singletons + length(tstrsplit(as.vector(table[i,4]), ","))
        # But if the inclusive probability is equal to or greater than the threshold, and there is more than one sibling in the family, investigate full and half sib relationships reconstructed by Colony.
      } else if (isTRUE(table$Prob.Inc..[i] >= inc.prob) & isTRUE(nrow(parent.list) > 1)){
        # Create one matrix of the parents used to search "test.parents" and a matrix of the rest of the parents "other.parents".
        for (f in 1:nrow(parent.list)){
          test.parents <- parent.list[f,]
          other.parents <- parent.list[-f,]
          # Test these matrices of parents against each other under different conditions: do neither match, or one out of two match, or do they both match? Add up the matches in temporary values. 
          if (!(test.parents[1] %in% other.parents[]) & !(test.parents[2] %in% other.parents[])){
            no.relatedness <- no.relatedness + 1
          } else if ((test.parents[1] %in% other.parents[]) & (test.parents[2] %in% other.parents[])){
            full.relatedness <- full.relatedness + 1
          } else if (xor((test.parents[1] %in% other.parents[]), (test.parents[2] %in% other.parents[]))){
            half.relatedness <- half.relatedness + 1
          }
          rm(other.parents)
        }
      } else if (nrow(parent.list) == 1){
        singletons <- singletons + 1
      } else{
        break
        print("Error: unaccounted for families")
      }
      
      # Now look at these temporary values and decide whether the family as a whole belongs in one of three categories: erroneously created non-siblings, half-siblings lumped by Coony, or true full siblings.    
      if (no.relatedness > 0){
        non.sibs <- non.sibs + 1
      } else if (no.relatedness == 0 & half.relatedness > 0){
        half.sibs <- half.sibs + 1
      } else if (no.relatedness == 0 & half.relatedness == 0 & full.relatedness > 0){
        full.sibs <- full.sibs + 1
      } else {
      }
    }
    
    # These are two loops that investigate how many full sibling families are erroneously split by Colony using simulated data only. 
    # First, all parents in a data set are added to a list, then the unique ones are extracted as an index with which to check the families. 
    all.parents <- list()
    for (fam in 1:nrow(table))
    {
      fam.data <- tstrsplit(as.vector(table[fam,4]), ",")
      fam.data <- gsub("-.*", "", fam.data)
      fam.data <- t(fam.data)
      for (parent in 1:ncol(fam.data))
      {
        all.parents <- list.append(all.parents, fam.data[parent])
      }
    }
    
    unique.parents <- as.matrix(unique(all.parents))
    unique.parents <- cbind(unique.parents, -1)
    
    # Here, the unique parents are used as an index and the families are iterated through, checking every parent against every row and counting up the number of times each parent appears. If the family row has an acceptable inclusive probability, it's counted normally If its inclusive is below the threshold, the number of siblings in it are added to the erroneously split family total. 
    for (parent in 1:nrow(unique.parents))
    {
      for (fam in 1:nrow(table))
      {
        fam.data <- tstrsplit(as.vector(table[fam,4]), ",")
        fam.data <- gsub("-.*", "", fam.data)
        fam.data <- t(fam.data)
        if (unique.parents[parent,1] %in% fam.data)
        {
          if (table[fam,]$Prob.Inc.. >= inc.prob)
          {
            unique.parents[parent,2] <- as.numeric(unique.parents[parent,2]) + 1
          } else if (table[fam,]$Prob.Inc.. < inc.prob){
            unique.parents[parent,2] <- as.numeric(unique.parents[parent,2]) + length(fam.data)
          }
        }
      }
    }
    split.families <- split.families + sum(as.numeric(unique.parents[,2]))

    # This for loop returns lists of family sizes for each exclusive probability, used for calculating average family size later. It splits families into single individuals when a family's inclusive probability is lower than the threshold, and ignores families with exclusive probabilities below chosen thresholds. 
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        for (j in 1:length(sibs))
        {
          exc.high.fam.size <- list.append(exc.high.fam.size, 1)
        }
        rm(sibs)
      } else if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] >= inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        exc.high.fam.size <- list.append(exc.high.fam.size, length(sibs))
        rm(sibs)
      }
      
      if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        for (j in 1:length(sibs))
        {
          exc.low.fam.size <- list.append(exc.low.fam.size, 1)
        }
        rm(sibs)
      } else if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] >= inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        exc.low.fam.size <- list.append(exc.low.fam.size, length(sibs))
        rm(sibs)
      }
    }
    
    # Loops to count up the number of breeders under two different exclusive probabilities
    # First, a loop based on the lower of the two exclusive probabilities, which returns the higher of the Ns estimates.
    Nshigh <- 0
    
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        Nshigh <- Nshigh + length(sibs)
        rm(sibs)
      }  else if (table$Prob.Exc..[i] >= exc.prob.low & table$Prob.Inc..[i] >= inc.prob){
        Nshigh <- Nshigh + 1
      }
    }
    
    Nshigh <- Nshigh * 2
    
    # Then a loop based on the higher of the exclusive probabilities that returns the lower Ns estimate.
    Nslow <- 0
    
    for (i in 1:nrow(table))
    {
      if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] < inc.prob){
        sibs <- tstrsplit(as.vector(table[i,4]), ",")
        Nslow <- Nslow + length(sibs)
        rm(sibs)
      }  else if (table$Prob.Exc..[i] >= exc.prob.high & table$Prob.Inc..[i] >= inc.prob){
        Nslow <- Nslow + 1
      }
    }
    Nslow <- Nslow * 2
    
    
    Est.Low <- list.append(Est.Low, Nslow)
    
    Est.High <- list.append(Est.High, Nshigh)
    
    # A simpel if else statement to check if the estimated Ns values contain the true Ns value for simulated data only.
    if (Nslow <= trueNs & Nshigh >= trueNs){
      success <- 1
    } else {
      success <- 0
    }
    successes <- list.append(successes, success)
  }
  
  
  #Calculating error rates for various reconstructed families as applied to sim data only.
  
  non.sib.error <- non.sibs/total
  half.sib.error <- half.sibs/total
  split.fam.error <- split.families/total
  
  # Calculating error from a normal distribution to 95% confidence for both the high and low Ns estimates
  
  High.mean <- mean(as.numeric(Est.High))
  High.sd <- sd(as.numeric(Est.High))
  High.n <- length(as.numeric(Est.High))
  High.error <- qnorm(0.975)*High.sd/sqrt(High.n)
  
  Low.mean <- mean(as.numeric(Est.Low))
  Low.sd <- sd(as.numeric(Est.Low))
  Low.n <- length(as.numeric(Est.Low))
  Low.error <- qnorm(0.975)*Low.sd/sqrt(Low.n)
  
  # Calculating average family size and 95% confidence intervals
  avg.high.exc.fam <- mean(as.numeric(exc.high.fam.size))
  high.fam.sd <- sd(as.numeric(exc.high.fam.size))
  high.fam.n <- length(as.numeric(exc.high.fam.size))
  high.fam.error <- qnorm(0.975)*high.fam.sd/sqrt(high.fam.n)
  
  avg.low.exc.fam <- mean(as.numeric(exc.low.fam.size))
  low.fam.sd <- sd(as.numeric(exc.low.fam.size))
  low.fam.n <- length(as.numeric(exc.low.fam.size))
  low.fam.error <- qnorm(0.975)*low.fam.sd/sqrt(low.fam.n)
  
  
  Success.rate <- sum(as.numeric(successes))/length(as.numeric(successes))
  
  # Binding all the data into r objects
  parameters <- cbind(inc.prob, exc.prob.low, exc.prob.high, trueNs)
  summary <- cbind(Low.mean, Low.error, High.mean, High.error, avg.low.exc.fam, low.fam.error, low.fam.n, avg.high.exc.fam, high.fam.error, high.fam.n, Success.rate)
  errors <- cbind(non.sib.error, half.sib.error, split.fam.error)
  total.families <- cbind(non.sibs, half.sibs, full.sibs, singletons, split.families,total)
  data <- cbind(Est.Low, Est.High, successes)
  
  
  # Writing these r objects with our analyzed data to a csv
  
  file.name <- paste("Nsinfo","_",trueNs,"_","sim",".csv", sep = "")
  
  write.table(parameters, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

  write("", file = file.name, append = TRUE)

  write.table(summary, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

  write("", file = file.name, append = TRUE)

  write.table(errors, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

  write.table(total.families, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)

  write("", file = file.name, append = TRUE)

  write.table(data, file = file.name, quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
}
