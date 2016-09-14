library(dplyr)

processAll <- function()
{
    computeStats()

    createSummaryCsv()
}

computeStats <- function()
{
    # Loop through all weekly .csv files
    myFiles <- list.files(path = "data/raw data/", 
                          pattern = "\\.csv$", 
                          ignore.case=TRUE)
    size <- length(myFiles)
    for(i in 1:size)
    {
        print(paste("Computing stats from file #", i, "out of", size))

        # Read data from current CSV file
        curURL <- paste("data/raw data/",
                        myFiles[i],
                        sep = "")
        curData <- read.csv(curURL, sep = ";")
        nbPlayers <- nrow(curData)
        
        # Add columns for processed data
        curData[, "averageBuyin"] <- numeric()
        curData[, "itmPercent"] <- numeric()
        curData[, "effectiveRake"] <- numeric()
        curData[, "totalRake"] <- numeric()
        curData[, "challengePrize"] <- numeric()
        curData[, "profitPostChall"] <- numeric()
        curData[, "miles"] <- numeric()
        curData[, "milesD3"] <- numeric()
        curData[, "milesD4"] <- numeric()
        curData[, "milesD5"] <- numeric()
        curData[, "milesRedD"] <- numeric()
        
        # Add challenge prize to data frame
        curData <- addChallengePrize(curData, curURL)
        
        # Loop on each player
        for(j in 1:nbPlayers)
        {
            # Compute stats
            averageBuyin <- round(curData[j, which(colnames(curData) == "totalBuyin")] / curData[j, which(colnames(curData) == "expressoNb")], 2)
            itmPercent <- round((100 * curData[j, which(colnames(curData) == "places1")]) / curData[j, which(colnames(curData) == "expressoNb")], 2)
            effectiveRake <- round((100 * (curData[j, which(colnames(curData) == "totalBuyin")] - (curData[j, which(colnames(curData) == "totalJackpot")] / 3))) / curData[j, which(colnames(curData) == "totalBuyin")], 2)
            totalRake <- round(curData[j, which(colnames(curData) == "totalBuyin")] * 0.07, 2)
            miles <- round(totalRake * 4, 2)
            milesD3 <- round(miles * 4, 2)
            milesD4 <- round(miles * 4.5, 2)
            milesD5 <- round(miles * 5, 2)
            milesRedD <- round(miles * 6, 2)
            
            # Add stats to data frame
            curData[j, which(colnames(curData) == "averageBuyin")] <- averageBuyin
            curData[j, which(colnames(curData) == "itmPercent")] <- itmPercent
            curData[j, which(colnames(curData) == "effectiveRake")] <- effectiveRake
            curData[j, which(colnames(curData) == "totalRake")] <- totalRake
            curData[j, which(colnames(curData) == "miles")] <- miles
            curData[j, which(colnames(curData) == "milesD3")] <- milesD3
            curData[j, which(colnames(curData) == "milesD4")] <- milesD4
            curData[j, which(colnames(curData) == "milesD5")] <- milesD5
            curData[j, which(colnames(curData) == "milesRedD")] <- milesRedD
            
            # Add profit + Challenge column
            profitPostChall <- curData[j, which(colnames(curData) == "profit")] + curData[j, which(colnames(curData) == "challengePrize")]
            curData[j, which(colnames(curData) == "profitPostChall")] <- profitPostChall
        }
        
        # Save processed data file
        write.table(curData, 
                    file=paste("data/processed data/", myFiles[i], sep = ""), 
                    sep = ";",
                    quote = FALSE, 
                    row.names = FALSE)
    }
}

########################################
########################################

addChallengePrize <- function(curData, curURL)
{
    # Check stakes being played
    stakes = 0
    if(grepl("25E",curURL) == TRUE)   {stakes = 25}
    if(grepl("100E",curURL) == TRUE)  {stakes = 100}
    
    # Loop on each player
    if (stakes != 0)
    {
        nbPlayers <- nrow(curData)
        for(i in 1:nbPlayers)
        {
            # Get relevant prize
            prize = 0
            curRank <- curData[i, which(colnames(curData) == "rank")]
            if (stakes == 25)
            {
                if (curRank == 1)   {prize <- 2250}
                if (curRank == 2)   {prize <- 1500}
                if (curRank == 3)   {prize <- 1000}
                if (curRank == 4)   {prize <- 750}
                if (curRank == 5)   {prize <- 500}
                if (findInterval(curRank, c(6:7) ) > 0)     {prize <- 375}
                if (findInterval(curRank, c(8:10) ) > 0)    {prize <- 225}
                if (findInterval(curRank, c(11:15) ) > 0)   {prize <- 150}
                if (findInterval(curRank, c(16:25) ) > 0)   {prize <- 75}
                if (findInterval(curRank, c(26:50) ) > 0)   {prize <- 50}
                if (findInterval(curRank, c(51:100) ) > 0)  {prize <- 25}
                if (curRank > 100)  {prize <- 0}
            }
            if (stakes == 100)
            {
                if (curRank == 1)   {prize <- 3500}
                if (curRank == 2)   {prize <- 2250}
                if (curRank == 3)   {prize <- 1500}
                if (curRank == 4)   {prize <- 1000}
                if (curRank == 5)   {prize <- 750}
                if (findInterval(curRank, c(6:7) ) > 0)     {prize <- 500}
                if (findInterval(curRank, c(8:10) ) > 0)    {prize <- 350}
                if (findInterval(curRank, c(11:15) ) > 0)   {prize <- 250}
                if (findInterval(curRank, c(16:25) ) > 0)   {prize <- 150}
                if (findInterval(curRank, c(26:50) ) > 0)   {prize <- 100}
                if (findInterval(curRank, c(51:100) ) > 0)  {prize <- 50}
                if (curRank > 100)  {prize <- 0}
            }
            
            # Add prize to data frame
            curData[i, which(colnames(curData) == "challengePrize")] <- prize
        }
    }
    
    return(curData)
}    

########################################
########################################

computeRB <- function(allData)
{
    # Loop on each player
    nbPlayers <- nrow(allData)
    for(i in 1:nbPlayers)
    {
        milesD3 <- as.numeric(allData[i, which(colnames(allData) == "milesD3")])
        milesD4 <- as.numeric(allData[i, which(colnames(allData) == "milesD4")])
        milesD5 <- as.numeric(allData[i, which(colnames(allData) == "milesD5")])
        milesRedD <- as.numeric(allData[i, which(colnames(allData) == "milesRedD")])
        
        # Compute RB for all levels
        rbD3 <- round((milesD3 / 333330) * 5000, 2)
        rbD4 <- round((milesD4 / 333330) * 5000, 2)
        rbD5 <- round((milesD5 / 333330) * 5000, 2)
        rbRedD <- round((milesRedD / 333330) * 5000, 2)
        
        # Write in summary file
        allData[i, which(colnames(allData) == "rbD3")] <- rbD3
        allData[i, which(colnames(allData) == "rbD4")] <- rbD4
        allData[i, which(colnames(allData) == "rbD5")] <- rbD5
        allData[i, which(colnames(allData) == "rbRedD")] <- rbRedD
        
        profitPostChall <- as.numeric(allData[i, which(colnames(allData) == "profitPostChall")])
        allData[i, which(colnames(allData) == "profitPostRbD3")] <- round(rbD3 + profitPostChall, 2)
        allData[i, which(colnames(allData) == "profitPostRbD4")] <- round(rbD4 + profitPostChall, 2)
        allData[i, which(colnames(allData) == "profitPostRbD5")] <- round(rbD5 + profitPostChall, 2)
        allData[i, which(colnames(allData) == "profitPostRbRedD")] <- round(rbRedD + profitPostChall, 2)
    }
    
    return(allData)
}

########################################
########################################

createSummaryCsv <- function()
{
    # Create an empty dataframe which will contain all data
    allData <- data.frame(rank = integer(),
                          playerName = character(),
                          expressoNb = integer(),
                          profit = numeric(),
                          totalBuyin = numeric(),
                          totalJackpot = numeric(),
                          places1 = integer(),
                          places2 = integer(),
                          places3 = integer(),
                          totalX2 = integer(),
                          totalX4 = integer(),
                          totalX6 = integer(),
                          totalX10 = integer(),
                          totalX20 = integer(),
                          totalX50 = integer(),
                          totalX200 = integer(),
                          totalX1000 = integer(),
                          totalX10000 = integer(),
                          itmPercent = numeric(),
                          averageBuyin = numeric(),
                          effectiveRake = numeric(),
                          totalRake = numeric(),
                          challengePrize = numeric(),
                          profitPostChall = numeric(),
                          miles = numeric(),
                          milesD3 = numeric(),
                          milesD4 = numeric(),
                          milesD5 = numeric(),
                          milesRedD = numeric(),
                          rbD3 = numeric(),
                          rbD4 = numeric(),
                          rbD5 = numeric(),
                          rbRedD = numeric(),
                          profitPostRbD3 = numeric(),
                          profitPostRbD4 = numeric(),
                          profitPostRbD5 = numeric(),
                          profitPostRbRedD = numeric(),
                          stringsAsFactors=FALSE)
    
    # Loop through all weekly .csv files to create a unique dataframe
    myFiles <- list.files(path = "data/processed data/", 
                          pattern = "\\.csv$", 
                          ignore.case=TRUE)
    size <- length(myFiles)
    for(i in 1:size)
    {
        # Read data from current CSV file
        curURL <- paste("data/processed data/",
                        myFiles[i],
                        sep = "")
        curData <- read.csv(curURL, sep = ";")
        
        if (i != 1) 
        {
            # Add rows to data from previous csv files
            allData <- rbind(allData, curData)
        }
        else    
        {
            # It is the first file
            allData <- curData
        }
    }
    
    # Summarize data
    allData <- group_by(allData, playerName)
    allData <- summarize(allData, 
                         expressoNb = sum(expressoNb), 
                         profit = sum(profit), 
                         totalBuyin = sum(totalBuyin), 
                         totalJackpot = sum(totalJackpot), 
                         places1 = sum(places1), 
                         places2 = sum(places2), 
                         places3 = sum(places3), 
                         totalX2 = sum(totalX2), 
                         totalX4 = sum(totalX4), 
                         totalX6 = sum(totalX6), 
                         totalX10 = sum(totalX10), 
                         totalX20 = sum(totalX20), 
                         totalX50 = sum(totalX50), 
                         totalX200 = sum(totalX200), 
                         totalX1000 = sum(totalX1000), 
                         totalX10000 = sum(totalX10000), 
                         itmPercent = round((100 * sum(places1)) / sum(expressoNb), 2), 
                         averageBuyin = round(sum(totalBuyin) / sum(expressoNb), 2), 
                         effectiveRake = round((100 * (sum(totalBuyin) - (sum(totalJackpot) / 3))) / sum(totalBuyin), 2), 
                         totalRake = sum(totalRake), 
                         challengePrize = sum(challengePrize), 
                         profitPostChall = sum(profitPostChall), 
                         miles = sum(miles), 
                         milesD3 = sum(milesD3), 
                         milesD4 = sum(milesD4), 
                         milesD5 = sum(milesD5), 
                         milesRedD = sum(milesRedD),  
                         rbD3 = numeric(),
                         rbD4 = numeric(),
                         rbD5 = numeric(),
                         rbRedD = numeric(), 
                         profitPostRbD3 = numeric(),
                         profitPostRbD4 = numeric(),
                         profitPostRbD5 = numeric(),
                         profitPostRbRedD = numeric()) 

    # Order data
    allData <- arrange(allData, -expressoNb)
    
    # Add rakeback
    allData <- computeRB(allData)
    
    # Save summarized data to disk 
    write.table(allData, 
                file="data/summarizedData.csv", 
                sep = ";",
                quote = FALSE, 
                row.names = FALSE)
    
    return(allData)
}
