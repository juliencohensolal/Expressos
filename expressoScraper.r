# TO DO : rake study
# TO DO : automatically create correct week.csv file name with date and stake
# TO DO : check for duplicates when merging data before plotting
# TO DO : improve performances

# LOG : can't parse players who changed nicks
# LOG : only parsed players with at least 100 expressos at 25E, 50 exp at 100E


library(httr)
library(XML)
library(stringr)

playerParser <- function(iRank, iURL, iFile, iLimit)
{
    # Retrieve HTML source code
    htmlDoc <- GET(iURL)
    
    # Check for error during retrieval (file not found for example)
    stop_for_status(htmlDoc)
    
    # Access the body of the request
    docContent <- content(htmlDoc, as = "text")
    
    # Put content in XML form
    parsedDoc = htmlParse(docContent)
    
    # Check for unexpected HTML result
    newsTitle <- xpathSApply(parsedDoc,'//*[@class="news-title"]', xmlValue)
    if(length(newsTitle) != 0) {return(-1)}
    
    # Find player name
    playerName <- xpathSApply(parsedDoc, "/descendant::h1[1]", xmlValue)
    
    # Init variables
    expressoNb <- 0
    profit <- 0
    totalBuyin <- 0
    totalJackpot <- 0
    places1 <- 0
    places2 <- 0
    places3 <- 0
    totalX2 <- 0
    totalX4 <- 0
    totalX6 <- 0
    totalX10 <- 0
    totalX20 <- 0
    totalX50 <- 0
    totalX200 <- 0
    totalX1000 <- 0
    totalX10000 <- 0
    
    # Loop on results
    results <- xpathSApply(parsedDoc, "/descendant::td", xmlValue)
    size <- length(results)
    i <- 1
    while(i <= size)
    {
        expressoNb <- expressoNb + 1
        
        # Retrieve results
        curDate <- results[i]
        curBuyin <- as.numeric(gsub(",", ".", results[i+2]))
        curJackpot <- gsub(" ", "", results[i+3])
        curJackpot <- as.numeric(gsub(",", ".", curJackpot))
        curRank <- as.numeric(gsub(",", ".", results[i+4]))
        
        # Update temporary values
        totalBuyin <- totalBuyin + curBuyin
        totalJackpot <- totalJackpot + curJackpot
        jackpotScale <- curJackpot/curBuyin
        switch(as.character(jackpotScale), 
               "2" = {totalX2 <- totalX2 + 1},
               "4" = {totalX4 <- totalX4 + 1},
               "6" = {totalX6 <- totalX6 + 1},
               "10" = {totalX10 <- totalX10 + 1},
               "20" = {totalX20 <- totalX20 + 1},
               "50" = {totalX50 <- totalX50 + 1},
               "200" = {totalX200 <- totalX200 + 1},
               "1000" = {totalX1000 <- totalX1000 + 1},
               "10000" = {totalX10000 <- totalX10000 + 1}
        )
        
        if(curRank == 1)
        {
            # Expresso won
            places1 <- places1 + 1
            if(jackpotScale < 50)   {profit <- profit + curJackpot - curBuyin}
            else
            {
                # Not a winner-take-all case
                profit <- profit + (0.8 * curJackpot) - curBuyin
            }
        }
        else
        {
            # Expresso lost
            if(curRank == 2)    {places2 <- places2 + 1}
            else                {places3 <- places3 + 1}
            
            if(jackpotScale < 50)   {profit <- profit - curBuyin}
            else
            {
                # Not a winner-take-all case
                if(curRank == 2)    {profit <- profit + (0.12 * curJackpot) - curBuyin}
                else                {profit <- profit + (0.08 * curJackpot) - curBuyin}
            }
        }
        
        i <- i + 5
    }

    # Check for captchas
    if (expressoNb == 0)    {return(-2)}

    # Only parse players with more than X Expressos in a week
    if (expressoNb < iLimit)   {return(-3)}

    # Write results to CSV file
    print(paste("Player", playerName, "parsed : ", expressoNb, "expressos"))
    finalValues <- matrix(
        c(iRank,
          playerName,
          expressoNb, 
          profit, 
          totalBuyin, 
          totalJackpot, 
          places1, 
          places2, 
          places3, 
          totalX2, 
          totalX4, 
          totalX6, 
          totalX10, 
          totalX20, 
          totalX50, 
          totalX200, 
          totalX1000, 
          totalX10000),
        nrow = 1, 
        ncol = 18)
    write.table(finalValues, 
                iFile, 
                append = TRUE, 
                col.names = FALSE, 
                row.names = FALSE, 
                sep = ";")

    return(1)
}

########################################
########################################

weekParser <- function(iURL, iFile, iLimit, iStart)
{
    # Retrieve HTML source code
    htmlDoc <- GET(iURL)

    # Check for error during retrieval (file not found for example)
    stop_for_status(htmlDoc)

    # Access the body of the request
    docContent <- content(htmlDoc, as = "text")

    # Put content in XML form
    parsedDoc = htmlParse(docContent)
    
    # Loop on results
    results <- xpathSApply(parsedDoc, "/descendant::td", xmlValue)
    size <- length(results) - iStart
    i <- ((iStart - 1) * 4) + 1
    while(i < size)
    {
        # Retrieve results
        curRank <- as.numeric(results[i])
        curName <- results[i+1]
        print(paste(curRank, "-", curName))
        
        # Encode name
        curName <- str_replace_all(curName, "\\s", "%2B")
        curName <- str_replace_all(curName, "&", "%26")

        # Create URL
        weekCode <- gsub("^.*?&lb=", "", iURL)
        curURL = paste("https://www.winamax.fr/poker/challenges/leaderboard_detail.php?user=",
                       curName,
                       "&lb=", 
                       weekCode, 
                       "&d=1010467",
                       sep = "")
        
        # If file doesn't already exist, create it with a header
        if (!file.exists(iFile))
        {
            allData <- data.frame(rank = integer(),
                                  playerName = factor(),
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
                                  stringsAsFactors=FALSE)
            write.table(x = allData, 
                        file = iFile, 
                        row.names = FALSE, 
                        sep = ";")
        }

        # Parse player page
        returnCode <- playerParser(curRank, curURL, iFile, iLimit)
        if (returnCode == -1)   {print("WARNING : Parsing of player name returns incorrect value")}
        if (returnCode == -2)   {return("ERROR : Stopping for captcha")}
        if (returnCode == -3)   {return("Parsed all players who played more than Expresso limit")}
        
        i <- i + 4
    }
}
