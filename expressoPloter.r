library(dplyr)
library(ggplot2)
library(scales)

loadSummary <- function(cutoff)
{
    # Read summarized data
    allData <- read.csv("data/summarizedData.csv", sep = ";")
    
    # Subset as desired for plotting
    allData <- subset(allData, allData$expressoNb > cutoff)

    return(allData)
}

savePlot <- function(url)
{
    # Save plot to disk
    ggsave(plot = last_plot(), 
           filename = url)
}

plotProfit <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Plot profit pre-Challenge prizes
    myPlot <- ggplot(allData, aes(expressoNb, profit)) + 
        geom_point(size = 2, aes(color = averageBuyin)) + 
        ggtitle(bquote(atop(.("Expresso results"), 
                            atop(italic(.("min. 2000 Expressos played")), "")))) + 
        labs(x = "Number of Expressos played", 
             y = "Winnings (in Euros)", 
             color = "Average Buyin") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 10))
    options(scipen = 1000000)
    print(myPlot)
    savePlot("figures/ProfitPreChal.pdf")
    
    # Plot profit post-Challenge prizes
    newPlot <- ggplot(allData, aes(expressoNb, profitPostChall)) + 
        geom_point(size = 2, aes(color = averageBuyin)) + 
        ggtitle("Results (post-Challenge prizes)") + 
        labs(x = "Number of Expressos played", 
             y = "Winnings (in Euros)", 
             color = "Average Buyin") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 10))
    options(scipen = 1000000)
    print(newPlot)
    savePlot("figures/ProfitPostChal.pdf")
}

plotChallengeImpact <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Plot it
    myPlot <- ggplot(allData, aes(expressoNb, profit)) + 
        geom_segment(x = allData$expressoNb, 
                     y = allData$profit, 
                     xend = allData$expressoNb, 
                     yend = allData$profitPostChall, 
                     color = "orange") + 
        geom_point(size = 2, aes(color = averageBuyin)) + 
        geom_point(data = allData, 
                   aes(expressoNb, profitPostChall, color = averageBuyin), 
                   size = 2) + 
        ggtitle(bquote(atop(.("Impact of challenge prizes"), 
                            atop(italic(.("high volume players")), "")))) + 
        labs(x = "Number of Expressos played", 
             y = "Winnings (in Euros)", 
             color = "Average Buyin") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 20)) +
        ylim(c(-70000, 70000)) 
    print(myPlot)
    savePlot("figures/ChallengeImpact.pdf")
}

plotRbImpact <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Plot it
    myPlot <- ggplot(allData, aes(expressoNb, profit)) + 
        geom_segment(aes(x = expressoNb, 
                         y = profit, 
                         xend = expressoNb, 
                         yend = profitPostChall, 
                         color = "Challenge prizes"), 
                     size = 1) + 
        geom_segment(aes(x = expressoNb, 
                         y = profitPostChall, 
                         xend = expressoNb, 
                         yend = profitPostRbD3, 
                         color = "Rakeback D3"), 
                     size = 1) + 
        geom_segment(aes(x = expressoNb, 
                         y = profitPostRbD3, 
                         xend = expressoNb, 
                         yend = profitPostRbD4, 
                         color = "Rakeback D4"), 
                     size = 1) + 
        geom_segment(aes(x = expressoNb, 
                         y = profitPostRbD4, 
                         xend = expressoNb, 
                         yend = profitPostRbD5, 
                         color = "Rakeback D5"), 
                     size = 1) + 
        geom_segment(aes(x = expressoNb, 
                         y = profitPostRbD5, 
                         xend = expressoNb, 
                         yend = profitPostRbRedD, 
                         color = "Rakeback Red Diamond"), 
                     size = 1) + 
        scale_color_brewer(palette = "OrRd") + 
        geom_point(size = 2) + 
        #geom_point(data = allData, 
        #           aes(expressoNb, profitPostChall), 
        #           size = 2, alpha = 0.3) + 
        #geom_point(data = allData, 
        #           aes(expressoNb, profitPostRbD3), 
        #           size = 2) + 
        ggtitle(bquote(atop(.("Impact of challenges + rakeback"), 
                            atop(italic(.("high volume players")), "")))) + 
        labs(x = "Number of Expressos played", 
             y = "Winnings (in Euros)", 
             color = "Other revenues") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 20)) +
        ylim(c(-70000, 70000)) 
    print(myPlot)
    savePlot("figures/RbImpact.pdf")
}

plotProfitPreChalDistrib <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Plot it
    myPlot <- ggplot(allData, aes(profit)) + 
        geom_histogram(color = "black", fill = "blue", alpha = .5) + 
        ggtitle("Distribution of results") + 
        labs(x = "Winnings (in Euros)", 
             y = "Number of players") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 20)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 20))
    options(scipen = 1000000)
    print(myPlot)
    savePlot("figures/ProfitPreChalDistrib.pdf")
    
    # Now remove outliers and plot again
    newPlot <- myPlot + 
        xlim(c(-30000, 30000)) + 
        ggtitle("Distribution of results without outliers") 
    print(newPlot)
    savePlot("figures/ProfitPreChalDistribNoOutlier.pdf")
}

plotItm <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Plot it
    myPlot <- ggplot(allData, aes(expressoNb, itmPercent)) + 
        geom_point(size = 2, aes(color = averageBuyin)) + 
        ggtitle(bquote(atop(.("ITM percentages"), 
                            atop(italic(.("high volume players")), "")))) + 
        labs(x = "Number of Expressos played", 
             y = "ITM percentage", 
             color = "Average Buyin") + 
        theme(plot.title = element_text(face="bold"), 
              axis.title = element_text(face="bold")) + 
        scale_x_continuous(breaks = pretty_breaks(n = 10)) + 
        scale_y_continuous(breaks = pretty_breaks(n = 10)) + 
        geom_hline(yintercept = 35.8, color = "red") + 
        geom_hline(yintercept = 36.5, color = "orange") + 
        geom_hline(yintercept = 37.2, color = "green") +
        geom_text(x = 30000, y = 35.8,label = "35.8", color = "red", 
                  vjust = 1, hjust = -0.5) + 
        geom_text(x = 30000, y = 36.5,label = "36.5", color = "orange", 
                  vjust = 1, hjust = -0.5) + 
        geom_text(x = 30000, y = 37.2,label = "37.2", color = "green", 
                  vjust = 1, hjust = -0.5)
    options(scipen = 1000000)
    print(myPlot)
    savePlot("figures/ProfitItm.pdf")
}

printQuantile <- function(cutoff)
{
    # Load the desired data
    allData <- loadSummary(cutoff)
    
    # Print quantile info
    print(summary(allData$profit))
}
