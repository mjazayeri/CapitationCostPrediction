library(ggplot2)
library(reshape2)

plotDistribution.Factor <- function(x, x.name, levels = NULL, vertical = FALSE) {
  
  if(is.null(levels)) {
    levels = levels(x)
  }
  
  p <- qplot(x, geom = "bar", fill = factor(x), xlab = "") +
    scale_fill_discrete(name = x.name, labels = levels) + 
    scale_x_discrete(labels = levels)
  if(!vertical) {
    p <- p + geom_text(aes(y=..count.., label=..count..), 
                       stat="count", color="white", size=3, hjust = 1.2) +
      coord_flip() 
  }
  else {
    p <- p + geom_text(aes(y=..count.., label=..count..), 
                       stat="count", color="white", size=3, vjust = 1.2, angle = 90)
  }
  return (p)
}

plotTopNDistribution.Factor <- function(x, n = 10, x.name, vertical = FALSE) {

  tbl <- as.data.frame(table(x))
  # select top n frequent rows
  tbl <- tbl[order(tbl$Freq, decreasing = T), ]
  tbl <- tbl[1:n, ]
  colnames(tbl) <- c("item", "count")
  
  p <- ggplot(tbl) + 
    geom_bar(aes(x = item, y = count, fill = item), stat="identity", position = "dodge", width = 0.7) + 
    xlab(x.name) + 
    ylab("count") + 
    scale_fill_discrete(x.name)
  
  if(!vertical) {
    p <- p + geom_text(aes(x = tbl$item, y=tbl$count, label=tbl$count), 
                       stat="identity", color="white", size=3, hjust = 1.2) +
      coord_flip()
  }
  else {
    p <- p + geom_text(aes(x = tbl$item, y=tbl$count, label=tbl$count), 
                       stat="identity", color="white", size=3, hjust = 1.2, angle = 90)
  }
  
  return (p)
}

plotDistribution.Numeric <- function(x, width = 30, xlab = "", hist = FALSE, min = 0, max = 50000) {
  
  min <- min * 1000
  max <- max * 1000
  p <- NULL
  x <- subset(x, x >= min & x <= max)
  if(hist) {
    p <- qplot(x, geom = "histogram", binwidth = (max - min)/20,
               fill = I("blue"), color = I("white")) +
      xlab(xlab)
  }
  else {
    df <- data.frame(key = x)
    p <- ggplot(df, aes(x = key, fill = I("blue"), color = I("white"))) + 
      geom_density(alpha = .8) + 
      xlab(xlab)
  }
  return(p)
}

plotDistribution.Numeric_Factor <- function(num, fact, fact.name, fact.levels = NULL) {
  if(is.null(fact.levels)) {
    fact.levels = levels(fact)
  }
  
  aggr <- aggregate(num ~ fact, FUN = sum)
  total <- sum(aggr$num)
  
  p <- ggplot(aggr, aes(x=factor(1), num / total, fill = fact)) + 
    geom_bar(width = 1, stat = "identity") + 
    coord_polar(theta = "y") + 
    xlab("") + 
    ylab("") + 
    scale_fill_discrete(fact.name, labels = fact.levels)
  
  return (p)
}

plotTopNDistribution.Numeric_Factor <- function(num, fact, fact.name, n = 5) {
  
  aggr <- aggregate(num ~ fact, FUN = sum)
  total <- sum(aggr$num)
  
  if(n >= 10) {
    n <- 9
  }
  
  aggr <- aggr[order(aggr$num, decreasing = T), ]
  aggr[, 1] <- as.character(aggr[, 1])
  final <- aggr[1:n, ]
  final <- rbind.data.frame(final, data.frame(fact = "Others", num = sum(aggr[(n+1):nrow(aggr), 2]) ))
  
  fact.level <- as.factor(final$fact)
  p <- ggplot(final, aes(x=factor(1), num / total, color = I("white") ,fill = as.factor(fact))) + 
    geom_bar(width = 1, stat = "identity") + 
    coord_polar(theta = "y") + 
    xlab("") + 
    ylab("") + 
    scale_fill_brewer(fact.name, palette = "Set3")
  
  return (p)
}

plotComparision.Factor <- function(x.year1, x.year2, x.name, 
                                   year1 = 2012, year2 = 2013, 
                                   filter = NULL, levels = NULL) {
  
  freq.year1 <- as.data.frame(table(x.year1))
  freq.year2 <- as.data.frame(table(x.year2))
  
  if(!is.null(filter)) {
    filter <- filter[order(filter)]
    freq.year1 <- freq.year1[freq.year1[, 1] %in% filter, ]
    freq.year2 <- freq.year2[freq.year2[, 1] %in% filter, ]
  }
  
  freq <- cbind.data.frame(freq.year1, freq.year2$Freq)
  colnames(freq) <- c("fact", "freq2013", "freq2014")
  
  freq.long <- melt(freq, id = "fact")
  rm(freq.year1, freq.year2, freq)
  if(is.null(levels)) {
    levels <- freq.long$fact
  }
  
  p <- ggplot(freq.long) +
    geom_bar(aes(x = fact, y = value, fill = variable), stat="identity", position = "dodge", width = 0.7) +
    scale_fill_manual(name = "Year", labels = as.character(c(year1, year2)), values = c("blue", "cyan")) + 
    scale_x_discrete(labels = levels) +
    labs(x = x.name, y = "count")
  
  rm(freq.long)
  return (p)
}

plotComparision.Numeric <- function(x.year1, year1 = 2012, x.year2, year2 = 2013, x.name, hist = FALSE,
                                    min = 0, max = 50000) {
  
  min <- min * 1000
  max <- max * 1000
  
  # Filter by min and max values
  x.year1 <- subset(x.year1, x.year1 <= max & x.year1 >= min)
  x.year2 <- subset(x.year2, x.year2 <= max & x.year2 >= min)
  
  # Create datafram containing both vectors
  df <- data.frame(year = year1 , key = x.year1)
  df <- rbind.data.frame(df, data.frame(year = year2, key = x.year2))
  
  p <- ggplot(df, aes(key, fill = factor(year), color = I("white")))
  
  if(hist) {
      p <- p + geom_histogram(position = "identity", alpha = 0.5)
  }
  else {
      p <- p + geom_density(alpha = .5)
  }
  
  p <- p + scale_fill_manual(name = "Year", labels = as.character(c(year1, year2))
                             , values = c("blue", "cyan")) +
    xlab(x.name) + 
    ylab("")
  
  return (p)
}

plotPredictedCost <- function(actual, predicted, hist = FALSE, min = 0, max = 50000) {
  
  min <- min * 1000
  max <- max * 1000
  # Filter by min and max values
  actual <- subset(actual, actual <= max & actual >= min)
  predicted <- subset(predicted, predicted <= max & predicted >= min)
  
  # Create datafram containing both vectors
  df <- data.frame(state = "Actual" , key = actual)
  df <- rbind.data.frame(df, data.frame(state = "Predicted", key = predicted))
  
  p <- ggplot(df, aes(key, fill = factor(state), color = I("white")))
  
  if(hist) {
    p <- p + geom_histogram(position = "identity", alpha = 0.5)
  }
  else {
    p <- p + geom_density(alpha = .5)
  }
  
  p <- p + scale_fill_manual(name = "State", labels = c("Actual", "Predicted"), values = c("blue", "cyan")) +
    xlab("Cost")
  
  return (p)
}
