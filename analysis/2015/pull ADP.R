library(XML)
library(dplyr)


url10 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2010&teams=12&view=graph&pos=all"
url11 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2011&teams=12&view=graph&pos=all"
url12 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2012&teams=12&view=graph&pos=all"
url13 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2013&teams=12&view=graph&pos=all"
url14 <- "http://fantasyfootballcalculator.com/adp.php?format=standard&year=2014&teams=12&view=graph&pos=all"

data10 <- readHTMLTable(url10, as.data.frame=TRUE, stringsAsFactors = FALSE)$'NULL'
data11 <- readHTMLTable(url11, as.data.frame=TRUE, stringsAsFactors = FALSE)$'NULL'
data12 <- readHTMLTable(url12, as.data.frame=TRUE, stringsAsFactors = FALSE)$'NULL'
data13 <- readHTMLTable(url13, as.data.frame=TRUE, stringsAsFactors = FALSE)$'NULL'
data14 <- readHTMLTable(url14, as.data.frame=TRUE, stringsAsFactors = FALSE)$'NULL'

data10$year <- 2010
data11$year <- 2011
data12$year <- 2012
data13$year <- 2013
data14$year <- 2014

data <- rbind(data10,data11,data12,data13,data14)
colnames(data) <- tolower(colnames(data))
data <- select(data,year,name,pos,team,overall)
data$overall <- as.numeric(data$overall)

data2 <- data %>%
  group_by(year,pos) %>%
  mutate(pick = rank(overall))

setwd("/home/john/Fantasy Football/2015 Prep")
write.csv(data2,"adp.csv")
