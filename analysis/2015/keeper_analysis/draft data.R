# Change directly to location of text files
setwd("/home/john/Fantasy Football/Raw Data Files")

# Pull in all the data
draft_10 <- read.table("Draft_2010", sep="\t", header=FALSE, fill=TRUE, quote = "",
                     stringsAsFactors = FALSE)
draft_11 <- read.table("Draft_2011", sep="\t", header=FALSE, fill=TRUE, quote = "",
                     stringsAsFactors = FALSE)
draft_12 <- read.table("Draft_2012", sep="\t", header=FALSE, fill=TRUE, quote = "",
                     stringsAsFactors = FALSE)
draft_13 <- read.table("Draft_2013", sep="\t", header=FALSE, fill=TRUE, quote = "",
                     stringsAsFactors = FALSE)
draft_14 <- read.table("Draft_2014", sep="\t", header=FALSE, fill=TRUE, quote = "",
                     stringsAsFactors = FALSE)
draft_15 <- read.table("Draft_2015",sep="\t", header=FALSE,fill=TRUE, quote = "",
                       stringsAsFactors = FALSE)
#Load necessary libraries
require(dplyr)
require(stringr)


# Start data manipulation
draft_10 <- mutate(draft_10,year="2010")
draft_11 <- mutate(draft_11,year="2011")
draft_12 <- mutate(draft_12,year="2012")
draft_13 <- mutate(draft_13,year="2013")
draft_14 <- mutate(draft_14,year="2014")
draft_15 <- mutate(draft_15,year="2015")

draft <- rbind(draft_10,draft_11,draft_12,draft_13,draft_14,draft_15)
draft$owner <- rep(
                rep(c("Hartman",
                     "Kane",
                     "Harrington",
                     "Higdon",
                     "Regan",
                     "Shokunbi",
                     "McShane",
                     "Thieneman",
                     "Matovina",
                     "Ready",
                     "Skrzyskewski",
                     "Olson"),
                   each = 16),
                6)
draft$V1 = str_trim(draft$V1)
draft$V2 = str_trim(draft$V2)

draft$pick   <- draft$V1
draft$player <- sapply(str_split(draft$V2,","), '[[', 1)
draft$s1 <- str_trim(sapply(str_split(draft$V2,","),'[',2))
draft$team   <- str_trim(sapply(str_split(draft$s1,' '), '[', 1))
draft$pos    <- str_sub(draft$s1,
                        start = str_locate(draft$s1,' ')[,1],
                        end   = str_locate(draft$s1,' ')[,1] + 2)
draft$s2 <- sapply(str_split(draft$s1,' '),'[',4)
draft <- mutate(draft, keeper = ifelse(s2 !='K' | is.na(s2),0,1))
draft$value <- as.numeric(str_replace(str_sub(draft$V3, start = str_locate(draft$V3,'\\$')[,1]),
                                    '\\$',
                                    '')
)

## Coaches and defenses in 2013, 2014 and 2015 don't read so nicely, so some more work needs to be done ##
null_value <- which(is.na(draft$pos))

draft$team[null_value] <- str_trim(str_sub(draft$player[null_value],
                                  end = str_locate(draft$player[null_value],
                                                   ' ')[,1]))
draft[null_value,]                            
draft$pos[null_value] <- str_trim(lapply(str_split(draft$player[null_value],' '),
                                         '[[',3))
colnames(draft)
draft <- draft %>% select(c(4,5,6,8,9,11,10))
head(draft)
write.csv(draft,"/home/john/stats_corner/2015/keeper_analysis/draft_data.csv")

