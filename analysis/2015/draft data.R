# Change directly to location of text files
setwd("/home/john/Fantasy Football/Raw Data Files")

# Pull in all the data
draft_10 <- read.csv("Draft_2010.csv", header=FALSE, stringsAsFactors=FALSE)
draft_11 <- read.csv("Draft_2011.csv", header=FALSE, stringsAsFactors=FALSE)
draft_12 <- read.csv("Draft_2012.csv", header=FALSE, stringsAsFactors=FALSE)
draft_13 <- read.csv("Draft_2013.csv", header=FALSE, stringsAsFactors=FALSE)
draft_14 <- read.csv("Draft_2014.csv", header=FALSE, stringsAsFactors=FALSE)


#Load necessary libraries
require(dplyr)
require(stringr)


# Good practice
d10_df <- tbl_df(draft_10)
d11_df <- tbl_df(draft_11)
d12_df <- tbl_df(draft_12)
d13_df <- tbl_df(draft_13)
d14_df <- tbl_df(draft_14)
# Start data manipulation
d10_df <- mutate(d10_df,year="2010")
d11_df <- mutate(d11_df,year="2011")
d12_df <- mutate(d12_df,year="2012")
d13_df <- mutate(d13_df,year="2013")
d14_df <- mutate(d14_df,year="2014")

draft <- rbind(d10_df,d11_df,d12_df,d13_df,d14_df)
draft$V1 = str_trim(draft$V1)
draft$V2 = str_trim(draft$V2)

draft$pick   <- sapply(str_split(draft$V1,'\t'), '[[', 1)
draft$player <- sapply(str_split(draft$V1,'\t'), '[[', 2)
draft$s1     <- sapply(str_split(draft$V2,'\t'), '[[', 1)
draft$team   <- sapply(str_split(draft$s1,' '), '[[', 1)
draft$pos    <- str_sub(draft$s1,
                        start = str_locate(draft$s1,' ')[,1],
                        end   = str_locate(draft$s1,' ')[,1] + 2)
draft$s2 <- sapply(str_split(draft$s1,' '),'[',4)
draft <- mutate(draft, keeper = ifelse(s2 !='K' | is.na(s2),0,1))
draft$value <- as.numeric(str_replace(str_sub(draft$V2, start = str_locate(draft$V2,'\\$')[,1]),
                                    '\\$',
                                    '')
)

## Coaches and defenses in 2013 and 2014 don't read so nicely, so some more work needs to be done ##
null_value <- which(is.na(draft$value))

draft$pick[null_value] <- sapply(str_split(draft$V1[null_value], '\t'), '[[', 1)
draft$n2[null_value] <- sapply(str_split(draft$V1[null_value], '\t'), '[[', 2)
draft$pos[null_value] <- sapply(str_split(draft$n2[null_value],' '),'[[',3)
draft$player[null_value] <- paste(sapply(str_split(draft$n2[null_value],' '),'[[',1),
                                  sapply(str_split(draft$n2[null_value],' '),'[[',2),
                                  sep = " ")
draft$value[null_value] <- as.numeric(
                           str_replace(
                               sapply(str_split(draft$V1[null_value], '\t'), '[[', 3),
                                '\\$',
                                '')
                            )
draft <- select(draft,year,pick,player,team,pos,value,keeper)


write.csv(draft,"draft_summary.csv")

