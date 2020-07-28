library(readr)
library(stringr)
setwd("~/Fantasy Football/Raw Data Files")

draft <- read.table("Draft_2015",sep="\t", header=FALSE,fill=TRUE, quote = "")
draft$owner <- rep(c("Hartman",
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
                      each = 16)
draft$year <- 2015
draft <- mutate(draft,value=as.numeric(str_replace(V3,"\\$","")),pick=V1,V2=str_replace_all(V2,"  "," "))

draft <- mutate(draft,player=str_sub(V2,end=str_locate(V2,',')[,1]-1))

# In 2013 coaches and defenses did not have commas after their name so the previous step
# resulted in a missing value, the following corrects that
missing_player <- c(which(is.na(draft$player)))

end_of_name_indices <- lapply(str_locate_all(draft$V2[missing_player]," "),'[[',2,1)

str(end_of_name_indices)


draft$player[missing_player] <- str_sub(draft$V2[missing_player],end=as.numeric(end_of_name_indices)-1)


# Put in positions, several values of 'V2' don't have commas separating them so we need to do
# this in pieces

no_split <- which(sapply(sapply(str_split(draft$V2,","),'[',2),is.na)==TRUE)

draft_no_split = draft[no_split,]
draft_w_split = draft[-no_split,]

draft_no_split$position <- str_trim(
                                str_sub(draft_no_split$V2,
                                start=as.numeric(lapply(str_locate_all(draft_no_split$V2," "),'[[',2,1)))
)
draft_no_split$team <- str_trim(
                            str_sub(draft_no_split$V2,
                            end = as.numeric(lapply(str_locate_all(draft_no_split$V2," "),'[[',1,1)))
)
s1 <- sapply(str_split(draft_w_split$V2,","),'[',2)

draft_w_split$position <- str_trim(sapply(sapply(s1,str_split," "),'[',3))
draft_w_split$team <- str_trim(sapply(sapply(s1,str_split," "),'[',2))
# Need a keeper flag: any player whose split has length > 4? 

draft_no_split$keeper <- 0

keeper1 <- sapply(sapply(s1,str_split," "),'[',4) 

draft_w_split$keeper <- ifelse(keeper1!="K"|is.na(keeper1),0,1)


draft <- rbind(draft_w_split,draft_no_split)

# Create final version, output
draft_df <- select(draft,owner,player,position,team,keeper)
write.csv(draft_df,"draft_df.csv",row.names=FALSE)
