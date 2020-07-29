# Recreate the historic draft data, adjust keeper values
library(readr)
library(stringr)
library(dplyr)
library(magrittr)
library(RMySQL)
path <- "/home/john/projects/fantasy-football/data/bn-draft-data/"

data_list <- list()

for(i in 1:10){
  data_list[[i]] <- read_table(paste0(path,"Draft_",2009+i),col_names = FALSE)
  data_list[[i]]$year <- 2009+i
  if(i <= 6){
    data_list[[i]]$owner <- rep(c("Hartman","Kane","Harrington","Higdon","Regan","Shokunbi","McShane","Thieneman","Matovina","Ready","Skrzyszewski","Olson"), each = 16) 
  }
  if(i == 7){
    data_list[[i]]$owner <- rep(c("Hartman","Kane","Harrington","Higdon","Regan","Harris","McShane","Thieneman","Matovina","Ready","Skrzyszewski","Olson"), each = 16)
  }
  if(i >= 8){
    data_list[[i]]$owner <- rep(c("Hartman","Kane","Harrington","Higdon","Regan","Harris","McShane","Thieneman","Matovina","Shokunbi","Skrzyszewski","Olson"), each = 16)
  }
  
}

# Treat 2009 to 2018 differently
draft_data <- do.call(what = rbind, data_list[1:9])
9*192

#' Time to Parse
#' We'll get these columns:
#' year, owner, pick, player, team, position, value, keeper
#' 
draft_data$pick   <- as.numeric(sapply(str_split(draft_data$X1,'\t'), '[[', 1))
draft_data$player_team_pos <- sapply(str_split(draft_data$X1,'\t'), '[[', 2)
draft_data$value <- sapply(str_split(draft_data$X1,'\t'), '[[',3)
draft_data$value2 <- as.numeric(str_replace(draft_data$value,'\\$',''))
draft_data$player <- sapply(str_split(draft_data$player_team_pos,','),'[[',1)
comma_break <- str_locate(draft_data$player_team_pos,',')[,1]
split <- ifelse(is.na(comma_break),
                sapply(str_locate_all(draft_data$player_team_pos,' '),'[[',2),
                comma_break)
draft_data$team_pos_keeper <- (str_sub(draft_data$player_team_pos,start = split + 1))
draft_data$split_vars <- str_split(draft_data$team_pos_keeper,' ')
draft_data$team <- sapply(str_split(draft_data$team_pos_keeper,' '),'[[',1)
draft_data$team <- sapply(draft_data$split_vars,'[',2)
draft_data$pos <- ifelse(sapply(draft_data$split_vars,'[',1) %in% c("HC","D/ST"),
                         sapply(draft_data$split_vars,'[',1),
                         sapply(draft_data$split_vars,'[',3))
draft_data$keeper <- ifelse(sapply(draft_data$split_vars,'[',5) == 'K',1,0)
draft_data <- draft_data %>%
  select(year,owner,pick,player,team,pos,value2,keeper) %>%
  mutate(keeper = ifelse(is.na(keeper),0,keeper))

draft_data %<>% arrange(player,year)

draft_data$team <- toupper(draft_data$team)



## Draft 2019
#' year, owner, pick, player, team, position, value, keeper
draft2019 <- data_list[[10]]
draft2019$pick <- as.numeric(sapply(str_split(draft2019$X1,' '),'[[',1))
draft2019$player_team <- 
  str_remove(
    str_extract(
        draft2019$X1,
        '[A-Z].*,'
    ),
    ',')
draft2019$team <- 
  str_extract(
    draft2019$player_team,
    '\\D{2,3}$'
  )

draft2019$player <-
  sapply(
    str_split(draft2019$player_team, 
              ' \\D{2,3}$'),
    '[[',
  1)

draft2019$position_value_keeper <-
  sapply(
    str_split(draft2019$X1,','),
    '[[',
    2
  )

draft2019$position <- 
  str_extract(draft2019$position_value_keeper,'^ [A-Z//]+ ') %>%
  str_remove_all(.," ")
with(draft2019,table(position))
draft2019$value <- 
  str_remove(
    str_extract(draft2019$position_value_keeper,'\\$\\d{1,3}'),
    '\\$'
  ) %>%
  as.numeric()


draft2019$keeper <- 
  ifelse(
    grepl(
      pattern ='^ [A-Z//]+ K',
      x = draft2019$position_value_keeper),
  1,
  0)

with(draft2019,table(keeper))  

draft2019$team <- toupper(draft2019$team)

draft2019 <-
  draft2019 %>%
  select(year,owner,pick,player,team,position,value,keeper)

draft_data <- bind_rows(draft_data,draft2019)

######################################################
 # start here
#######################################################

# Fix keeper values...
# remove all asterisks
draft_data$player <- str_replace(draft_data$player, "[*]","")

# Change Le'Veon Bell
draft_data$player <- ifelse(draft_data$player == "Le Veon Bell", "Le'Veon Bell",draft_data$player)

# Make Steve Smith Steve Smith Sr.
draft_data$player <- ifelse(draft_data$player == "Steve Smith" & draft_data$team %in% c("BAL","CAR"),
                            "Steve Smith Sr.",
                            draft_data$player)

draft_data$player <- ifelse(draft_data$pos == "HC",str_replace(draft_data$player,"HC",""),draft_data$player)
draft_data$player <- str_trim(draft_data$player)

#' Look at all D/ST, see if we need to change those names
#' 
draft_data %>% filter(pos == "D/ST")

draft_data$first_name <-  sapply(str_split(draft_data$player,' '), '[[', 1)      
draft_data$last_name <- sapply(str_split(draft_data$player, ' '), '[[', 2)
head(draft_data)

draft_data <- draft_data %>%
  mutate(player = ifelse(pos == "D/ST",
                         paste(first_name,"D/ST",sep = " "),
                         player))


draft_data$player <- str_trim(draft_data$player)

draft_data$player <- ifelse(draft_data$player == "Steve Smith" & draft_data$team %in% c("BAL","CAR"),
                            "Steve Smith Sr.",
                            draft_data$player)

draft_data_players <- draft_data %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

draft_data <- draft_data %>%
  arrange(player,year) %>%
  group_by(player) %>%
  mutate(player_obs = row_number())


## Easiest way to do this is in a loop I think
draft_data$adj_value <- draft_data$value2
for(i in 1:dim(draft_data)[1]){
  if(draft_data$keeper[i] == 1){
    if(draft_data$player_obs[i] != 1){
      if(draft_data$year[i] == (draft_data$year[i-1] + 1)){
        draft_data$adj_value[i] <- draft_data$adj_value[i-1] + 7
      }
    }
  }
}


draft_data %>% filter(value2 != adj_value)

## Combine with Armchair Analysis Code 

#' Need:
#' ) Unique players in the draft data
#' 2) Their player_key

draft_data_players <- draft_data %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)


## Connect to the database
pool <- dbPool(drv = RMySQL::MySQL(), 
               dbname = "armchair_analysis", 
               host = "localhost", 
               username = "john", 
               port = 3306, 
               unix.sock = "/var/run/mysqld/mysqld.sock")
players <- dbGetQuery(con = pool,
                      "SELECT player, fname, lname, pos1
                       FROM player")
duplicate_entries <- 
  draft_data_players %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player.x) %>%
  summarise(count = n()) %>%
  filter(count > 1)

single_entries <- 
  draft_data_players %>%
  anti_join(.,duplicate_entries, by = c("player" = "player.x"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 

draft_data_w_code <-
  draft_data %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player.x,player.y), by = c("player" = "player.x")) %>%
  rename(player_code = player.y)

draft_data_w_code %<>%
  mutate(player_code = ifelse(pos %in% c("HC","D/ST"), player,player_code))

uncoded <- 
  draft_data_w_code %>%
  filter(is.na(player_code == TRUE))

uncoded %<>%
  mutate(player_code = ifelse(player == 'Adrian Peterson', 'AP-0700',
                              ifelse(player == 'Alex Smith','AS-1600',
                                     ifelse(player == 'Antonio Brown','AB-3500',
                                            ifelse(player == 'Beanie Wells','CW-1400',
                                                   ifelse(player == 'Brandon Marshall','BM-0300',
                                                          ifelse(player == 'Cam Newton','CN-0500',
                                                                 ifelse(player == 'Charles Johnson','CJ-1450',
                                                                        ifelse(player == 'Chris Givens', 'CG-2060', player_code ))))))))) %>%
  mutate(player_code = ifelse(player == "Chris Johnson",'CJ-1700',
                              ifelse(player == 'David Johnson','DJ-1325',
                                     ifelse(player == 'Derek Dimke',NA,
                                            ifelse(player == 'EJ Manuel','EM-0250',
                                                   ifelse(player == 'James Jones','JJ-4200',
                                                          ifelse(player == 'Jonathan Stewart','JS-6700',
                                                                 ifelse(player == 'Kevin Smith','KS-1700',
                                                                        ifelse(player %in% c('Marvin Jones','Marvin Jones Jr.'), 'MJ-2250',
                                                                               ifelse(player == 'Mike Sims-Walker', 'MW-0400', player_code)))))))))) %>%
  mutate(player_code = ifelse(player == 'Mike Williams' & team == 'SEA','MW-2700',
                              ifelse(player == 'Mike Williams' & team %in% c('BUF','TB'), 'MW-2800',
                                     ifelse(player == 'Odell Beckham Jr.','OB-0075',
                                            ifelse(player == 'Ricky Williams','RW-2600',
                                                   ifelse(player == 'Royce Adams',NA,
                                                          ifelse(player == 'Roy Williams','RW-3200',
                                                                 ifelse(player == 'Ryan Grant','RG-1600',
                                                                        ifelse(player == 'Steve Smith Sr.','SS-2100',
                                                                               ifelse(player == 'Steve Smith', 'SS-2200', player_code )))))))))) %>%
  mutate(player_code = ifelse(player == 'T.Y. Hilton','TH-1850',
                              ifelse(player == 'Zach Miller','ZM-0200',player_code))) %>%
  mutate(player_code = ifelse(player == 'Kevin White' & year %in% c(2016,2017), 'KW-0887',
                              ifelse(player == 'Matt Jones' & year == 2016,'MJ-2275',
                                     ifelse(player == 'Michael Thomas','MT-0875',
                                            ifelse(player == 'Michael Vick','MV-0300',
                                                   ifelse(player == 'Chad Ochocinco','CJ-1200',
                                                                 ifelse(player == 'Wil Lutz','WL-0300',
                                                                        ifelse(player == 'Chris Thompson','CT-1220',player_code)))))))) %>%
  mutate(player_code = ifelse(player == 'Mike Thomas','MT-0900',
                              ifelse(player == 'Steven Hauschka','SH-0400',
                                     ifelse(player == 'JuJu Smith-Schuster','JS-4750',
                                            ifelse(player == 'DJ Moore','DM-2850',
                                                   ifelse(player == 'Josh Allen','JA-1087',player_code))))))


uncoded %>% filter(is.na(player_code) == TRUE)

# Cannot match Derek Dimke or Royce Adams

dbGetQuery(conn = pool,
           statement = "select * from player where lname = 'Thomas' and fname = 'Michael'")
dbGetQuery(conn = pool,
           statement = "select * from player where lname = 'Adams' and pos1 = 'WR' and start <= 2012")
dbGetQuery(conn = pool,
           statement = "select * from player where fname = 'Chad' and (lname = 'Johnson' or lname = 'Ochocinco')")
dbGetQuery(conn = pool,
           statement = "select * from player where fname = 'Chris' and lname = 'Thompson'")
dbGetQuery(conn = pool,
           statement = "SELECT * FROM player WHERE lname = 'Hauschka'")
dbGetQuery(conn = pool,
           statement = "SELECT * FROM player WHERE lname = 'Smith-Schuster'")
dbGetQuery(conn = pool,
           statement = "SELECT * FROM player WHERE lname = 'Moore'")
dbGetQuery(conn = pool,
           statement = "SELECT * FROM player WHERE lname = 'Allen'")



coded_draft_data <- 
  bind_rows(draft_data_w_code %>% filter(is.na(player_code) == FALSE),
            uncoded)

lapply(coded_draft_data, function(x) sum(is.na(x)))

coded_draft_data %>%
  filter(is.na(team) == TRUE) %>%
  print(n = 90)




team_name_abbr_lookup <- data.frame(abbr = c('ARI','ATL','BAL','BUF','CAR','CHI','CIN','CLE','DAL','DEN',
                                             'DET','GB' ,'HOU','IND','JAC','KC' ,'LA' ,'MIA','MIN','NE',
                                             'NO' ,'NYG','NYJ','OAK','PHI','PIT','SD' ,'SEA','SF' , 'STL',
                                             'TB','TEN','WAS'),
                                    name = c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns',
                                             'Cowboys','Broncos','Lions','Packers','Texans','Colts','Jaguars','Chiefs','Rams',
                                             'Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles',
                                             'Steelers','Chargers','Seahawks','49ers','Rams','Buccaneers','Titans','Redskins'),
                                    stringsAsFactors = FALSE)



coded_draft_data %<>%
  left_join(team_name_abbr_lookup, by = c("first_name" = "name")) %>% 
  mutate(team = ifelse(is.na(team),abbr,team)) %>%
  ungroup()

## This included the Rams twice in four years. 
## The Rams have been in LA only since the 2016 season.

coded_draft_data %<>%
  filter((player == 'Rams D/ST' & year %in% c(2013,2014) & abbr == 'LA') == FALSE,
         (player == 'Rams D/ST' & year %in% c(2015,2016,2018) & abbr == 'STL') == FALSE,
         (player == 'Rams Coach' & year == 2018 & abbr == 'STL') == FALSE)

coded_draft_data %>% group_by(player,year) %>% summarise(count = n()) %>% filter(count > 1)

# Mike Williams being here twice is OK, the two different Mike Williams' are coded differently. 


coded_draft_data %<>% 
  mutate(team = ifelse(player %in% c('49ers Coach','49ers D/ST'),'SF',
                       ifelse(player %in% c('Bears Coach','Bears D/ST'),'CHI',
                              ifelse(player %in% c('Bengals Coach','Bengals D/ST'),'CIN',
                                     ifelse(player %in% c('Bills Coach,Bills D/ST'),'BUF',team)))))
colnames(coded_draft_data)

coded_draft_data %>% filter(is.na(player_code) == TRUE)


draft_data <- coded_draft_data[,c(1,2,3,4,13,5,6,8,12)]
head(draft_data)

draft_data %>% ungroup() %>% group_by(player,year) %>% summarise(count = n()) %>% filter(count > 1)

save(draft_data, file = "/home/john/projects/fantasy-football/data/bn-draft-data/draft_data.Rda")
