# Recreate the historic draft data, adjust keeper values


library(readr)

path <- "/home/john/stats_corner/fantasy_football/raw_data/"

data_list <- list()

for(i in 1:7){
  data_list[[i]] <- read_table(paste0(path,"Draft_",2009+i),col_names = FALSE)
  data_list[[i]]$year <- 2009+i
  if(i <= 6){
    data_list[[i]]$owner <- rep(c("Hartman","Kane","Harrington","Higdon","Regan","Shokunbi","McShane","Thieneman","Matovina","Ready","Skrzyszewski","Olson"), each = 16) 
  }
  else{
    data_list[[i]]$owner <- rep(c("Hartman","Kane","Harrington","Higdon","Regan","Harris","McShane","Thieneman","Matovina","Ready","Skrzyszewski","Olson"), each = 16)
  }
}

draft_data <- do.call(what = rbind, data_list)
7*192
str(draft_data)

library(stringr)
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
View(draft_data)

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
#' 1) Unique players in the draft data
#' 2) Their player_key

draft_data_players <- draft_data %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)


## Connect to the database
mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player, fname, lname, pos1
                       from player")
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

library(magrittr)

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
                                                                        ifelse(player == 'Marvin Jones','MJ-2250',
                                                                               ifelse(player == 'Mike Sims-Walker', 'MW-0400', player_code )))))))))) %>%
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
  mutate(player_code = ifelse(player == 'Kevin White' & year == 2016,'KW-0887',
                              ifelse(player == 'Matt Jones' & year == 2016,'MJ-2275',
                                     ifelse(player == 'Michael Thomas','MT-0875',player_code))))


uncoded %>% filter(is.na(player_code) == TRUE)

dbGetQuery(conn = mydb,
           statement = "select * from player where lname = 'Thomas' and fname = 'Michael'")

coded_draft_data %>% filter(is.na(player_code) == TRUE)

coded_draft_data <- 
  bind_rows(draft_data_w_code %>% filter(is.na(player_code) == FALSE),
            uncoded)

lapply(coded_draft_data, function(x) sum(is.na(x)))

coded_draft_data %>%
  filter(is.na(team) == TRUE) %>%
  print(n = 90)

table(coded_draft_data$team)


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


coded_draft_data %<>% 
  mutate(team = ifelse(player %in% c('49ers Coach','49ers D/ST'),'SF',
                       ifelse(player %in% c('Bears Coach','Bears D/ST'),'CHI',
                              ifelse(player %in% c('Bengals Coach','Bengals D/ST'),'CIN',
                                     ifelse(player %in% c('Bills Coach,Bills D/ST'),'BUF',team)))))
colnames(coded_draft_data)
draft_data <- coded_draft_data[,c(1,2,3,4,13,5,6,8,12)]
save(draft_data, file = "/home/john/stats_corner/fantasy_football/draft_data.Rda")
