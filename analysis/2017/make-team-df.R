team_name <- c('49ers','Bears','Bengals','Bills','Broncos','Browns','Buccaneers','Cardinals','Chargers','Chiefs','Colts','Cowboys', 
'Dolphins','Eagles','Falcons','Giants','Jaguars','Jets','Lions','Packers','Panthers','Patriots','Raiders','Rams','Ravens','Redskins', 
'Saints','Seahawks','Steelers','Texans','Titans','Vikings')

city <- c('San Francisco','Chicago','Cincinnati','Buffalo','Denver','Cleveland','Tampa Bay','Arizona','Los Angeles','Kansas City',
          'Indianapolis','Dallas','Miami','Philadelphia','Atlanta','New York','Jacksonville','New York','Detroit','Green Bay',
          'Carolina','New England','Oakland','Los Angeles','Baltimore','Washington','New Orleans','Seattle',
          'Pittsburgh','Houston','Tennessee','Minnesota')
abbr <- c('SF','CHI','CIN','BUF','DEN','CLE','TB','ARI','LAR','KC','IND','DAL','MIA','PHI','ATL','NYG','JAC','NYJ','DET','GB','CAR',
          'NE','OAK','LAC','BAL','WAS','NO','SEA','PIT','HOU','TEN','MIN')

team_df <- data.frame(team_name = team_name, city = city, abbr = abbr,stringsAsFactors =FALSE)

save(team_df,file="/home/john/stats_corner/fantasy_football/team_df.Rda")
