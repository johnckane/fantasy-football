library(tidyverse)
kane <- read_csv("/home/john/projects/fantasy-football/data/FFA/2020/ffa_customrankings2020-0.csv")
library(tidyverse)
library(magrittr)

head(kane)
colnames(kane)

## Output dataset needs:
# 1. player_team
# 2. position
# 3. age
# 4. BYE
# 5. kane__adp
# 6. kane_points
# 7. catalina_adp
# 8. catalina_points
# 9. chicago_adp
#10. chicago_points




kane2 <-
  kane %>%
  mutate(player_team = paste0(player," - ",team),
         kane_adp = adp,
         kane_points = ptsGame) %>%
  select(player_team,position,age,bye,kane_adp,kane_points)

# catalina2 <-
#   catalina %>%
#   mutate(player_team = paste0(player," - ",team),
#          catalina_adp = adp,
#          catalina_points = ptsGame) %>%
#   select(player_team,position,age,bye,catalina_adp,catalina_points)
# 
# chicago2 <-
#   chicago %>%  
#   mutate(player_team = paste0(player," - ",team),
#          chicago_adp = adp,
#          chicago_points = ptsGame) %>%
#   select(player_team,position,age,bye,chicago_adp,chicago_points)
#   

snake_app_data <-
  kane2 %>%
  # full_join(.,
  #           catalina2,
  #           by = c("player_team","position","age","bye")) %>%
  # full_join(.,
  #           chicago2,
  #           by = c("player_team","position","age","bye")) %>%
  filter(position %in% c('DST','QB','RB','WR','TE','K','DST'))

with(snake_app_data,table(position))

snake_app_data_top300 <-
snake_app_data %>%
  arrange(kane_adp) %>%
  slice(1:300)

# 300 should be ok

write_csv(snake_app_data_top300,
          path = "/home/john/projects/fantasy-football/data/FFA/2020/snake_app_data.csv")