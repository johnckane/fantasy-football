nppr_data_file <- "/home/john/fantasy-football/data/FFA/ffa_customrankings2018-NPPR.csv"
ppr_data_file <- "/home/john/fantasy-football/data/FFA/ffa_customrankings2018-PPR.csv"

library(tidyverse)
library(magrittr)
nppr <- read_csv(nppr_data_file)
ppr <- read_csv(ppr_data_file)

## Output dataset needs:
# 1. player_team
# 2. position
# 3. age
# 4. BYE
# 5. standard_adp
# 6. standard_points
# 7. ppr_adp
# 8. ppr_points


colnames(nppr)

nppr %<>%
  mutate(player_team = paste0(player," - ",team),
         standard_adp = adp,
         standard_points = ptsGame) %>%
  select(player_team,position,age,bye,standard_adp,standard_points)

ppr %<>%
  mutate(player_team = paste0(player," - ",team),
         ppr_adp = adp,
         ppr_points = ptsGame) %>%
  select(player_team,position,age,bye,ppr_adp,ppr_points)

snake_app_data <- full_join(nppr,ppr,by = c("player_team","position","age","bye")) %>%
  filter(is.na(ppr_adp) == FALSE,
         is.na(standard_adp) == FALSE,
         position %in% c('DST','QB','RB','WR','TE','K')) %>%
  arrange(standard_adp) %>%
  slice(1:300)

write_csv(snake_app_data,
          "/home/john/fantasy-football/data/FFA/snake_app_data.csv")
