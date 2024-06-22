install.packages("nflfastR")
ids <- nflfastR::fast_scraper_schedules(2017:2019) %>%
  dplyr::filter(game_type == "SB") %>%
  dplyr::pull(game_id)
pbp <- nflfastR::build_nflfastR_pbp(ids)

head(pbp)

stats <- nflfastR::calculate_player_stats(pbp, weekly = FALSE)

rosters <- nflreadr::load_rosters(2019)


  seasons = most_recent_season(roster = TRUE),
  file_type = getOption("nflreadr.prefer", default = "rds")
)


install.packages("nflreadr")
