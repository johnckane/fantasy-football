library(httr)
library(dplyr)
library(XML)
library(RCurl)


### ESPN ###
url_base <- "http://games.espn.go.com/ffl/tools/projections?&startIndex="
url_end <- "&leagueId=725667"

data_list <- list()

for(i in c(0,40,80,120,160,200,240,280,320)) {
    url <- paste(url_base,i,url_end,sep="")
    data_list <- c(data_list,readHTMLTable(url)$playertable_0)             
}

### cbssports.com ###
cbs_list <- list()

cbs_url_qb <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/QB/season?&print_rows=9999"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_qb)[[4]])

cbs_url_rb <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/season/avg/standard?&print_rows=9999"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_rb)[[4]]) 

cbs_url_wr <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/WR/season/avg/standard?&print_rows=9999"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_wr)[[4]])

cbs_url_te <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/TE/season/avg/standard?&print_rows=9999"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_te)[[4]])

cbs_url_k <- "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/K/season/avg/standard"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_k)[[4]])

cbs_url_dst <-"http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/DST/season/avg/standard"
cbs_list <- c(cbs_list,readHTMLTable(cbs_url_dst)[[4]])

### yahoo ###

yahoo_list <- list()

yahoo_url_qb <- "http://www.fantasypros.com/nfl/projections/qb.php"
yahoo_url_rb <- "http://www.fantasypros.com/nfl/projections/rb.php"
yahoo_url_wr <- "http://www.fantasypros.com/nfl/projections/wr.php"
yahoo_url_te <- "http://www.fantasypros.com/nfl/projections/te.php"
yahoo_url_k <- "http://www.fantasypros.com/nfl/projections/k.php"
yahoo_url_dst <- "http://www.fantasypros.com/nfl/projections/dst.php"

yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_qb)$data)
yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_rb)$data)
yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_wr)$data)
yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_te)$data)
yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_k)$data)
yahoo_list <- c(yahoo_list,readHTMLTable(yahoo_url_dst)$data)
