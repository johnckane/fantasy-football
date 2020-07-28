# Match ffa with adp data players only
# So find the projected points for all the players in the adp2017 file

rm(adp2017_final)
load(file = "/home/john/stats_corner/2017/adp2017_final.Rda")

adp2017_final

colnames(adp2017_final)
colnames(ffa_no_dst_w_code)

colnames(ffa_no_dst)
#filter(ffa_no_dst_w_code, player == 'Antonio Brown') %>% View()
#filter(adp2017_final, player == 'Antonio Brown') %>% View()
ffa_no_dst_w_code %<>% mutate(pos = position)
ffa_no_dst_w_code %<>%
  mutate(player_code = ifelse(player == 'Alvin Kamara','Kamar01',
                       ifelse(player == 'Christian McCaffrey','McCaf01',
                       ifelse(player == 'C.J. Prosise','Prosi01',
                       ifelse(player == 'Cooper Kupp','KuppC01',
                       ifelse(player == 'Corey Davis','Davis01',
                       ifelse(player == 'Dalvin Cook','CookD01',
                       ifelse(player == 'Deshaun Watson','Watso01',
                       ifelse(player == 'DOnta Foreman','Freem01',
                       ifelse(player == 'Evan Engram','Engra01',
                       ifelse(player == 'Jamaal Williams','Willi01',
                       ifelse(player == 'James Conner','Conne01',
                       ifelse(player == 'Joe Mixon','Mixon01',
                       ifelse(player == 'Joe Williams','Willi02',
                       ifelse(player == 'John Ross','RossJ01',
                       ifelse(player == 'Kareem Hunt','HuntK01',
                       ifelse(player == 'Kenny Golladay','Golla01',
                       ifelse(player == 'Leonard Fournette','Fourn01',
                       ifelse(player == 'Marlon Mack','MackM01',
                       ifelse(player == 'O.J. Howard','Howar01',
                       ifelse(player == 'Samaje Perine','Perin01',
                       ifelse(player == 'Zay Jones','Jones01',
                       ifelse(player == 'Adrian Peterson' & team == 'NO','AP-0700',
                       ifelse(player == 'Alex Smith' & pos == 'QB','AS-1600',
                       ifelse(player == 'Antonio Brown','AB-3500',
                       ifelse(player == 'Brandon Marshall','BM-0300',
                       ifelse(player == 'Cam Newton','CN-0500',
                       ifelse(player == 'CJ Anderson','CA-0750',
                       ifelse(player == 'David Johnson','DJ-1325',
                       ifelse(player == 'Jonathan Stewart','JS-6700',
                       ifelse(player == 'Kevin White','KW-0887',
                       ifelse(player == 'LeVeon Bell','LB-0250',
                       ifelse(player == 'Marvin Jones','MJ-2250',
                       ifelse(player == 'Michael Thomas','MT-0875',
                       ifelse(player == 'Odell Beckham','OB-0075',
                       ifelse(player == 'T.Y. Hilton','TH-1850',
                       ifelse(player == 'Chris Brown', 'CB-3600',player_code
                       )))))))))))))))))))))))))))))))))))))

ffa_no_dst_w_code %>% filter(player == 'Odell Beckham')
ffa_no_dst_w_code %>% filter(grepl('Foreman',player) == T)

ffa_final <- bind_rows(ffa_no_dst_w_code,ffa_dst2)

ffa_final %>% filter(position == 'D/ST')

ffa_final$ptsGame <- ffa_final$points/16

str(ffa_final)
str(adp2017_final)


adp_w_proj <- adp2017_final2 %>% 
  left_join(.,
            ffa_final %>% ungroup() %>% select(player_code,ptsGame) %>% filter(is.na(player_code) == FALSE), 
            by = c("player_code"))
colnames(adp_w_proj)

adp_w_proj %>% filter(is.na(ptsGame)) %>% arrange(player)
adp_w_proj2017 <- adp_w_proj                                                             
save(adp_w_proj2017,file="/home/john/stats_corner/2017/predict-cost-model/adp_w_proj2017.Rda")                                                                          
