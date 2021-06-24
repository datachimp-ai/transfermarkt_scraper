require(tidyverse)
require(shiny)
require(rvest)
require(tidyverse)
require(stringi)
require(stringr)
require(tidyr)
require(data.table)
require(RSQLite)
require(rlist)

getCompetitions <- function(competitions_df) {
  #leagues
  leagues <- competitions_df %>% filter(type == "League")
  if(nrow(leagues) > 0) {
  leagues_html <- leagues$competition_url %>% lapply(read_html)
  league_country_tier <- leagues_html %>% lapply(html_nodes, xpath = '//*[@id="wettbewerb_head"]/div/div/div[2]/div[2]/div[1]/table') %>% 
                         lapply(html_table) %>% sapply(., "[", 1) %>% sapply(., "[[", 1, 2) %>% sapply(., str_split, coll(" -  "))
  
  leagues_df <- data.frame(competition_id      = str_split(leagues$competition_url, "/") %>% sapply(., "[[", 7)
                          ,competition_country = str_split(league_country_tier, "-", simplify = TRUE) %>% .[,2] %>% str_trim()
                          ,competition_name    = leagues_html %>% lapply(html_node, xpath = '//*[@id="wettbewerb_head"]/div/div/div[1]/h1') %>%
                                                 lapply(html_text) %>% unlist()
                          ,competition_type    = leagues$type
                          ,league_tier         = str_split(league_country_tier, "-", simplify = TRUE) %>% .[,1] %>% str_trim()
                          ,competition_url     = leagues$competition_url
                          ,current_season_url  = leagues_html %>% lapply(html_node, xpath = '//*[@id="spieltagsbox"]/table/tr/td/div[2]/a') %>% 
                                                 lapply(html_attr, "href") %>% paste(home_page, ., sep = "")
                          )
  }
  #cups
  cups <- filter(competitions_df, type == "Cup")
  if(nrow(cups) > 0) {
  cups_html <- cups$competition_url %>% lapply(read_html)
  
  cups_df <- data.frame(competition_id      = str_split(cups$competition_url, "/") %>% sapply(., "[[", 7)
                       ,competition_country = cups_html %>% lapply(html_nodes, xpath = '//*[@id="wettbewerb_head"]/div/div/div[2]/div/div[1]/p[1]/span[2]/img') %>% 
                                              lapply(html_attr, "title") %>% unlist()
                       ,competition_name    = cups_html %>% lapply(html_nodes, xpath = '//*[@id="wettbewerb_head"]/div/div/div[1]/div/div[1]/h1/b') %>%
                                              lapply(html_text) %>% unlist()
                       ,competition_type    = cups$type
                       ,league_tier         = ""
                       ,competition_url     = cups$competition_url
                       ,current_season_url  = cups_html %>% lapply(html_node,xpath = '//*[@id="spieltagsbox"]/div/table/tbody/tr/td/div/a') %>% 
                                              lapply(html_attr, "href") %>% paste(home_page, ., sep = "")
                       )
  }
  #international_cups
  international_cups <- competitions_df %>% filter(type == "International Cup")
  if(nrow(international_cups) > 0) {
  international_cups_html <- international_cups$competition_url %>% lapply(read_html)
  international_cups_df <- data.frame(competition_id      = str_split(international_cups$competition_url, "/") %>% sapply(., "[[", 7)
                                     ,competition_country = ""
                                     ,competition_name    = international_cups_html %>% lapply(html_nodes, xpath = '//*[@id="wettbewerb_head"]/div/div/div[1]/div/div[1]/h1/b') 
                                                            %>% lapply(html_text) %>% unlist()
                                     ,competition_type    = international_cups$type
                                     ,league_tier         =  ""
                                     ,competition_url     = international_cups$competition_url
                                     ,current_season_url  = international_cups_html %>% lapply(html_node,xpath = '//*[@id="spieltagsbox"]/div/table/tbody/tr/td/div/a') %>% 
                                                            lapply(html_attr, "href") %>% paste(home_page, ., sep = "")
                                     ) 
  }
  competitions <- rbind(if(exists("leagues_df")) leagues_df, if(exists("cups_df")) cups_df, if(exists("international_cups_df")) international_cups_df)
  competitions <- competitions %>% filter(current_season_url != "https://www.transfermarkt.comNA")
  return(competitions)
}

getMatches <- function (competitions) {
  print("finding match pages")
  season_html <- competitions$current_season_url %>% lapply(read_html)
  match_id_list <- season_html %>% lapply(html_nodes, ".ergebnis-link") %>% lapply(html_attr, "id")
  match_id_unlisted <- match_id_list %>% unlist() %>% data.frame(match_id = .)
  season <- season_html %>% lapply(html_nodes, xpath = "//option[@selected]") %>% lapply(html_text) %>% sapply("[[", 1) %>% rep(sapply(match_id_list, length))
  competition_id <- competitions$competition_id %>% rep(sapply(match_id_list, length)) %>% .[!is.na(match_id_unlisted)]
  match_ids <- match_id_unlisted[!is.na(match_id_unlisted)]
  match_urls <- season_html %>% lapply(html_nodes, ".ergebnis-link") %>% lapply(html_attr, "href") %>% unlist() %>% 
                lapply(stri_trans_general, id = "Latin-ASCII") %>% paste(home_page, ., sep = "") %>% .[!is.na(match_id_unlisted)]
  filtered_matches <- data.frame(match_id = match_id_unlisted, competition_id = competition_id, match_url = match_urls, season = season) %>% 
                      anti_join(existing_matches, by = "match_id")
  counter <- 1
  total <- length(filtered_matches$match_url)
  print("scraping match pages")
  match_html <- lapply(filtered_matches$match_url, function(x) {
    html <- NULL
    attempt <- 0
    while(is.null(html) && attempt <= 3 ) {
      attempt <- attempt + 1
      html <- tryCatch(read_html(x), error = function(x) {return(NULL)})
      print(paste0(counter, "/", total, " attempt: ", attempt))
    }
    counter <<- counter + 1
    return(html)
  }
  )
  stats_urls <- match_html %>% lapply(html_nodes, "#statistics > a") %>% lapply(html_attr, "href") %>% 
                lapply(stri_trans_general, id = "Latin-ASCII")
  stats_url_exists <- stats_urls %>% lapply(length) %>% sapply("[[", 1) 
  stats_url <- if_else(stats_url_exists == 0, "http://www.google.com", paste(home_page, stats_urls, sep = ""))
  counter <- 1
  total <- length(stats_url)
  print("scraping stats pages")
  stats_html <- lapply(stats_url, function(x) {
    html <- NULL
    attempt <- 0
    while(is.null(html) && attempt <= 3 ) {
      attempt <- attempt + 1
      html <- tryCatch(read_html(x), error = function(x) {return(NULL)})
      print(paste0(counter, "/", total, " attempt: ", attempt))
    }
    counter <<- counter + 1
    return(html)
  }
  )
  matches <- list(match_id = filtered_matches$match_id
                  ,competition_id = filtered_matches$competition_id
                  ,match_url = filtered_matches$match_url
                  ,season = filtered_matches$season
                  ,match_html = match_html
                  ,stats_url = stats_url
                  ,stats_html = stats_html
                  )
  return(matches)
}

getMatchOverview <- function(matches) {
  scores <- matches$match_html %>% lapply(html_nodes, "div.sb-spieldaten > div > div > div") %>% lapply(html_text) %>%
            sapply("[[", 1) %>% sapply(str_squish) %>% unname() %>% lapply(str_split, coll("("), simplify = TRUE) %>% 
            lapply(str_remove, coll(")"))
  ft_score <- scores %>% sapply("[", 1) %>% unlist()
  ht_score <- scores %>% sapply("[", 2) %>% unlist()
  match_overview <- data.frame(match_id           = matches$match_id 
                              ,competition_id     = matches$competition_id 
                              ,season             = matches$season 
                              ,match_url          = matches$match_url
                              ,matchday           = matches$match_html %>% lapply(html_nodes, "p.sb-datum.hide-for-small > a:nth-child(1)") %>% 
                                                    lapply(html_text) %>% {ifelse(lapply(., length)==0, "", .)} %>% str_extract(".") 
                              ,date               = matches$match_html %>% lapply(html_nodes, "p.sb-datum.hide-for-small > a") %>%
                                                    lapply(html_text) %>% lapply(tail, n = 1) %>% {ifelse(lapply(., length)==0, "", .)} %>% unlist() %>% str_squish()
                              ,time               = matches$match_html %>% lapply(html_nodes, "p.sb-datum.hide-for-small") %>% lapply(html_text) %>% 
                                                    {ifelse(lapply(., length)==0, "", .)} %>% str_squish() %>% str_split(coll("|"), simplify = TRUE) %>% .[,3]
                              ,home               = matches$match_html %>% lapply(html_nodes, "div.sb-team.sb-heim .sb-vereinslink") %>% 
                                                    {ifelse(lapply(., length)==0, "", .)} %>% lapply(html_text) %>% unlist()
                              ,away               = matches$match_html %>% lapply(html_nodes, "div.sb-team.sb-gast .sb-vereinslink") %>%
                                                    {ifelse(lapply(., length)==0, "", .)} %>% lapply(html_text) %>% unlist()
                              ,home_ko_league_pos = matches$match_html %>% lapply(html_nodes, ".sb-team.sb-heim p") %>% lapply(html_text) %>%
                                                    {ifelse(lapply(., length)==0, "", .)}%>% unlist()
                              ,away_ko_league_pos = matches$match_html %>% lapply(html_nodes, ".sb-team.sb-gast p") %>% lapply(html_text) %>% 
                                                    {ifelse(lapply(., length)==0, "", .)} %>% unlist()
                              ,venue              = matches$match_html %>% lapply(html_nodes, "p.sb-zusatzinfos span a") %>% lapply(html_text) 
                                                    %>% {ifelse(lapply(., length)==0, "", .)} %>% unlist()
                              ,referee            = matches$match_html %>% lapply(html_nodes, "div.sb-spieldaten > p.sb-zusatzinfos > a") %>% 
                                                    lapply(html_text) %>% {ifelse(lapply(., length)==0, "", .)}%>% unlist() 
                              ,ft_score           = ft_score
                              ,ft_h_goals         = str_split(ft_score, ":", simplify = TRUE)[,1]
                              ,ft_a_goals         = str_split(ft_score, ":", simplify = TRUE)[,2]
                              ,ht_score           = ht_score
                              ,ht_h_goals         = str_split(ht_score, ":", simplify = TRUE)[,1]
                              ,ht_a_goals         = str_split(ht_score, ":", simplify = TRUE)[,2]
                              )
  possession_str <-  matches$stats_html %>% lapply(html_nodes, "script:nth-child(9)") %>% lapply(html_text)
  h <- stri_locate_last(possession_str, coll = ",'y':") %>% .[,2]+1
  pos_h <- as.integer(ifelse(is.na(h), NA, substr(possession_str, h, h+1)))
  pos_a <- ifelse(is.na(h), NA, 100-pos_h)
  pos <- data.frame(match_id = matches$match_id
                   ,stat     = "posession"
                   ,home     = pos_h
                   ,away     = pos_a
                   )
  stats_class <- matches$stats_html %>% lapply(html_nodes,".sb-sprite") %>% lapply(html_attr, "class")
  match_id <- matches$match_id %>% rep(sapply(stats_class, length))
  stats_base <- stats_class %>% unlist() %>% data.frame(match_id = match_id, class = .) %>%  
                mutate(stat = case_when(class == "sb-sprite sb-stat-schuss"     ~ "shots",
                                       class == "sb-sprite sb-stat-nebenschuss" ~ "shots_off",
                                       class == "sb-sprite sb-stat-gehalten"    ~ "saves",
                                       class == "sb-sprite sb-stat-ecke"        ~ "corners",
                                       class == "sb-sprite sb-stat-freistoss"   ~ "freekicks",
                                       class == "sb-sprite sb-stat-foul"        ~ "fouls",
                                       class == "sb-sprite sb-stat-abseits"     ~ "offsides"
                                       )
                      ) 
  home_stats <- matches$stats_html %>% lapply(html_nodes, ".sb-statistik-heim .sb-statistik-zahl") %>%
                lapply(html_text) %>% unlist()
  away_stats <- matches$stats_html %>% lapply(html_nodes, ".sb-statistik-gast .sb-statistik-zahl") %>%
                lapply(html_text) %>% unlist()
  stats <- stats_base %>% mutate(home = home_stats, away = away_stats) %>% select(-class) 
  stats_final <- rbind(stats, pos) %>% filter(stat != "NA") %>% pivot_wider(id_cols = match_id, names_from = stat, values_from = c(home, away))
  match_overview_final <- left_join(match_overview, stats_final, by = "match_id")
  return(match_overview_final)
}

getMatchEvents <- function(matches) {
remove <- c("background-position: ", "-", "px", ";")
#goals
goal_times <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-tore"]/ul/li/div/div[1]/span') %>% lapply(html_attr, "style") %>%
              unlist %>% str_remove_all(paste(remove, collapse = "|")) %>% str_split(" ")
goal_nodes <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-tore"]/ul/li/div/div[4]//a[1]')
goals <- data.frame(match_id = rep(matches$match_id, sapply(goal_nodes, length))
                   ,home_away = matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-tore"]/ul/li') %>% lapply(html_attr, "class") %>% unlist() %>%
                                {. == "sb-aktion-heim"} %>% ifelse("Home", "Away")
                   ,time = as.integer(paste0(goal_times %>% sapply("[[", 2) %>% as.integer(.)/36, "0"))+(goal_times %>% sapply("[[", 1) %>% as.numeric(.)/36+1)
                   ,event = "Goal"
                   ,player = lapply(goal_nodes, html_text) %>% unlist()
                   ,details = lapply(goal_nodes, html_node, xpath = 'following-sibling::text()') %>% lapply(html_text) %>% unlist() %>% str_remove(", ")
                   )
#cards
card_nodes <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-karten"]/ul/li/div/div[4]//a[1]')
cards_times <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-karten"]/ul/li/div/div[1]/span') %>% lapply(html_attr, "style") %>%
               unlist %>% str_remove_all(paste(remove, collapse = "|")) %>% str_split(" ")
cards <- data.frame(match_id = rep(matches$match_id, sapply(card_nodes, length))
                   ,home_away = matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-karten"]/ul/li') %>% lapply(html_attr, "class") %>% unlist() %>%
                                {. == "sb-aktion-heim"} %>% ifelse("Home", "Away")
                   ,time = as.integer(paste0(cards_times %>% sapply("[[", 2) %>% as.integer(.)/36, "0"))+(cards_times %>% sapply("[[", 1) %>% as.numeric(.)/36+1)
                   ,event = matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-karten"]/ul/li/div/div[2]/span') %>% lapply(html_attr, "class") %>% 
                            unlist() %>% {case_when(. == "sb-sprite sb-gelb" ~ "Yellow Card"
                                                   ,. == "sb-sprite sb-gelbrot" ~ "Red Card - 2nd Yellow"
                                                   ,. == "sb-sprite sb-rot" ~ "Red Card"
                                                   )}
                   ,player = lapply(card_nodes, html_text) %>% unlist()
                   ,details = lapply(card_nodes, html_node, xpath = 'following-sibling::text()') %>% lapply(html_text) %>% unlist() %>% str_squish() %>% str_replace(" , ", ", ")
                   )
#missed_penalties
missed_penalty_nodes <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-verschossene"]/ul/li/div/div[4]/span[1]')
missed_penalty_times <- matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-verschossene"]/ul/li/div/div[1]/span') %>% lapply(html_attr, "style") %>%
                        unlist() %>% str_remove_all(paste(remove, collapse = "|")) %>% str_split(" ")
missed_penalties <- data.frame(match_id = rep(matches$match_id, sapply(missed_penalty_nodes, length))
                              ,home_away = matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-verschossene"]/ul/li') %>% lapply(html_attr, "class") %>% unlist() %>%
                                           {. == "sb-aktion-heim"} %>% ifelse("Home", "Away")
                              ,time = as.integer(paste0(missed_penalty_times %>% sapply("[[", 2) %>% as.integer(.)/36, "0"))+(missed_penalty_times %>% sapply("[[", 1) %>% as.numeric(.)/36+1)
                              ,event = "Missed Penalty"
                              ,player = lapply(missed_penalty_nodes, html_text) %>% unlist() %>% str_squish() %>% str_split(", ", simplify = TRUE) %>% .[,1]
                              ,details = matches$match_html %>% lapply(html_nodes, xpath = '//*[@id="sb-verschossene"]/ul/li/div/div[4]/span[2]') %>% 
                                         lapply(html_text) %>% unlist() %>% str_squish() 
                              )
match_events <- rbind(if(exists("goals")) goals, if(exists("cards")) cards, if(exists("missed_penalties")) missed_penalties) %>% 
                group_by(match_id) %>% arrange(match_id, time) %>% mutate(id = paste(match_id, row_number(), sep = "")) %>% ungroup()
}

home_page <- "https://www.transfermarkt.com"
competitions_df <- read_csv("competitions.csv")
competitions <- getCompetitions(competitions_df)
matches <- getMatches(competitions)
match_overview <- getMatchOverview(matches)
match_events <- getMatchEvents(matches)

competitions <- competitions %>% lapply(iconv, to = "UTF-8") %>% as.data.frame()
match_overview <- match_overview %>% lapply(iconv, to = "UTF-8") %>% as.data.frame()
match_events <- match_events %>% lapply(iconv, to = "UTF-8") %>% as.data.frame()
