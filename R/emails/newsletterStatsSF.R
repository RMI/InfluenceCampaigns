library(tidyverse)
library(rvest)
library(httr)
library(rjson)
library(jsonlite)
library(dotenv)
library(dplyr)
library(stringr)
library(curl)
library(purrr)


#setwd('C:\\Users\\elizabeth.duchan\\Document\\GitHub\\InfluenceCampaigns\\R\\powerBI')

setwd("~/GitHub/InfluenceCampaignsNew/InfluenceCampaigns/R/powerBI")

load_dot_env('Renviron.env')

#' SF Authentication
sf_auth()
objects <- sf_list_objects()
head(objects)

# get all Marketing Cloud Salesforce newsletters

get_title <- function(url) {
  #GET(url, timeout(30))
  if (is.na(url)) {
    return(NA_character_)
  } else {
    webpage <- read_html(url)
    titles <- html_nodes(webpage, "title") %>% html_text()
    # Ensure that only the first title is returned if there are multiple
    title <- titles[1] %>% str_replace_all("- RMI", "") %>% str_trim()
    return(title)
  }
}


# Get total clicks
SF_getTotalClicks <- function () {
  
  my_soql <- sprintf("SELECT Id, et4ae5__EmailName__c, et4ae5__Subject__c, 
                    et4ae5__DateSent__c, CreatedDate, et4ae5__NumberofTotalClicks__c
                    
                    FROM et4ae5__SendDefinition__c
                    WHERE et4ae5__DateSent__c >= 2023-01-01T00:00:00Z")
  
  email_clicks <- sf_query(my_soql, "et4ae5__SendDefinition__c", api_type = "Bulk 1.0") %>% 
    rename(
      id = Id,
      name = et4ae5__EmailName__c,
      subject = et4ae5__Subject__c,
      sentAt = et4ae5__DateSent__c,
      totalClicks = et4ae5__NumberofTotalClicks__c,
      date = CreatedDate
    ) %>%
    select(id, name, subject, sentAt, totalClicks, date) %>%
    mutate(
      date = as.Date(date),
      sentAt = format(as.POSIXct(sentAt, tz = "America/Denver"), "%Y-%m-%dT%H:%M:%S-07:00"))
  
  return(email_clicks)
}


allSFEmailClicks <- SF_getTotalClicks()


# filter through list to find correct emails

allSparkSF <- allSFEmailClicks %>%
  filter(grepl('Spark', name) | grepl('Upcoming Events', name) | grepl('PEM', name) |
           grepl('Climate Resilience Update', name) | grepl('Scandinavia House', name) | grepl('Guide to Climate Week', name)) %>%
  filter(!grepl('Proof', subject) & !grepl('v2', name)) %>% 
  filter(totalClicks != '' & totalClicks > 0) %>% 
  filter(!grepl('Fred', name)) %>% 
  mutate(name = ifelse(name == 'NL Spark 2023-04-20', 'NL 2023-04-20 Spark', name),
         date = as.Date(str_extract(name, '[0-9]+-[0-9]+-[0-9]+')))

allMCSF <- allSFEmailClicks %>%
  filter(!grepl('Proof', subject) & grepl('^PEM', name) & grepl('Transparency Newsletter|Policy Newsletter|Finance Newsletter|Resilience Newsletter|Corporate Newsletter', name)) %>% 
  filter(totalClicks != '' & totalClicks > 0) %>% 
  mutate(date = as.Date(paste0(str_extract(name, '[0-9]+-[0-9]+'), '-01'), "%Y-%m-%d")) %>% 
  filter(date >= '2023-06-01') 


SF_urlClicks <- function(df) {
  ids <- paste0("'", df$id, "'", collapse = ",")
  
  my_soql <- sprintf(
    "SELECT Id, Name, et4ae5__LinkURL__c, et4ae5__Send_Definition__c, et4ae5__NumberOfTotalClicks__c,
            et4ae5__Unique_Link_ID__c
     FROM et4ae5__AggregateLink__c
     WHERE et4ae5__Send_Definition__c IN (%s)", ids
  )
  
  url_stats <- sf_query(my_soql, "et4ae5__AggregateLink__c", api_type = "Bulk 1.0") %>%
    rename(
      id = Id,
      URL = et4ae5__LinkURL__c,
      SendDefinition = et4ae5__Send_Definition__c,
      storyClicks = et4ae5__NumberOfTotalClicks__c,
      uniqueLinkID = et4ae5__Unique_Link_ID__c
    ) %>%
    left_join(df %>% select(id, name), by = c("SendDefinition" = "id")) %>%
    mutate(
      url = ifelse(grepl("term=question", URL), paste("Question -", sub("\\?(.*)", "", URL)), URL),
      url = ifelse(grepl("term=chart", url), paste("Chart -", sub("\\?(.*)", "", url)), url),
      url = ifelse(grepl("https://rmi.org/events/", url), paste("Events -", sub("\\?(.*)", "", url)), url),
      plain_text = grepl("plain-text", url),
      url = gsub("\\&utm_content=spark-b|\\&utm_content=spark-a|\\&utm_term=plain-text|-plain-text|\\&utm_term=text", "", url)
    ) %>%
    mutate(
      url = case_when(
        grepl("donation/checkout", url) ~ "Donate",
        grepl("subscribe", url) ~ "Subscribe",
        grepl("stories", url) ~ "Stories",
        grepl("research", url) ~ "Research",
        grepl("our-work", url) ~ "Our Work",
        TRUE ~ sub("\\?(.*)", "", url)
      )
    )
  
  return(url_stats)
}

getSFLinkClicks <- function(newsletter_df) {
  allCleaned <- data.frame()
  
  for (i in 1:nrow(newsletter_df)) {
    email_id <- newsletter_df[i, "id"]
    email_name <- newsletter_df[i, "name"]
    
    # Get URL click data from SF
    click_data <- SF_urlClicks(newsletter_df[i, , drop = FALSE])
    
    if (nrow(click_data) == 0) next
    
    # Compute plainTextClicks before any URL cleaning
    plainTextClicks <- click_data %>%
      filter(grepl("plain-text", URL)) %>%
      summarise(clicks = sum(as.numeric(storyClicks), na.rm = TRUE)) %>%
      pull(clicks)
    
    # Compute total clicks before grouping
    totalClicks <- click_data %>%
      summarise(clicks = sum(as.numeric(storyClicks), na.rm = TRUE)) %>%
      pull(clicks)
    
    plainTextPCT <- round(plainTextClicks / totalClicks, 3)
    
    # Now clean and label URLs
    click_data <- click_data %>%
      mutate(
        url = URL,
        url = ifelse(grepl("term=question", url), paste("Question -", sub("\\?(.*)", "", url)), url),
        url = ifelse(grepl("term=chart", url), paste("Chart -", sub("\\?(.*)", "", url)), url),
        url = ifelse(grepl("https://rmi.org/events/", url), paste("Events -", sub("\\?(.*)", "", url)), url),
        url = gsub("\\&utm_content=spark-b|\\&utm_content=spark-a|\\&utm_term=plain-text|-plain-text|\\&utm_term=text", "", url),
        url = case_when(
          grepl("donation/checkout", url) ~ "Donate",
          grepl("subscribe", url) ~ "Subscribe",
          grepl("stories", url) ~ "Stories",
          grepl("research", url) ~ "Research",
          grepl("our-work", url) ~ "Our Work",
          TRUE ~ sub("\\?(.*)", "", url)
        )
      )
    
    # Group and summarize clicks
    grouped <- click_data %>%
      group_by(url) %>%
      summarise(clicks = sum(as.numeric(storyClicks), na.rm = TRUE), .groups = "drop") %>%
      arrange(-clicks)
    
    # Add tags and metadata
    grouped <- grouped %>%
      mutate(
        tag = case_when(
          grepl("Question", url) ~ "question",
          grepl("Chart", url) ~ "chart",
          grepl("Events", url) ~ "events",
          TRUE ~ NA_character_
        ),
        email = email_name,
        plainTextPCT = plainTextPCT
      )
    
    allCleaned <- bind_rows(allCleaned, grouped)
    print(paste0(i, ": ", email_name))
  }
  
  return(allCleaned)
}


allSpark_SF <- SF_urlClicks(allSparkSF) %>%
  left_join(allSparkSF, by = c("SendDefinition" = "id")) %>%
  select(-name.y) %>%
  rename(name = name.x)


# Testing

SF_IndividualClicks <- function(df) {
 # ids <- paste0("'", df$id, "'", collapse = ",")
  
  my_soql <- sprintf(
    "SELECT Id, Name, et4ae5__LinkURL__c, CreatedById, CreatedDate, et4ae5__ExactTargetLinkID__c,
    et4ae5__Individual_Email_Result__c, et4ae5__Last_Clicked__c, et4ae5__NumberOfTotalClicks__c,
    et4ae5__Unique_Link_ID__c, IsDeleted, LastModifiedById, LastModifiedDate, OwnerId, et4ae5__Individual_Email_Result__r.et4ae5__Lead__c, 
    et4ae5__Individual_Email_Result__r.et4ae5__Contact__c
    FROM et4ae5__IndividualLink__c"
  )
  
  url_stats <- sf_query(my_soql, "et4ae5__IndividualLink__c", api_type = "Bulk 1.0")
  
  return(url_stats)
}


# allMC <- getLinkClicks(newsletter = marketCatalystNLs) 

# commented out bc currently no observations!!

#allMC_SF <- SF_urlClicks(allMCSF) %>%
#  left_join(allMCSF, by = c("SendDefinition" = "id"))


### get email stats
getSFEmailStats <- function (df) {
  
  ids <- paste0("'", df$id, "'", collapse = ",")
  
  my_soql <- sprintf("SELECT Id, et4ae5__EmailName__c, et4ae5__Subject__c, 
                    et4ae5__DateSent__c, CreatedDate, et4ae5__NumberSent__c, et4ae5__Number_Delivered__c, 
                    et4ae5__Deliverability_Rate__c, et4ae5__NumberofUniqueOpens__c, et4ae5__Open_Rate__c, 
                    et4ae5__NumberofTotalClicks__c, et4ae5__NumberofUniqueClicks__c, et4ae5__Click_Through_Rate__c,
                    et4ae5__Unsubscribe_Rate__c
                    
                    FROM et4ae5__SendDefinition__c
                    WHERE Id IN (%s)", 
                     ids
  )
  
  email_stats <- sf_query(my_soql, "et4ae5__SendDefinition__c", api_type = "Bulk 1.0") %>% 
    rename(
      id = Id,
      name = et4ae5__EmailName__c,
      subject = et4ae5__Subject__c,
      DateSent = et4ae5__DateSent__c,
      CreatedDate = CreatedDate,
      total_clicks = et4ae5__NumberofTotalClicks__c,
      sent = et4ae5__NumberSent__c,
      delivered = et4ae5__Number_Delivered__c,
      delivered_rate = et4ae5__Deliverability_Rate__c,
      unique_opens = et4ae5__NumberofUniqueOpens__c,
      open_rate = et4ae5__Open_Rate__c,
      unique_clicks = et4ae5__NumberofUniqueClicks__c,
      unique_CTR = et4ae5__Click_Through_Rate__c,
      opt_out_rate = et4ae5__Unsubscribe_Rate__c,
      
    )
  
  
  return(email_stats)
}


allSparkEmailStatsSF <- getSFEmailStats(allSparkSF)

#allMCEmailStatsSF <- getSFEmailStats(allMCSF)


install.packages("furrr")
library(furrr)

getSFSparkStats <- function(emailStatsDF = allSparkEmailStatsSF, emailClicksDF = allSpark_SF){
  
  CTR_mean <- (sum(emailStatsDF$unique_CTR)/length(emailStatsDF$unique_CTR))
  openRT_mean <- (sum(emailStatsDF$open_rate)/length(emailStatsDF$open_rate))
  
  allEmailStats2 <- emailStatsDF %>% 
    mutate(UCTRvsAvg = 10*(unique_CTR - CTR_mean),
           ORvsAvg = (open_rate - openRT_mean)) %>% 
    mutate_at(c("open_rate", "unique_CTR", 'UCTRvsAvg', 'ORvsAvg'), ~ round(.x, 3)) %>% 
    relocate(UCTRvsAvg, .after = unique_CTR) %>% 
    relocate(ORvsAvg, .after = open_rate) %>% 
    #might just be name
    left_join(emailClicksDF, by = c('name' = 'name')) %>% 
    #mutate(COR = as.numeric(clicks / unique_opens / 100),
    #       COR_chart = as.numeric(clicks_chart / unique_opens / 100),
    #       COR_question = as.numeric(clicks_question / unique_opens / 100),
    #       COR_events = as.numeric(clicks_events / unique_opens / 100),
    #       rows = '') %>% 
    #mutate_at(c('url', 'url_chart', 'url_question', 'url_events'), ~ sub('\\?(.*)', '', .x)) %>% 
    mutate(date = as.Date(str_extract(name, '[0-9]+-[0-9]+-[0-9]+')))
  
  allEmailStats3 <- allEmailStats2[rev(order(allEmailStats2$date)),]
  rownames(allEmailStats3) <- NULL
  allEmailStats3[, 'rows'] <- as.numeric(rownames(allEmailStats3)) + 1
  

  plan(multisession, workers = 6) 
  
  page_names <- allEmailStats3 %>%
    select(url) %>%
    distinct() %>%
   # mutate(title = map_chr(url, possibly(get_title, otherwise = NA_character_)))
    mutate(title = future_map_chr(url, possibly(get_title, otherwise = NA_character_)))
  
  
  allEmailStats4 <- allEmailStats3 %>% 
    left_join(page_names, by = c('url' = 'url'))  %>%
    select(-id.y, -subject.y) %>%
    rename(
      id = id.x,
      subject = subject.x
    ) %>%
    select(id, name, date, subject_line = subject, sent, delivered, delivered_rate, unique_opens, open_rate,
           ORvsAvg, total_clicks, unique_clicks, unique_CTR, UCTRvsAvg, opt_out_rate, url = URL, title, clicks = storyClicks)
  # mutate_at(c('COR', 'COR_chart', 'COR_events'), ~ round(.x * 100, 3)) %>% 
  #  mutate_at(c('COR_question'), ~ round(.x * 100, 4)) %>% 
  # mutate_at(c('url_chart', 'url_question', 'url_events'), ~ ifelse(grepl('NA', .x), NA, .x)) %>% 
  #  mutate_at(c('clicks_chart', 'COR_chart', 'clicks_question', 'COR_question', 'clicks_events', 'COR_events'), ~ ifelse(.x == 0.000, NA, .x)) %>% 
  #mutate_at(c('url_chart', 'url_question', 'url_events'), ~ sub('(.*) - ', '', as.character(.x))) %>% 
  #mutate(COR_chart = ifelse(is.na(clicks_chart), NA, COR_chart),
  #      COR_question = ifelse(is.na(clicks_question), NA, COR_question),
  #     COR_events = ifelse(is.na(clicks_events), NA, COR_events)) %>% 
  
  #select(id, name, date, subject_line = subject, sent, delivered, delivered_rate, unique_opens, 
  #       open_rate, ORvsAvg, total_clicks, unique_clicks, unique_CTR, UCTRvsAvg, 
  #       opt_outs, opt_out_rate, plaintext_rate = plainTextPCT, url, title, clicks, COR,
  #       url_chart, clicks_chart, url_question, clicks_question, 
  #       clicks_events) 
  
  return(allEmailStats4)
  #GET(url, timeout(30))
}



#allSparkStats <- getSparkStats()

#  write.csv(allSparkStats, 'allSparkStats.csv')

allSparkStats <- getSFSparkStats() 

#write.csv(allSFSparkStats, 'allSparkStats.csv')

#allSparkStats <- bind_rows(allSparkStats, allSFSparkStats)

write.csv(allSparkStats, 'allSparkStats.csv')



getSFMCStats <- function(emailStatsDF = allMCEmailStatsSF, emailClicksDF = allMC_SF){
  
  CTR_mean <- (sum(emailStatsDF$unique_CTR)/length(emailStatsDF$unique_CTR))
  openRT_mean <- (sum(emailStatsDF$open_rate)/length(emailStatsDF$open_rate))
  
  allEmailStats2 <- emailStatsDF %>% 
    mutate(UCTRvsAvg = 10*(unique_CTR - CTR_mean),
           ORvsAvg = (open_rate - openRT_mean)) %>% 
    mutate_at(c("open_rate", "unique_CTR", 'UCTRvsAvg', 'ORvsAvg'), ~ round(.x, 3)) %>% 
    relocate(UCTRvsAvg, .after = unique_CTR) %>% 
    relocate(ORvsAvg, .after = open_rate) %>% 
    #might just be name
    left_join(emailClicksDF, by = c('name' = 'name')) %>% 
    #mutate(COR = as.numeric(clicks / unique_opens / 100),
    #       COR_chart = as.numeric(clicks_chart / unique_opens / 100),
    #       COR_question = as.numeric(clicks_question / unique_opens / 100),
    #       COR_events = as.numeric(clicks_events / unique_opens / 100),
    #       rows = '') %>% 
    #mutate_at(c('url', 'url_chart', 'url_question', 'url_events'), ~ sub('\\?(.*)', '', .x)) %>% 
    mutate(date = as.Date(str_extract(name, '[0-9]+-[0-9]+-[0-9]+')))
  
  allEmailStats3 <- allEmailStats2[rev(order(allEmailStats2$date)),]
  rownames(allEmailStats3) <- NULL
  allEmailStats3[, 'rows'] <- as.numeric(rownames(allEmailStats3)) + 1
  
  plan(multisession, workers = 6) 
  
  
  page_names <- allEmailStats3 %>%
    select(url) %>%
    distinct() %>%
    mutate(title = map_chr(url, possibly(get_title, otherwise = NA_character_)))
  
  allEmailStats4 <- allEmailStats3 %>% 
    left_join(page_names, by = c('url' = 'url'))  %>%
    select(-id.y, -subject.y) %>%
    rename(
      id = id.x,
      subject = subject.x
    ) %>%
    select(id, name, date, subject_line = subject, sent, delivered, delivered_rate, unique_opens, open_rate,
           ORvsAvg, total_clicks, unique_clicks, unique_CTR, UCTRvsAvg, opt_out_rate, url = URL, title, clicks = storyClicks)
  # mutate_at(c('COR', 'COR_chart', 'COR_events'), ~ round(.x * 100, 3)) %>% 
  #  mutate_at(c('COR_question'), ~ round(.x * 100, 4)) %>% 
  # mutate_at(c('url_chart', 'url_question', 'url_events'), ~ ifelse(grepl('NA', .x), NA, .x)) %>% 
  #  mutate_at(c('clicks_chart', 'COR_chart', 'clicks_question', 'COR_question', 'clicks_events', 'COR_events'), ~ ifelse(.x == 0.000, NA, .x)) %>% 
  #mutate_at(c('url_chart', 'url_question', 'url_events'), ~ sub('(.*) - ', '', as.character(.x))) %>% 
  #mutate(COR_chart = ifelse(is.na(clicks_chart), NA, COR_chart),
  #      COR_question = ifelse(is.na(clicks_question), NA, COR_question),
  #     COR_events = ifelse(is.na(clicks_events), NA, COR_events)) %>% 
  
  #select(id, name, date, subject_line = subject, sent, delivered, delivered_rate, unique_opens, 
  #       open_rate, ORvsAvg, total_clicks, unique_clicks, unique_CTR, UCTRvsAvg, 
  #       opt_outs, opt_out_rate, plaintext_rate = plainTextPCT, url, title, clicks, COR,
  #       url_chart, clicks_chart, url_question, clicks_question, 
  #       clicks_events) 
  
  return(allEmailStats4)
  #GET(url, timeout(30))
}

#allMCStats <- getMCStats()

#No data currently
#allMCStats <- getSFMCStats() 

#allMCStats <- bind_rows(allMCStats, allSFMCStats)

#write.csv(allMCStats, 'allMCStats.csv')




