
setwd("~/GitHub/InfluenceCampaigns/R/powerBI")
### get packages and functions
source("packages.R")  
source("functions.R")  

load_dot_env('Renviron.env')

#detach("package:RMySQL", unload = TRUE)

## Dbase connection

library(DBI)

library(RMariaDB)
ssl = "~/GitHub/DigiCertGlobalRootCA.crt.pem"


con <- dbConnect(
        RMariaDB::MariaDB(),
        dbname = 'rmi_influence_campaign',
        username = Sys.getenv('DBASE_USER'),
        password = Sys.getenv("DBASE_PWD"),
        host = Sys.getenv('DBASE_IP'),
        port = '3306',
        ssl.ca = normalizePath("~/GitHub/DigiCertGlobalRootCA.crt.pem")
      )



# a <- dbListTables(con)
# 
# b <- as.character("allTraffic" , "campaignNewsletters" ,"campaignPosts"  ,  "contentSummary" ,   "donations", 
#           "geographyTraffic"  ,  "mediaReferrals"  ,    "SFcampaigns"   ,      "socialTraffic"    ,   "targetCampaign")
# 
# 
# readxl::excel_sheets('OCI+ Dashboard Dataset.xlsx')
# 
# for(i in readxl::excel_sheets('OCI+ Dashboard Dataset.xlsx')){
#   df <- read.xlsx('OCI+ Dashboard Dataset.xlsx', sheet = i, detectDates = TRUE)
#   dbWriteTable(con, i, df, append = TRUE)
# 
# }


### SET CAMPAIGN

#' Monday.com Token
mondayToken <- Sys.getenv("Monday_Token")

#' Sprout Social Token
sproutToken <- Sys.getenv("SproutSocial_Token")
sproutHeader <- c("Authorization" = sproutToken, "Accept" = "application/json", "Content-Type" = "application/json")
currentDate <- paste(Sys.Date())
sixMonthsAgo <- ymd(currentDate) - months(6) # Updated to 6 months on 2/5/2024.

#' Pardot API Token & Request Headers
pardotTokenV4 <- Sys.getenv("Pardot_TokenV4")
pardotTokenV5 <- Sys.getenv("Pardot_TokenV5")
pardotBusinessID <- Sys.getenv("Pardot_Business_ID")
header4 <- c("Authorization" = pardotTokenV4, "Pardot-Business-Unit-Id" = pardotBusinessID)
header5 <- c("Authorization" = pardotTokenV5, "Pardot-Business-Unit-Id" = pardotBusinessID)


## GA Authentication
ga_auth(email = "elizabeth.duchan@rmi.org")

#' SF Authentication
sf_auth()
objects <- sf_list_objects()
head(objects)

## set google sheet
ss <- 'COP28_testing Dashboard Dataset.xlsx'

### START OF NEW VERSION
rmiPropertyID <- 354053620
dateRangeGA <- c("2023-01-01", paste(currentDate))
campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+", "nycw24", "transition-narrative")

# Call the main function to get filtered URLs with campaign tags
filtered_urls <- data.frame(pagePath = character(),
                            campaign_tag = character(),
                            stringsAsFactors = FALSE)
filtered_urls <- getWebsiteURLs(propertyID = rmiPropertyID, campaign_tags = campaign_tags)

# Process the filtered URLs: add page titles and types
campaign_urls <- filtered_urls %>%
  mutate(pageTitle = sapply(pagePath, scrape_pagetitle)) %>%
  mutate(pageType = sapply(pagePath, scrape_pagetype)) %>%
  select(pageTitle, pageType, pagePath, campaign_tag) %>%
  mutate(propertyID = rmiPropertyID)

# View the result
print(campaign_urls)


#campaigns <- c('OCI', 'Coal v Gas', "COP28")
campaigns <- unique(campaign_urls$campaign_tag)
unique_campaign_tags <- unique(campaign_urls$campaign_tag)


#add back later if needed
allCampaignPages <- data.frame(pageTitle = character(),
                               dashboardCampaign = character(),
                               stringsAsFactors = FALSE)
#campaign <- campaigns[3]
#campaignID <- campaign

# for multiple campaigns
#campaignDataList <- list()

#campaignID = c("cop28", "2023-2025_coalvgas", "oci+")

# pages_df <- filtered_urls %>%
#   rename(campaignID = campaign_tag,  # Rename campaignTag to campaignID
#          pageTitle = pagePath) 

# NEW VERSION, but was causing errors
# for (i in campaigns){
#   campaignID <- i  
#   print(paste("Processing campaign:", campaignID))
#   #campaign <- i
#   #print(campaign)
# 
#   campaignPages <- campaign_urls %>% 
#     filter(campaign_tag == campaignID)
#   
#   print(campaignPages)  # Print the filtered dataframe to inspect the output
#   
#   if (nrow(campaignPages) > 0) {
#     allCampaignPages <- rbind(allCampaignPages, campaignPages)
#     #campaignDataList[[campaignID]] <- campaignPages
#     
#   }
# }

for (campaignID in campaigns){
  print(paste("processing campaign:", campaignID))
  
  campaignPages <- campaign_urls %>%
    filter(campaign_tag == campaignID) %>%
    mutate(dashboardCampaign = campaignID)
  

  print(campaignPages)
  
  if(nrow(campaignPages)>0){
    allCampaignPages <- bind_rows(allCampaignPages, campaignPages)
  }
}
  
  
print(allCampaignPages)

campaignPages <- allCampaignPages

# page_to_campaign <- allCampaignPages %>%
#   select(pageTitle, campaign_tag) %>%
#   distinct()

# Rename columns for clarity
# colnames(page_to_campaign) <- c("pageTitle", "campaignID")

#campaignPages <- campaign_urls %>% 
 # filter(campaign_tag == campaignID)


### READ CAMPAIGN KEY
#campaignsSheet <- c('OCI', 'Coal v Gas', "COP28")

#campaignKey <- read.xlsx('Campaign Key.xlsx', sheet = paste0('Campaign Key - ', campaignsSheet))
#instead, we'd want to make this "key" on the r interface

#campaignID <- as.character(campaignKey[1, c('campaignID')])

# NEW: remaking reportID and eventID dataframes

# List of sheet names
sheets <- c("Campaign Key - OCI", "Campaign Key - Coal v Gas", "Campaign Key - COP28")

# Initialize an empty list to store reportId values from each sheet
reportID_data <- data.frame(reportID = character(), stringsAsFactors = FALSE)

# Loop through each sheet and extract the reportId column
for (sheet in sheets) {
  # Read the sheet
  data <- read.xlsx('Updated Campaign Key.xlsx', sheet = sheet)
  
  # Check if the reportId column exists
  if ("reportID" %in% colnames(data)) {
    # Combine the reportId values into the combined dataframe
    temp_data <- data.frame(reportID = data$reportID, IDCampaignID = sheet)
    reportID_data <- rbind(reportID_data, temp_data)
  } else {
    warning(paste("reportId column not found in sheet", sheet))
  }
}

reportID_data$IDCampaignID <- recode(reportID_data$IDCampaignID, 
                                   "Campaign Key - OCI" = "oci+",
                                   "Campaign Key - Coal v Gas" = "2023-2025_coalvgas",
                                   "Campaign Key - COP28" = "cop28")

# remove duplicates/ NA
reportID_data <- reportID_data %>%
  filter(!is.na(reportID) & reportID != "") %>%
  filter(!(reportID == "7016f0000023VTHAA2" & IDCampaignID == "2023-2025_coalvgas")) %>%
  mutate(IDCampaignID = case_when(
    reportID == "7016f0000023TgYAAU" & IDCampaignID == "2023-2025_coalvgas" ~ "oci+",
    TRUE ~ IDCampaignID
  ))


reportID_data <- unique(reportID_data)

# new for metadata tag
report_id_urls <- filtered_urls 

reportID_in_metadata <- scrape_report_id(report_id_urls) 

relevant_reportID_in_metadata <- reportID_in_metadata %>%
  filter(!is.na(report_id)) %>%
  select(report_id, campaign_tag) %>%
  rename(reportID = report_id, IDCampaignID = campaign_tag)

reportID_data <- bind_rows(reportID_data, relevant_reportID_in_metadata)
reportID_data <- unique(reportID_data)


# Initialize an empty list to store reportId values from each sheet
eventID_data <- data.frame(eventID = character(), stringsAsFactors = FALSE)

# Loop through each sheet and extract the reportId column
for (sheet in sheets) {
  # Read the sheet
  data <- read.xlsx('Campaign Key.xlsx', sheet = sheet)
  
  # Check if the reportId column exists
  if ("eventID" %in% colnames(data)) {
    # Combine the reportId values into the combined dataframe
    temp_data2 <- data.frame(eventID = data$eventID, IDCampaignID = sheet)
    eventID_data <- rbind(eventID_data, temp_data2)
  } else {
    warning(paste("eventId column not found in sheet", sheet))
  }
}

eventID_data$IDCampaignID <- recode(eventID_data$IDCampaignID, 
                                     "Campaign Key - OCI" = "oci+",
                                     "Campaign Key - Coal v Gas" = "2023-2025_coalvgas",
                                     "Campaign Key - COP28" = "cop28")

# remove duplicates/ NA
eventID_data <- eventID_data %>%
  filter(!is.na(eventID) & eventID != "")  # Keep only rows where reportId is not NA and not empty

eventID_data <- unique(eventID_data)


#new for metadata tag

event_id_urls <- filtered_urls 

eventID_in_metadata <- scrape_event_id(event_id_urls) 

relevant_eventID_in_metadata <- eventID_in_metadata %>%
  filter(!is.na(event_id)) %>%
  select(event_id, campaign_tag) %>%
  rename(eventID = event_id, IDCampaignID = campaign_tag)

eventID_data <- bind_rows(eventID_data, relevant_eventID_in_metadata)
eventID_data <- unique(eventID_data)

eventID_dataTiny <- head(eventID_data)
eventID_dataTiny2 <- tail(eventID_data, 1)
  
#campaignPages <- campaign_urls[, c('propertyID', 'pagePath')]
propertyIDs <- unique(campaignPages$propertyID)
propertyIDs <- propertyIDs[!is.na(propertyIDs)]


#if (campaignID == 'COP28') campaignReports <- campaignKey[campaignKey$type =='Report', 'type'] %>%
# as.data.frame() %>% rename('reportID'='.') else 

# # OLD 
# campaignReports <- campaignPages[!is.na(campaignPages$reportID), 'reportID'] %>%
#   as.data.frame() %>%
#   rename('reportID'='.')

#if (campaignID == 'COP28') campaignEvents <- campaignKey[campaignKey$type=='Event', 'type'] %>%
# as.data.frame() %>% rename('eventID'='.') else 

# #OLD
# campaignEvents <- campaignPages[!is.na(campaignPages$eventID), 'eventID'] %>%
#   as.data.frame() %>%
#   rename('eventID'='.')

#if(length(campaignReports) == 0) hasReport <- FALSE else hasReport <- TRUE
#if(length(campaignEvents) == 0) hasEvent <- FALSE else hasEvent <- TRUE

# hasReport <- length(campaignReports) != 0
# hasEvent <- length(campaignEvents) != 0

# NEW
hasReport <- length(reportID_data) != 0
hasEvent <- length(eventID_data) != 0

#if(campaignID == 'COP28') socialTag <- 'COP28' else 
 # socialTag <- as.character(campaignKey[1, c('socialTag')])

socialTag <- unique(campaignPages$campaign_tag[!is.na(campaignPages$campaign_tag)])


#### GOOGLE ANALYTICS ####
message('GETTING GOOGLE ANALYTICS DATA')

#' SUMMARY
#' 
#' 1. Pulls list of pages from campaign key file
#' 2. Gets page title and metadata by webscraping provided URLs
#' 3. For all pages
#'      Get key metrics - page views, users, engagement duration
#'      Get acquisition data - sessions + conversions - broken down by channel
#'      Get social media acquisition data - sessions
#'      Get page views broken down by country and region
#'      Get page traffic - sessions - driven by referral sources that have been identified as “Media” 
#'        - These sources are defined in the referralSites file
#' 4. If a new property ID is provided in the campaign key, process is repeated for the new website
#'    and datasets are combined
#' 5. write dataset

#' set GA variables and property ID
rmiPropertyID <- 354053620 #no longer will need?
metadataGA4 <- ga_meta(version = "data", rmiPropertyID)
dateRangeGA <- c("2023-01-01", paste(currentDate))


### get referral sites
referralSites <- read.xlsx('Referral Site Categories.xlsx', sheet = 'All Referral Sites')

###
campaignPages <- campaignPages %>% 
  mutate(site = ifelse(propertyID == rmiPropertyID, 'rmi.org', sub('/(.*)', '', sub('(.*)https://', '', pagePath)))) %>% 
  filter(!is.na(propertyID))

pageData <- data.frame(site = campaignPages$site, 
                       pageURL = campaignPages$pagePath, 
                       pageTitle = '', 
                       pageType = '', 
                       metadata = '',
                       program = '')

pageData <- getPageData(pageData)

pageData <- pageData %>%
  mutate(pageType = ifelse(pageTitle == "Toward a Technology Ecosystem for Carbon Accounting - RMI", 
                           "Article", 
                           pageType))
  
# maybe need to check on hub once it's working

# Hard code section for pages without pageType in metadata
#pageData <- pageData %>% 
 # mutate(pageType = ifelse(pageTitle == 'RMI at COP - RMI', 'Hub', pageType),
  #       pageType = ifelse(pageTitle == 'RMI China Seminar at COP28', 'Event', pageType))

#' set page titles
rmiPages <- pageData %>% filter(site == 'rmi.org')
pages <- rmiPages[['pageTitle']]

pages2 <- campaign_urls$pagePath

#' get page metrics
pageMetrics <- getPageMetrics(rmiPropertyID, pages) 

#' get acquisition - CHECK.
acquisition <- getAcquisition(rmiPropertyID, pages) 

#' get social traffic
#socialTraffic <- getTrafficSocial(rmiPropertyID, pages, campaignID) 

socialTraffic <- getTrafficSocial2(rmiPropertyID, pages, campaignPages)

#' get geographic segments
#geographyTrafficBad <- getTrafficGeography(rmiPropertyID, pages) 

geographyTraffic <- getTrafficGeography2(rmiPropertyID, pages, campaignPages) 

#' get referrals
mediaReferrals <- getReferrals(rmiPropertyID, pages)


#' get data for new website if a new GA property ID is provided
if(length(propertyIDs) > 1){
  
  #' set property ID for new website
  sitePropertyID <- propertyIDs[2]
  
  #' get pages
  newSitePages <- pageData %>% filter(site != 'rmi.org')
  pages <- unique(newSitePages[['pageTitle']])
  
  #' get website URL
  newSiteURL <- unique(newSitePages[['site']])
  
  ###
  
  #' get page metrics + acquisition
  pageMetricsNS <- getPageMetrics(sitePropertyID, pages) %>% 
    distinct()
  
  pageMetrics <- pageMetrics %>% rbind(pageMetricsNS)
  
  #' get acquisition
  acquisitionNS <- getAcquisition(sitePropertyID, pages, site = newSiteURL) 
  acquisition <- acquisition %>% rbind(acquisitionNS)
  
  #' bind page metrics and pivot table so that sessions/conversions are stored in one column
  #' this is to make a Power BI table column that changes based on an applied filter
  #' here, add event downloads
  allTraffic <- pageData %>% 
    select(site, pageTitle) %>% 
    distinct() %>% 
    plyr::rbind.fill(acquisition) %>% 
    pivot_longer(cols = c(Sessions:EventRegistered), names_to = "type", values_to = "count") %>% 
    mutate(count = round(count, 1)) %>% 
    left_join(select(pageMetrics, c(pageTitle, screenPageViews:avgEngagementDuration, pageType, icon)), by = 'pageTitle') %>% 
    mutate(count = as.numeric(ifelse(is.na(count), 0, count))) %>% 
    filter(defaultChannelGroup != 'Unassigned' & !is.na(defaultChannelGroup))
  
  numChannels <- allTraffic %>% group_by(pageTitle, type) %>% summarize(numChannels = n())
  
  allTraffic <- allTraffic %>% 
    left_join(numChannels) %>% 
    mutate('page_views' = round(screenPageViews/numChannels, 2)) %>% 
    select(-c(screenPageViews, numChannels))
  
  #' get social media acquisition and bind
  socialTrafficNS <- getTrafficSocial2(sitePropertyID, pages, site = newSiteURL) 
  
  socialTraffic <- socialTraffic %>% 
    rbind(socialTrafficNS)
  
  #' get page traffic geography and bind
  geographyTrafficNS <- getTrafficGeography2(sitePropertyID, pages, site = newSiteURL) 
  
  geographyTraffic <- geographyTraffic %>% 
    rbind(geographyTrafficNS) %>%
    rename(regionPageViews = 'Region Page Views')
  
  #' get media referrals and bind
  mediaReferralsNS <- getReferrals(sitePropertyID, pages, site = newSiteURL)
  
  mediaReferrals <- mediaReferrals %>% 
    rbind(mediaReferralsNS)
  
} else {
  #' bind page metrics and pivot table so that sessions/conversions are stored in one column
  #' this is to make a Power BI table column that changes based on an applied filter
  allTraffic <- pageData %>% 
    select(site, pageTitle) %>% 
    distinct() %>% 
    plyr::rbind.fill(acquisition) %>% 
    pivot_longer(cols = c(Sessions:EventRegistered), names_to = "type", values_to = "count") %>% # Update to add additional GA4 goals in the select function
    mutate(count = round(count, 1)) %>% 
    left_join(select(pageMetrics, c(pageTitle, screenPageViews:avgEngagementDuration, pageType, program, icon)), by = 'pageTitle') %>% 
    mutate(count = as.numeric(ifelse(is.na(count), 0, count))) %>% 
    filter(defaultChannelGroup != 'Unassigned' & !is.na(defaultChannelGroup)) 
  
  numChannels <- allTraffic %>% group_by(pageTitle, type) %>% summarize(numChannels = n())
  
  allTraffic <- allTraffic %>% 
    left_join(numChannels) %>% 
    mutate('page_views' = round(screenPageViews/numChannels, 2)) %>% 
    select(-c(screenPageViews, numChannels))
  
}

allTraffic <- allTraffic %>%
  filter(!is.na(pageType)) %>%
  mutate(pageType = ifelse(pageTitle == "Toward a Technology Ecosystem for Carbon Accounting", 
                           "Article", 
                           pageType))


### END OF NEW VERSION

# #### SET CAMPAIGN #### THIS IS THE OLD, UNTOUCHED VERSION
# 
# #would need to add new campaigns
# campaigns <- c('OCI', 'Coal v Gas', "COP28")
#' campaign <- campaigns[3]
#' campaignID <- campaign
#' 
#' # Use this once ready to process for multiple campaigns
#' for (i in campaigns){
#'   campaign <- i
#'   print(campaign)
#' }
#' 
#' ### READ CAMPAIGN KEY
#' campaignKey <- read.xlsx('Campaign Key.xlsx', sheet = paste0('Campaign Key - ', campaign))
#' #instead, we'd want to make this "key" on the r interface
#' 
#' #campaignID <- as.character(campaignKey[1, c('campaignID')])
#' campaignPages <- campaignKey[, c('propertyID', 'page')]
#' 
#' propertyIDs <- unique(campaignKey$propertyID)
#' propertyIDs <- propertyIDs[!is.na(propertyIDs)]
#' 
#' 
#' 
#' #if (campaignID == 'COP28') campaignReports <- campaignKey[campaignKey$type =='Report', 'type'] %>%
#'  # as.data.frame() %>% rename('reportID'='.') else 
#' 
#' campaignReports <- campaignKey[!is.na(campaignKey$reportID), 'reportID'] %>%
#'   as.data.frame() %>%
#'   rename('reportID'='.')
#' 
#' 
#' #if (campaignID == 'COP28') campaignEvents <- campaignKey[campaignKey$type=='Event', 'type'] %>%
#'  # as.data.frame() %>% rename('eventID'='.') else 
#'     
#' campaignEvents <- campaignKey[!is.na(campaignKey$eventID), 'eventID'] %>%
#'   as.data.frame() %>%
#'   rename('eventID'='.')
#' 
#' if(length(campaignReports) == 0) hasReport <- FALSE else hasReport <- TRUE
#' if(length(campaignEvents) == 0) hasEvent <- FALSE else hasEvent <- TRUE
#' 
#' if(campaignID == 'COP28') socialTag <- 'COP28' else 
#'   socialTag <- as.character(campaignKey[1, c('socialTag')])
#' 
#' 
#' #### GOOGLE ANALYTICS ####
#' message('GETTING GOOGLE ANALYTICS DATA')
#' 
#' #' SUMMARY
#' #' 
#' #' 1. Pulls list of pages from campaign key file
#' #' 2. Gets page title and metadata by webscraping provided URLs
#' #' 3. For all pages
#' #'      Get key metrics - page views, users, engagement duration
#' #'      Get acquisition data - sessions + conversions - broken down by channel
#' #'      Get social media acquisition data - sessions
#' #'      Get page views broken down by country and region
#' #'      Get page traffic - sessions - driven by referral sources that have been identified as “Media” 
#' #'        - These sources are defined in the referralSites file
#' #' 4. If a new property ID is provided in the campaign key, process is repeated for the new website
#' #'    and datasets are combined
#' #' 5. write dataset
#' 
#' #' set GA variables and property ID
#' rmiPropertyID <- 354053620 #no longer will need?
#' metadataGA4 <- ga_meta(version = "data", rmiPropertyID)
#' dateRangeGA <- c("2023-01-01", paste(currentDate))
#' 
#' 
#' ### get referral sites
#' referralSites <- read.xlsx('Referral Site Categories.xlsx', sheet = 'All Referral Sites')
#' 
#' ###
#' campaignPages <- campaignPages %>% 
#'   mutate(site = ifelse(propertyID == rmiPropertyID, 'rmi.org', sub('/(.*)', '', sub('(.*)https://', '', page)))) %>% 
#'   filter(!is.na(propertyID))
#' 
#' pageData <- data.frame(site = campaignPages$site, 
#'                        pageURL = campaignPages$page, 
#'                        pageTitle = '', 
#'                        pageType = '', 
#'                        metadata = '',
#'                        program = '')
#' 
#' pageData <- getPageData(pageData)
#' 
#' # Hard code section for pages without pageType in metadata
#' pageData <- pageData %>% 
#'   mutate(pageType = ifelse(pageTitle == 'RMI at COP - RMI', 'Hub', pageType),
#'          pageType = ifelse(pageTitle == 'RMI China Seminar at COP28', 'Event', pageType))
#' 
#' 
#' #' set page titles
#' rmiPages <- pageData %>% filter(site == 'rmi.org')
#' pages <- rmiPages[['pageTitle']]
#' 
#' #' get page metrics
#' pageMetrics <- getPageMetrics(rmiPropertyID, pages) 
#' 
#' #' get acquisition
#' acquisition <- getAcquisition(rmiPropertyID, pages) 
#' 
#' #' get social traffic
#' socialTraffic <- getTrafficSocial(rmiPropertyID, pages) 
#' 
#' #' get geographic segments
#' geographyTraffic <- getTrafficGeography(rmiPropertyID, pages) 
#' 
#' #' get referrals
#' mediaReferrals <- getReferrals(rmiPropertyID, pages)
#' 
#' 
#' 
#' #' get data for new website if a new GA property ID is provided
#' if(length(propertyIDs) > 1){
#'   
#'   #' set property ID for new website
#'   sitePropertyID <- propertyIDs[2]
#'   
#'   #' get pages
#'   newSitePages <- pageData %>% filter(site != 'rmi.org')
#'   pages <- unique(newSitePages[['pageTitle']])
#'   
#'   #' get website URL
#'   newSiteURL <- unique(newSitePages[['site']])
#'   
#'   ###
#'   
#'   #' get page metrics + acquisition
#'   pageMetricsNS <- getPageMetrics(sitePropertyID, pages) %>% 
#'     distinct()
#'   
#'   pageMetrics <- pageMetrics %>% rbind(pageMetricsNS)
#'   
#'   #' get acquisition
#'   acquisitionNS <- getAcquisition(sitePropertyID, pages, site = newSiteURL) 
#'   acquisition <- acquisition %>% rbind(acquisitionNS)
#'   
#'   #' bind page metrics and pivot table so that sessions/conversions are stored in one column
#'   #' this is to make a Power BI table column that changes based on an applied filter
#'   allTraffic <- pageData %>% 
#'     select(site, pageTitle) %>% 
#'     distinct() %>% 
#'     plyr::rbind.fill(acquisition) %>% 
#'     pivot_longer(cols = c(Sessions:DonationPageViews), names_to = "type", values_to = "count") %>% 
#'     mutate(count = round(count, 1)) %>% 
#'     left_join(select(pageMetrics, c(pageTitle, screenPageViews:avgEngagementDuration, pageType, icon)), by = 'pageTitle') %>% 
#'     mutate(count = as.numeric(ifelse(is.na(count), 0, count))) %>% 
#'     filter(defaultChannelGroup != 'Unassigned' & !is.na(defaultChannelGroup))
#'   
#'   numChannels <- allTraffic %>% group_by(pageTitle, type) %>% summarize(numChannels = n())
#'   
#'   allTraffic <- allTraffic %>% 
#'     left_join(numChannels) %>% 
#'     mutate('page_views' = round(screenPageViews/numChannels, 2)) %>% 
#'     select(-c(screenPageViews, numChannels))
#'   
#'   #' get social media acquisition and bind
#'   socialTrafficNS <- getTrafficSocial(sitePropertyID, pages, site = newSiteURL) 
#'   
#'   socialTraffic <- socialTraffic %>% 
#'     rbind(socialTrafficNS)
#'   
#'   #' get page traffic geography and bind
#'   geographyTrafficNS <- getTrafficGeography(sitePropertyID, pages, site = newSiteURL) 
#'   
#'   geographyTraffic <- geographyTraffic %>% 
#'     rbind(geographyTrafficNS) %>%
#'     rename(regionPageViews = 'Region Page Views')
#'   
#'   #' get media referrals and bind
#'   mediaReferralsNS <- getReferrals(sitePropertyID, pages, site = newSiteURL)
#'   
#'   mediaReferrals <- mediaReferrals %>% 
#'     rbind(mediaReferralsNS)
#'   
#' } else {
#'   #' bind page metrics and pivot table so that sessions/conversions are stored in one column
#'   #' this is to make a Power BI table column that changes based on an applied filter
#'   allTraffic <- pageData %>% 
#'     select(site, pageTitle) %>% 
#'     distinct() %>% 
#'     plyr::rbind.fill(acquisition) %>% 
#'     pivot_longer(cols = c(Sessions:DonationPageViews), names_to = "type", values_to = "count") %>% # Update to add additional GA4 goals in the select function
#'     mutate(count = round(count, 1)) %>% 
#'     left_join(select(pageMetrics, c(pageTitle, screenPageViews:avgEngagementDuration, pageType, program, icon)), by = 'pageTitle') %>% 
#'     mutate(count = as.numeric(ifelse(is.na(count), 0, count))) %>% 
#'     filter(defaultChannelGroup != 'Unassigned' & !is.na(defaultChannelGroup)) 
#'   
#'   numChannels <- allTraffic %>% group_by(pageTitle, type) %>% summarize(numChannels = n())
#'   
#'   allTraffic <- allTraffic %>% 
#'     left_join(numChannels) %>% 
#'     mutate('page_views' = round(screenPageViews/numChannels, 2)) %>% 
#'     select(-c(screenPageViews, numChannels))
#' 
#' }
#' 
#' allTraffic <- allTraffic %>%
#'   filter(!is.na(pageType))


### end of OLD, UNTOUCHED VERSION. ###

####'EMAIL NEWSLETTERS ####
message('GETTING NEWSLETTERS DATA')

#' SUMMARY
#' 
#' 1. Get all newsletters - Spark + Market Catalyst - from Newsletter Stats file
#' 2. Filter URLs to get newsletters that contain the page URLs in this campaign
#' 3. Write dataset

#' get all email stats 
#' WILL NEED TO CHANGE THIS


# need to make this a scraping function, because currently relies on csv files
if(!exists('allEmailStats')){
  allEmailStats <- getAllEmailStats()
}

#' get newsletter story URLs
pageURLs <- pageData$pageURL

campaignNewsletters <- getCampaignEmails(pageURLs) %>%
  left_join(campaignPages %>%
              select(pagePath, campaign_tag) %>%
              distinct(pagePath, .keep_all = TRUE), 
            by = c("story_url" = "pagePath")) %>%  
  rename(campaignID = campaign_tag)

# for emails with a/b testing

campaignNewsletters <- campaignNewsletters %>%
  group_by(name, story_url) %>%
  summarize(
    delivered_ = sum(delivered_, na.rm = TRUE),
    unique_opens = sum(unique_opens, na.rm = TRUE),
    open_rate = mean(open_rate, na.rm = TRUE), 
    unique_clicks = sum(unique_clicks, na.rm = TRUE),
    unique_CTR = mean(unique_CTR, na.rm = TRUE),
    UCTRvsAvg = mean(UCTRvsAvg, na.rm = TRUE),
    story_clicks = sum(story_clicks, na.rm = TRUE),
    story_COR = mean(story_COR, na.rm = TRUE),
    id = first(id),
    date = first(date),
    story_title = first(story_title),
    campaignID = first(campaignID)
  ) %>%
  select(id, name, date, delivered_, unique_opens, open_rate, unique_clicks,
         unique_CTR, UCTRvsAvg, story_url, story_title, story_clicks, story_COR, campaignID)

# tech & innovation utc situation for nycw
tech_innovation_newsletter <- campaignNewsletters %>%
  filter(startsWith(name, "2024-09-06: Tech + Innovation")) %>%
  group_by(name = "2024-09-06: Tech + Innovation") %>%  # Rename and aggregate
  summarise(
    delivered_ = sum(delivered_, na.rm = TRUE),
    unique_opens = sum(unique_opens, na.rm = TRUE),
    open_rate = mean(open_rate, na.rm = TRUE),
    unique_clicks = sum(unique_clicks, na.rm = TRUE),
    unique_CTR = mean(unique_CTR, na.rm = TRUE),
    UCTRvsAvg = mean(UCTRvsAvg, na.rm = TRUE),
    story_clicks = sum(story_clicks, na.rm = TRUE),
    story_COR = mean(story_COR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    id = 1777885594,
    date = as.Date("2024-09-06"),
    story_url = "https://rmi.org/rmi-at-climate-week/",
    story_title = "RMI at Climate Week 2024",
    campaignID = "nycw24"
  ) %>%
  select(id, name, date, delivered_, unique_opens, open_rate, unique_clicks,
         unique_CTR, UCTRvsAvg, story_url, story_title, story_clicks, story_COR, campaignID)

#for oci
tech_innovation_newsletter2 <- campaignNewsletters %>%
  filter(startsWith(name, "2023-07-31:")) %>%
  group_by(name = "2023-07-31:") %>%  # Rename and aggregate
  summarise(
    name = "2023-07-31: Tech & Innovation Newsletter",
    delivered_ = sum(delivered_, na.rm = TRUE),
    unique_opens = sum(unique_opens, na.rm = TRUE),
    open_rate = mean(open_rate, na.rm = TRUE),
    unique_clicks = sum(unique_clicks, na.rm = TRUE),
    unique_CTR = mean(unique_CTR, na.rm = TRUE),
    UCTRvsAvg = mean(UCTRvsAvg, na.rm = TRUE),
    story_clicks = sum(story_clicks, na.rm = TRUE),
    story_COR = mean(story_COR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    id = 1436379856,
    date = as.Date("2023-07-31"),
    story_url = "https://rmi.org/clean-energy-101-methane-detecting-satellites/",
    story_title = "Methane-Detecting Satellites 101: The Completeness Quotient",
    campaignID = "oci+"
  ) %>%
  select(id, name, date, delivered_, unique_opens, open_rate, unique_clicks,
         unique_CTR, UCTRvsAvg, story_url, story_title, story_clicks, story_COR, campaignID)

campaignNewsletters <- campaignNewsletters %>%
  filter(!startsWith(name, "2024-09-06: Tech + Innovation")) %>%
  filter(!startsWith(name, "2023-07-31: Tech & Innovation")) %>%
  bind_rows(tech_innovation_newsletter) %>%
  bind_rows(tech_innovation_newsletter2)

#mutate(name = if_else(name == "2023-07-31:", "2023-07-31: Techno & Innovation", name))



#' Set hasEmail to FALSE if no emails detected
if(nrow(campaignNewsletters) == 0) hasEmail <- FALSE else hasEmail <- TRUE

#debugging
# campaignNewsletters2 <- campaignNewsletters %>% distinct()
# print(storyLinks)
# print(clicksAll)

#if(hasEmail == TRUE){
  #' push data
 # message('PUSHING NEWSLETTER DATA')
  
 # ALL_NEWSLETTERS <- pushData(campaignNewsletters, 'Newsletters')
#}



#' #### PARDOT-WEBSITE CONNECTION ####
#' message('Bridging PARDOT and WEBSITE')
#' 
#' # NEW process for linking salesforce and pardot records 
#' 
#' # From the Form object (Pardot API v5), get all form embedCode values.
#' # From those embedCodes, isolate the src value
#' # ex. src=\"https://info.rmi.org/l/310101/2017-01-31/2d\"
#' # You can accomplish this with grepl or substring, isolating everything after "src=\" and before the following \" 
#' # From rmi.org, get pageURL, campaignName (from tags) and the iframe src
#' # The Python code below demonstrates how to accomplish this. I did this in Python so that you can decide how to adapt to R.
#' # In brief, you need to find the divs with id = download-form in the case of reports. You'll need to inspect event registration pages to figure out what the equivalent id is for events.
#' # For each download-form div, we want to get the data-form value, which will be the iframe src value.
#' # Then you can join form data to page data by iframe src.
#' 
#' 
#' #' Pardot: API call to get embedCode
#' #' 
#' #' 
#' pardot_domain <- "pi.pardot.com"
#' 
#' form_endpoint <- paste0("https://", pardot_domain, "/api/v5/objects/forms?fields=embedCode") 
#' 
#' pardotTokenV5 <- Sys.getenv("Pardot_TokenV5")
#' pardotBusinessID <- Sys.getenv("Pardot_Business_ID")
#' header5 <- c("Authorization" = pardotTokenV5, "Pardot-Business-Unit-Id" = pardotBusinessID)
#' 
#' 
#' 
#' # form_response <- GET(form_endpoint, add_headers(.headers = header5))
#' # 
#' # if (form_response$status_code == 200) {
#' #   # Parse as text 
#' #   form_content <- content(form_response, "text", encoding = "UTF-8")
#' #   parsed_content <- fromJSON(form_content)
#' #   
#' #   #might need to edit regex
#' #   embedCodeExtract <- str_extract_all(parsed_content, "https?://[^\\s]+") %>% unlist()
#' #   
#' #   listEmbedCode <- data.frame(embedCode = embedCodeExtract, stringsAsFactors = FALSE)
#' #   print(listEmbedCode)
#' # } else {
#' #   stop("Request failed with status code: ", form_response$status_code)
#' # }
#' #   
#' 
#' apiCall <- GET(paste0("https://pi.pardot.com/api/v5/objects/forms?fields=id,name,campaignId,embedCode,salesforceId,redirectLocation"),
#'                add_headers(.headers = header5))
#' formJson <- jsonlite::fromJSON(content(apiCall, as = "text", encoding = "UTF-8"))
#' 
#' # create empty dataframe to load into  
#' forms_all = data.frame()
#' 
#' for(i in 1:20){
#'   if(i == 1){
#'     # this sends the call for the first page of 200 results
#'     apiCall <- GET(paste0("https://pi.pardot.com/api/v5/objects/forms?fields=id,name,campaignId,embedCode,salesforceId,redirectLocation"),
#'                    add_headers(.headers = header5))
#'     # This formats the response into a json object
#'     formJson <- jsonlite::fromJSON(content(apiCall, as = "text", encoding = "UTF-8"))
#'     # This gets the nextPageUrl from the json object, which will catch the next page of results
#'     nextPage <- formJson[["nextPageUrl"]]
#'     # This gets the first page of results
#'     forms_df <- formJson[["values"]]
#'     forms_all <- forms_all %>%
#'       bind_rows(forms_df)
#'   } else {
#'     apiCall <- GET(nextPage, add_headers(.headers = header5))
#'     # This formats the response into a json object
#'     formJson <- jsonlite::fromJSON(content(apiCall, as = "text", encoding = "UTF-8"))
#'     # This gets the nextPageUrl from the json object, which will catch the next page of results
#'     nextPage <- formJson[["nextPageUrl"]]
#'     # This gets the first page of results
#'     forms_df <- formJson[["values"]]
#'     # This binds the results to the previous results
#'     forms_all <- forms_all %>%
#'       bind_rows(forms_df)
#'     if (nrow(forms_df) < 200) break
#'   }
#' }
#' 
#' embedCodeDF <- forms_all %>%
#'   mutate(embedCode_src = str_extract(embedCode, 'src=\\"(.*?)\\"') %>% gsub('src=\\"(.*?)\\"', '\\1', .)) %>%
#'   select(id, campaignId, embedCode, embedCode_src, name, redirectLocation, salesforceId)
#' 
#' 
#' # From rmi.org, get pageURL, campaignName (from tags) and the iframe src
#' # 
#' # url1 <- "https://rmi.org/insight/the-rural-equitable-climate-transition-toward-carbon-neutrality-and-shared-prosperity/"
#' # url2 <- "https://rmi.org/insight/the-value-of-green-hydrogen-trade-for-europe/"
#' # url3 <- "https://rmi.org/insight/prospects-for-chinas-next-generation-clean-low-carbon-technologies/"
#' 
#' # Need to add event page html
#' 
#' scrape_divID <- function(url){ 
#'   # read page's html content
#'   page <- read_html(url)
#'   
#'   div_value <- page %>%
#'     html_nodes("div#download-form")
#'   
#'   if (length(div_value) == 0) {
#'     return(NA)
#'   }
#'   
#'   dataform_value <- div_value %>%
#'     html_attr("data-form")
#'   
#'   return(dataform_value)
#' }
#' 
#' website_iframesrc <- filtered_urls %>%
#'   rowwise() %>%
#'   mutate(iframe_src = list(scrape_divID(pagePath))) 
#' 
#' website_iframesrc <- website_iframesrc %>%
#'   mutate(iframe_src = sapply(iframe_src, as.character))
#' 
#' # Join form data to page data by iframe src.
#' 
#' joint_report_page_data <- embedCodeDF %>%
#'   left_join(website_iframesrc, by = c("embedCode_src" = "iframe_src")) %>%
#'   filter(!is.na(pagePath)) %>%
#'   select(id, campaignId, pagePath, campaign_tag, embedCode_src, name, redirectLocation, salesforceId) %>%
#'   rename(Name = name)
#' 
#' #' ### SALESFORCE 2.0###
#' #' # Use the campaign ID from the pardot data to query all related Salesforce campaign records.
#' #' 
#' #' #' create dataframe for campaigns
#' #' df <- as.data.frame(matrix(0, ncol = 19, nrow = 0))
#' #' names(df) <- c('CampaignName', 'EngagementType', 'Id', 'RecordType', 'Status', 'EngagementDate', 
#' #'                'Name', 'Email', 'Domain', 'Account', 'AccountType', 'Industry', 'TotalGiving', 
#' #'                'NumOpenOpps', 'Pardot_Score', 'Pardot_URL', 'Giving_Circle', 'Last_Gift', 'AccountId') 
#' #'   
#' #' #' get list of campaigns
#' #' campaignList <- getCampaignList()
#' #'   
#' #' # filter campaignList based on pardot forms we care about
#' #' campaignListFiltered <- campaignList %>%
#' #'   filter(Name %in% joint_form_page_data$name)
#' #' 
#' #' 
#' #'   #' get all accounts
#' #' if(!exists('all_accounts')){
#' #'     all_accounts <- getAllAccounts() 
#' #'     }
#' #'   
#' #' #' remove accounts with duplicate names to avoid errors when joining by Account name
#' #' accountsUnique <- all_accounts[!duplicated(all_accounts$Account) & !duplicated(all_accounts$Account, fromLast = TRUE),] %>% 
#' #'   filter(!grepl('unknown|not provided|contacts created by revenue grid', Account))
#' #'   
#' #' ## get domain info for gov accounts
#' #' govDomains <- read.xlsx('audiences/govDomains.xlsx') 
#' #'   
#' #' ## get audience domains and accounts
#' #' audienceGroups <- read.xlsx('audiences/audienceGroups.xlsx')
#' #' audienceAccounts <- audienceGroups %>% select(Account, type) %>% filter(!is.na(Account)) %>% distinct(Account, .keep_all = TRUE)
#' #' audienceDomains <- audienceGroups %>% select(Domain, type) %>% filter(!is.na(Domain)) %>% distinct(Domain, .keep_all = TRUE)
#' #'   
#' #' 
#' #' #' get campaign member data for reports, events, and newsletter clicks **this part is important
#' #' 
#' #'   
#' #' for (campaign in campaignList$Name) {
#' #'   
#' #'   campaignMembersReports <- getSalesforceReports2()
#' #'   df <- df %>% rbind(campaignMembersReports)
#' #' }
#' #'   
#' #' # TRYING AGAIN 9/27
#' #'   
#' #' # nameOfInterest <- joint_report_page_data$Name %>%
#' #' #   as.data.frame() %>%
#' #' #   rename('Name'= '.')
#' #' 
#' #' nameOfInterest <- joint_report_page_data %>%
#' #'   pull(Name) %>%
#' #'   as.character()
#' #' nameOfInterest <- nameOfInterest[nameOfInterest != ""]
#' #' nameOfInterest <- unique(nameOfInterest)
#' #' 
#' #' 
#' #' print(class(nameOfInterest))  # Should return "character"
#' #' 
#' #'   
#' #' hasNameOfInterest <- length(reportName) != 0
#' #' 
#' #'   
#' #' if(hasNameOfInterest == TRUE | hasEmail == TRUE){
#' #'     
#' #'     #' create dataframe for campaigns
#' #'     df <- as.data.frame(matrix(0, ncol = 19, nrow = 0))
#' #'     names(df) <- c('CampaignName', 'EngagementType', 'Id', 'RecordType', 'Status', 'EngagementDate', 
#' #'                    'Name', 'Email', 'Domain', 'Account', 'AccountType', 'Industry', 'TotalGiving', 
#' #'                    'NumOpenOpps', 'Pardot_Score', 'Pardot_URL', 'Giving_Circle', 'Last_Gift', 'AccountId') 
#' #'     
#' #'     #' get list of campaigns
#' #'     campaignList <- getCampaignList(nameOfInterest)
#' #'     
#' #'     #' get all accounts
#' #'     if(!exists('all_accounts')){
#' #'       all_accounts <- getAllAccounts() 
#' #'     }
#' #'     
#' #'     #' remove accounts with duplicate names to avoid errors when joining by Account name
#' #'     accountsUnique <- all_accounts[!duplicated(all_accounts$Account) & !duplicated(all_accounts$Account, fromLast = TRUE),] %>% 
#' #'       filter(!grepl('unknown|not provided|contacts created by revenue grid', Account))
#' #'     
#' #'     ## get domain info for gov accounts
#' #'     govDomains <- read.xlsx('audiences/govDomains.xlsx') 
#' #'     
#' #'     ## get audience domains and accounts
#' #'     audienceGroups <- read.xlsx('audiences/audienceGroups.xlsx')
#' #'     audienceAccounts <- audienceGroups %>% select(Account, type) %>% filter(!is.na(Account)) %>% distinct(Account, .keep_all = TRUE)
#' #'     audienceDomains <- audienceGroups %>% select(Domain, type) %>% filter(!is.na(Domain)) %>% distinct(Domain, .keep_all = TRUE)
#' #'     
#' #'     #' get campaign member data for reports, events, and newsletter clicks **this part is important
#' #'     if(hasNameOfInterest == TRUE){
#' #'       campaignMembersReports <- getSalesforceReports2(nameOfInterest$Name)
#' #'       df <- df %>% rbind(campaignMembersReports)
#' #'     } 
#' #' }
#' #'     
#' #'     # if(hasEvent == TRUE) {
#' #'     #   campaignMembersEvents <- getSalesforceEvents()
#' #'     #   df <- df %>% rbind(campaignMembersEvents)
#' #'     # } 
#' #'     # COME BACK TO LATER
#' #'     
#' #'     
#' #'     if(hasEmail == TRUE) {
#' #'       #' get all prospects
#' #'       if(!exists('prospects')){
#' #'         prospects <- getProspects() %>% 
#' #'           mutate(Domain = sub("(.*)\\@", "", Email))
#' #'       }
#' #'       
#' #'       campaignMembersNewsletters <- getCampaignNewsletters()
#' #'       df <- df %>% rbind(campaignMembersNewsletters)
#' #'     }
#' #'     
#' #'     #' remove duplicates
#' #'     SFcampaigns <- df %>% distinct(Id, CampaignName, .keep_all = TRUE)
#' #'     
#' #'     #' get donation revenue attributed to these campaign components
#' #'     donations <- getAttributedDonationValue(SFcampaigns)
#' #'     
#' #'     #' bind donations to SF campaign data
#' #'     donationsByCampaignMember <- donations %>% 
#' #'       group_by(Id, CampaignName) %>% 
#' #'       summarize(AttributtedDonationValue = sum(AttributtedValue))
#' #'     
#' #'     SFcampaigns <- SFcampaigns %>% 
#' #'       left_join(donationsByCampaignMember) %>% 
#' #'       select(CampaignName, EngagementType, Icon, Id, Status, EngagementDate, Domain, Email, 
#' #'              DonorType, AttributtedDonationValue, AccountId, Account, AccountType, Audience1, Audience2, Industry, 
#' #'              Pardot_URL, Pardot_ID, GivingCircleTF, SolutionsCouncilTF, InnovatorsCircleTF, OpenOppTF, DonorTF, 
#' #'              LapsedDonorsTF, DownloadTF, EventTF, EmailClickTF, Engagements) %>%
#' #'       mutate(CampaignName = case_when(EngagementType == 'Report Download' ~ 
#' #'                                         substring(CampaignName, 11),
#' #'                                       EngagementType == 'Event' ~ 
#' #'                                         substring(CampaignName, 11),
#' #'                                       TRUE ~ CampaignName),
#' #'              CampaignName = trimws(CampaignName))
#' #'   }
#' #'   
#'   
#' 
#' # END of new process


#### SALESFORCE #### will need to amend process to include new reportids and eventids.
message('GETTING SALESFORCE DATA')

#' SUMMARY
#' 
#' 1a Pull Salesforce campaign IDs for reports provided in campaign key
#'    Get affiliated campaign members and binds data from Salesforce Contacts/Leads/Accounts query
#' 1b Pull Salesforce campaign IDs for events provided in campaign key
#'    Get affiliated campaign members and binds data from Salesforce Contacts/Leads/Accounts query
#' 1c Pull Pardot list email IDs from the campaignNewsletters dataframe generated by the getCampaignNewsletters function
#'    Get all link clicks for newsletters from Pardot
#'    Get all prospects - contacts and leads with pardot activity - using getProspects function
#'    Joins dataframes using Pardot ID from the Pardot URL field to get contact/lead information
#' 2. Cleans data and joins account data for unknown contacts/leads using email domains 
#'    Categorizes audience groups using the govDomains file and audienceGroups file
#' 3. Bind all dataframes together
#' 4. Get donations - view process summary in Donations section
#' 5. Write dataset
#' 

if(hasReport == TRUE | hasEvent == TRUE | hasEmail == TRUE){
  
  #' create dataframe for campaigns
  # df <- as.data.frame(matrix(0, ncol = 23, nrow = 0))
  # names(df) <- c('CampaignName', 'EngagementType', 'Id', 'RecordType', 'Status', 'EngagementDate', 
  #                'Name', 'Email', 'Domain', 'Account', 'AccountType', 'Industry', 'TotalGiving', 
  #                'NumOpenOpps', 'Pardot_Score', 'Pardot_URL', 'Giving_Circle', 'Last_Gift', 'AccountId',
  #                'Account_Constituent_Groups__c', 'Account_Constituent_Sub_Groups__c', 'reportID', 'campaignID') 
  # 
  
  df <- as.data.frame(matrix(0, ncol = 34, nrow = 0))
  names(df) <- c('CampaignName', 'EngagementType', 'Icon', 'Id', 'Status', 'EngagementDate', 'Domain',
                 'Email', 'DonorType', 'AccountId', 'Account', 'AccountType', 'Audience1', 'Audience2', 'Industry', 'TotalGiving', 
                 'Name', 'Pardot_Score', 'Pardot_URL', 'Pardot_ID', 'GivingCircleTF','SolutionsCouncilTF', 'InnovatorsCircleTF',
                 'OpenOppTF', 'DonorTF', 'LapsedDonorTF', 'DownloadTF', 'EventTF', 'EmailClickTF', 'Engagements', 'Account_Constituent_Groups__c',
                 'Account_Constituent_Sub_Groups__c', 'identifyingID', 'campaignID') 
  #will need tojust be ID, change names
  
  #' get list of campaigns
  campaignList <- getCampaignList()
  
  #' get all accounts
  if(!exists('all_accounts')){
    all_accounts <- getAllAccounts() 
  }
  
  #' remove accounts with duplicate names to avoid errors when joining by Account name
  accountsUnique <- all_accounts[!duplicated(all_accounts$Account) & !duplicated(all_accounts$Account, fromLast = TRUE),] %>% 
    filter(!grepl('unknown|not provided|contacts created by revenue grid', Account))
  
  ## get domain info for gov accounts
  govDomains <- read.xlsx('audiences/govDomains.xlsx') 
  
  ## get audience domains and accounts
  audienceGroups <- read.xlsx('audiences/audienceGroups.xlsx')
  audienceAccounts <- audienceGroups %>% select(Account, type) %>% filter(!is.na(Account)) %>% distinct(Account, .keep_all = TRUE)
  audienceDomains <- audienceGroups %>% select(Domain, type) %>% filter(!is.na(Domain)) %>% distinct(Domain, .keep_all = TRUE)

  #' get campaign member data for reports, events, and newsletter clicks 
  if(hasReport == TRUE){
    campaignMembersReports <- getSalesforceReports()
    df <- df %>% rbind(campaignMembersReports)
  } 
  
  if(hasEvent == TRUE) {
    campaignMembersEvents <- getSalesforceEvents()
    df <- df %>% rbind(campaignMembersEvents)
  } 
  
  if(hasEmail == TRUE) {
    #' get all prospects - this might not run currently bc prospects object does exist
    if(!exists('prospects')){
      prospects <- getProspects() %>% 
        mutate(Domain = sub("(.*)\\@", "", Email))
    }
    
    campaignMembersNewsletters <- getCampaignNewsletters()
    df <- df %>% rbind(campaignMembersNewsletters)
  }
  #manual because this report name was NA
  df <- df %>%
    mutate(CampaignName = ifelse(is.na(CampaignName) & identifyingID == "7016f0000013gTPAAY", "Ensuring an Equitable Energy Transition", CampaignName))
  
  
  #' remove duplicates
  SFcampaigns <- df %>% distinct(Id, CampaignName, .keep_all = TRUE)
  
  idsFromSFcampaigns <- SFcampaigns %>%
    select(CampaignName, campaignID) %>%
    distinct(CampaignName, campaignID)

  #' get donation revenue attributed to these campaign components
  donations <- getAttributedDonationValue(SFcampaigns) 
  
  donations <- donations %>%
    left_join(idsFromSFcampaigns, by = "CampaignName")
    
  
  #' bind donations to SF campaign data
  donationsByCampaignMember <- donations %>% 
    group_by(Id, CampaignName) %>% 
    summarize(AttributtedDonationValue = sum(AttributtedValue))
  
  SFcampaigns <- SFcampaigns %>% 
    left_join(donationsByCampaignMember)%>% 
    select(CampaignName, EngagementType, Icon, Id, Status, EngagementDate, Domain, Email, 
           DonorType, AttributtedDonationValue, AccountId, Account, AccountType, Audience1, Audience2, Industry, 
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c, Pardot_URL, Pardot_ID, GivingCircleTF, SolutionsCouncilTF, InnovatorsCircleTF, OpenOppTF, DonorTF, 
           LapsedDonorsTF, DownloadTF, EventTF, EmailClickTF, Engagements, campaignID) %>%
    mutate(CampaignName = case_when(EngagementType == 'Report Download' ~ 
                                      substring(CampaignName, 11),
                                    EngagementType == 'Event' ~ 
                                      substring(CampaignName, 11),
                                    TRUE ~ CampaignName),
           CampaignName = trimws(CampaignName))%>%
     separate(Account_Constituent_Groups__c, into = c("constituent_group_1", "constituent_group_2", "constituent_group_3", "constituent_group_4"), sep = ";", fill = "right") %>%
     separate(Account_Constituent_Sub_Groups__c, into = c("constituent_subgroup_1", "constituent_subgroup_2", "constituent_subgroup_3", "constituent_subgroup_4"), sep = ";", fill = "right") %>%
    mutate(across(starts_with("constituent_subgroup_"), 
                  ~ case_when(
                    . %in% c("Investor-Owned Utility", "Public Utility", "Co-op Utility") ~ paste("Electric Utility/Distribution Company -", .),
                    . %in% c("National Government", "State Government", "International Government",
                             "Regional/Municipal Government", "Public Utilities Commission") ~ paste("Government/Policymaker -", .),
                    . %in% c("Fortune 1000", "Technology", "Consulting", "Heavy Industry") ~ paste("Corporate -", .),
                    . %in% c("Bank", "Insurance Company", "Investor", "US Mid-Size Regional Bank") ~ paste("Financial Entity -", .),
                    . %in% c("Corporate Funder", "Foundation", "Household Funder") ~ paste("Funder -", .),
                    TRUE ~ . # Leave other values unchanged
                  )
    ))

}

#manual
SFcampaigns <- SFcampaigns %>%
  mutate(CampaignName = if_else(CampaignName == "n Equitable Energy Transition", 
                                "Ensuring an Equitable Energy Transition", 
                                CampaignName))
  
# print(digit)  # Check what 'digit' contains
# if (is.null(digit) || length(digit) == 0) {
#   message("digit is NULL or has length zero")
# }


#' #glenn querying
#' my_soql <- sprintf("SELECT Id, Name, ParentId, Type, Status, StartDate, EndDate, Description
#'                     FROM Campaign 
#'                     WHERE StartDate >= 2024-09-01")
#' 
#' campaignItemQuery <- sf_query(my_soql, 'Campaign', api_type = "Bulk 1.0")
#' 
#' itemIdList <- campaignItemQuery$Id
#' 
#' #' get campaign members that match any of the campaign item IDs
#' my_soql <- sprintf("SELECT CampaignId,
#'                              Name,
#'                              Status,
#'                              HasResponded,
#'                              ContactId,
#'                              LeadId,
#'                              CreatedDate
#' 
#'                       FROM CampaignMember
#'                       WHERE CampaignId in ('%s')",
#'                    paste0(itemIdList, collapse = "','"))
#' 
#' campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0")
#' 
#' #end querying


#### SOCIAL MEDIA ####
message('GETTING SOCIAL MEDIA DATA')

#' SUMMARY
#' 
#' 1. Get all posts made after Jan 1, 2023 with tags from all social channels except for program LinkedIn accounts
#' 2. Request does not return the post account/profile ID so use post links - perma_link - to identify the account
#     NOTE: This works for all platforms except LinkedIn, which doesn't include account info in post URLs
#' 3. Get all posts for program Linkedin accounts, add account identifier, then bind all of these to posts retrieved in step 1
#' 4. Get averages for all get posts made over the last year, left join these values to all posts df
#' 6. Filter by campaign tag to get campaign posts
#' 7. Write dataset

#' get all sprout social tags
metadeta <- getMetadata(url = 'metadata/customer/tags')
tags <- metadeta[["data"]]

#' get post averages based on all posts made over the last year 
posts1YRaverages <- getPostAverages()

#' get all social media posts with tags except for posts made from program LinkedIn accounts
allPostsTags <- getAllSocialPosts()

#' get all posts from linkedIn program channels
linkedInTagged <- getLIProgramPosts('tagged')

#' clean and bind linkedin program posts to all posts
taggedPosts <- cleanPostDF(allPostsTags, 'tagged') %>% 
  rbind(linkedInTagged) %>% 
  left_join(posts1YRaverages, by = c('post_type', 'account')) %>% 
  #' calculate post performance compared to avgs.
  mutate(impressionsVmedian = round((impressions - impressionsMedian)/impressionsMedian, 3),
         engagementsVmedian = round((engagements - engagementsMedian)/engagementsMedian, 3),
         engrtVmedian = round((engagementRate - engrtMedian)/engrtMedian, 3),
         impressionsVmean = round((impressions - impressionsMean)/impressionsMean, 3),
         engagementsVmean = round((engagements - engagementsMean)/engagementsMean, 3),
         engrtVmean = round((engagementRate - engrtMean)/engrtMean, 3),
         brand = ifelse(grepl('RMI Brand', account), 1, 0),
         program = ifelse(brand == 1, 0, 1),
         accountType = ifelse(brand == 1, 'RMI Brand', 'RMI Program'),
         post = 'Post') %>% 
  select(created_time, account, post_type, icon, tag_id, tag_name, 
         impressions, impressionsVmedian, impressionsVmean, engagements, engagementsVmedian, engagementsVmean, 
         engagementRate, engrtVmedian, engrtVmean, postClicks, shares, 
         brand, program, accountType, post, perma_link, text)
#' 
#' #' find tagged posts for this campaign
#' if ('oci+' %in% campaign) {
#'   #' OCI posts were not tagged properly so get these by creating a custom filter
#'   campaignPosts <- taggedPosts %>% 
#'     filter(created_time >= '2023-04-05' & grepl('OCI\\+', text)) %>% 
#'     distinct(perma_link, .keep_all = TRUE) 
#' } else {
#'   campaignPosts <- taggedPosts %>% 
#'     filter(tolower(tag_name) %in% tolower(socialTag)) %>% 
#'     distinct(perma_link, .keep_all = TRUE)  #CHECK IF POPULATED W OCI
#' }
#' 
#' 

pivotTaggedPosts <- taggedPosts %>%
  distinct(perma_link, tag_name) %>%
  group_by(perma_link) %>%
  mutate(tag_num = paste0("tag", row_number())) %>%
  ungroup() %>%
  pivot_wider(
    names_from = tag_num,  # Each unique tag position becomes a new column
    values_from = tag_name # Fill these columns with the tag_name values
  )

#redefining socialTag for transition-narrative
socialTag <- unique(campaignPages$campaign_tag[!is.na(campaignPages$campaign_tag)])

socialTag <- c(socialTag, "[1] 17. Energy Transition Narrative [transition-narrative]")


campaignPosts <- taggedPosts %>% 
  mutate(tag_name = ifelse(grepl('OCI\\+', text) & created_time >= '2023-04-05', 'OCI+', tag_name)) %>%
  filter(tolower(tag_name) %in% tolower(socialTag)) %>% 
  distinct(perma_link, .keep_all = TRUE) %>%
  left_join(pivotTaggedPosts, by = "perma_link")

# social_taggingKey <- read.csv('Tags_2024-10-15(in).csv')
# 
# campaignPosts2 <- campaignPosts %>%
#   left_join(social_taggingKey, by = c("tag_id" = "Id"))
# 


####' Monday.com ####
message('GETTING MONDAY.COM DATA')

#' SUMMARY
#' 
#' 1. Get all projects from Active Project Board
#' 2. Iterate through this board to find projects with "Metrics Dashboard" in the promotion tactics column
#' 3. Retrieve ID, audiences, and promotion tactics from these projects
#' 4. Filter for project that contains campaign ID
#' 5. Write data set
#' 


#' get Active Projects Board
#query <- "query { boards (ids: 2208962537) { items { id name column_values{ id value text } } } } "
##query <- "query { boards (ids: 2208962537) { items_page(limit: 500) { cursor items { id name column_values{ id value text } } } } } "

# completed board id: 4217437668


# Working to update API query to only get projects with "Metrics Dashboard" in the promotion tactics column direct from Monday
# query <- "query { items_page_by_column_values (limit: 500, board_id: 2208962537, columns: [{column_id: \"dropdown\", column_values: [\"Metrics Dashboard\"]}]) { cursor items { id  name  }  } } "
# activeProjectsID<- as.data.frame(res[["data"]][["items_page_by_column_values"]][["items"]][[1]])
# activeProjectsName <- as.data.frame(res[["data"]][["items_page_by_column_values"]][["items"]][[2]])
# projects <- cbind(activeProjectsID, activeProjectsName)
# names(projects) <- c('id', 'name')


##res <- getMondayCall(query)

##activeProjects <- as.data.frame(res[["data"]][["boards"]][["items_page"]][["items"]][[1]])



#' loop through Active Projects Board to find projects with "Metrics Dashboard" in the promotion tactics column
##projects <- data.frame(id = '', row = '', name = '')[0,]

##for(i in 1:nrow(activeProjects)){

  ##board <- activeProjects[[3]][[i]]
    ##if(grepl('Metrics Dashboard', paste(board[11, 'text']))){
    ##projects <- projects %>%
      ##rbind(c(paste(activeProjects[i, 'id']), i, c(paste(activeProjects[i, 'name']))))
     ##}
## }

##names(projects) <- c('id', 'row', 'name')


# Pulled this out from the loop, before it was included in the following loop and would be overwritten each time
##metricsDashboardCampaigns <- data.frame(CAMPAIGN_ID = '', name = '', ID = '', audiences = '',metrics = '', promotionTactics = '')[0,]

##row.names(campaignBoard) <- campaignBoard$id

##for(i in 1:nrow(projects)){
  

  #' get promotion tactics, audiences, and ID
 ## campaignRow <- as.numeric(projects[i, 'row']) # Changed this from 1 to i to get the correct row, before it was just looking at the first row
  ##campaignBoard <- activeProjects[[3]][[campaignRow]]
  ##campaignDF <- data.frame(CAMPAIGN_ID = campaignID,
           ##                name = projects[i, 'name'],
             ##              ID = campaignBoard[16, 'text'], 
                           # Assign value from text column to ID where id column equals text1
                         #  ID = campaignBoard['text1', 'text'],
                           
              ##             audiences = campaignBoard[15, 'text'], 
                          # audiences = campaignBoard['dropdown7', 'text'], 
                      
                  ##         metrics = campaignBoard[14, 'text'],
                          # metrics = campaignBoard['dropdown0', 'text'],
                      
                    ##       promotionTactics = campaignBoard[11, 'text']) 
  
##  metricsDashboardCampaigns <- metricsDashboardCampaigns %>% rbind(campaignDF)
  
##}

##metricsDashboardCampaigns <- metricsDashboardCampaigns %>% 
  ##mutate(promotionTactics = gsub(', Metrics Dashboard', '', promotionTactics))


##targetCampaign <- metricsDashboardCampaigns %>%
  ##filter(ID == campaignID)




### Looping testing!!!

#query1 <- "query { boards (ids: 2208962537) { items_page(limit: 500) { cursor items { id name column_values{ id value text } } } } }"
#query2 <- "query { boards (ids: 7114066583) { items_page(limit: 500) { cursor items { id name column_values{ id value text } } } } } "

#res1 <- getMondayCall(query1)
#res2 <- getMondayCall(query2)

#activeProjects <- as.data.frame(res1[["data"]][["boards"]][["items_page"]][["items"]][[1]])
#completedProjects <- as.data.frame(res2[["data"]][["boards"]][["items_page"]][["items"]][[1]])

# combining 
#allProjects <- rbind(activeProjects, completedProjects)

# print(str(allProjects))


# loop through Active Projects Board to find projects with "Metrics Dashboard" in the promotion tactics column
# projects had 0 observations?*!
#projects <- data.frame(id = '', row = '', name = '')[0,]

#for(i in 1:nrow(allProjects)){
 # board <- allProjects[[3]][[i]]
  #if(grepl('Metrics Dashboard', paste(board[11, 'text']))){
  #if(grepl('Article', paste(board[11, 'text']))){
   # projects <- projects %>%
    #  rbind(c(paste(allProjects[i, 'id']), i, c(paste(allProjects[i, 'name']))))
  #}
#}

#names(projects) <- c('id', 'row', 'name')

# Initialize the dataframe for metrics dashboard campaigns
#metricsDashboardCampaigns <- data.frame(CAMPAIGN_ID = '', name = '', ID = '', audiences = '', metrics = '', promotionTactics = '')[0,]

#row.names(campaignBoard) <- campaignBoard$id

#for(i in 1:nrow(projects)){
  
  #' get promotion tactics, audiences, and ID
 # campaignRow <- as.numeric(projects[i, 'row']) # Changed this from 1 to i to get the correct row, before it was just looking at the first row
  #campaignBoard <- allProjects[[3]][[campaignRow]]
  #campaignDF <- data.frame(CAMPAIGN_ID = campaignID,
   #                        name = projects[i, 'name'],
    #                       ID = campaignBoard[16, 'text'], 
                           # Assign value from text column to ID where id column equals text1
                           #  ID = campaignBoard['text1', 'text'],
                           
     #                      audiences = campaignBoard[15, 'text'], 
                           # audiences = campaignBoard['dropdown7', 'text'], 
                           
 #                          metrics = campaignBoard[14, 'text'],
                           # metrics = campaignBoard['dropdown0', 'text'],
                           
  #                         promotionTactics = campaignBoard[11, 'text']) 
  
#  metricsDashboardCampaigns <- metricsDashboardCampaigns %>% rbind(campaignDF)
  
#}


#metricsDashboardCampaigns <- metricsDashboardCampaigns %>% 
 # mutate(promotionTactics = gsub(', Metrics Dashboard', '', promotionTactics))


#targetCampaign <- metricsDashboardCampaigns %>%
#  filter(ID == campaignID)


### END

## looping with new consolidated Monday board!
# 7199415791

query <- "query { boards (ids: 7199415791) { items_page(limit: 500) { cursor items { id name column_values{ id value text } } } } } "

# completed board id: 4217437668


# Working to update API query to only get projects with "Metrics Dashboard" in the promotion tactics column direct from Monday
# query <- "query { items_page_by_column_values (limit: 500, board_id: 2208962537, columns: [{column_id: \"dropdown\", column_values: [\"Metrics Dashboard\"]}]) { cursor items { id  name  }  } } "
# activeProjectsID<- as.data.frame(res[["data"]][["items_page_by_column_values"]][["items"]][[1]])
# activeProjectsName <- as.data.frame(res[["data"]][["items_page_by_column_values"]][["items"]][[2]])
# projects <- cbind(activeProjectsID, activeProjectsName)
# names(projects) <- c('id', 'name')


res <- getMondayCall(query)

#maybe delete items page?  delete 
activeProjects <- as.data.frame(res[["data"]][["boards"]][["items_page"]][["items"]][[1]])


#' loop through Active Projects Board to find projects with "Metrics Dashboard" in the promotion tactics column
#projects <- data.frame(id = '', row = '', name = '')[0,]

# for(i in 1:nrow(activeProjects)){
  
 # board <- activeProjects[[3]][[i]]
#  if(grepl('Metrics Dashboard', paste(board[11, 'text']))){
   # projects <- projects %>%
  #    rbind(c(paste(activeProjects[i, 'id']), i, c(paste(activeProjects[i, 'name']))))
 # }
#}

#names(projects) <- c('id', 'row', 'name')

#removepromo tactics
# Pulled this out from the loop, before it was included in the following loop and would be overwritten each time
metricsDashboardCampaigns <- data.frame(campaignID = '', name = '', ID = '', audiences = '',metrics = '', promotionTactics = '')[0,]

#row.names(campaignBoard) <- campaignBoard$id

for(i in 1:nrow(activeProjects)){
  #' get promotion tactics, audiences, and ID
  #campaignRow <- as.numeric(activeProjects[i, 'row']) # Changed this from 1 to i to get the correct row, before it was just looking at the first row
  campaignBoard <- activeProjects[[3]][[i]]
  campaignDF <- data.frame(campaignID = campaignBoard[5,'text'],
                           name = activeProjects[i, 'name'],
                           ID = activeProjects[i, 'id'], 
                           # Assign value from text column to ID where id column equals text1
                           #  ID = campaignBoard['text1', 'text'],
                           audiences = campaignBoard[3, 'text'], 
                           # audiences = campaignBoard['dropdown7', 'text'], 
                           metrics = campaignBoard[4, 'text'])
  # metrics = campaignBoard['dropdown0', 'text'],
  #  promotionTactics = campaignBoard[11, 'text']) 
  metricsDashboardCampaigns <- metricsDashboardCampaigns %>% rbind(campaignDF)
}

#metricsDashboardCampaigns <- metricsDashboardCampaigns %>% 
 # mutate(promotionTactics = gsub(', Metrics Dashboard', '', promotionTactics))


targetCampaign <- metricsDashboardCampaigns %>%
  filter(tolower(campaignID) %in% tolower(campaigns)) 



# end

#### CREATE CONTENT SUMMARY ####

#' SUMMARY
#' 
#' 1. Create content summary table for Campaign Summary tab on dashboard
#'    by getting instances of all social media posts, Salesforce campaigns - all reports, events, or newsletters - 
#'    and media referrals 
#' 2. Write dataset

socialContent <- campaignPosts %>% 
  mutate(type = 'Social Media Posts') %>% 
  select(type, name = post_type) 

#come back to, salesforce sf
salesforceContent <- SFcampaigns %>% 
  select(name = CampaignName, EngagementType) %>% 
  distinct() %>% 
  mutate(type = ifelse(EngagementType == 'Newsletter', 'Newsletters', 
                       ifelse(EngagementType == 'Report Download', 'Reports', 
                              ifelse(EngagementType == 'Event', 'Events', '')))) %>% 
  select(type, name)

mediaContent <- mediaReferrals %>% 
  mutate(type = 'Media Referrals') %>% 
  ungroup() %>% 
  select(type, name = mediaSubtype) 

#come back to, salesforce sf
contentSummary <- socialContent %>% 
  rbind(salesforceContent) %>% 
  rbind(mediaContent)


# Check to verify dataframes have more than 1 row, if not stop the script**

dfs <- list(allTraffic, socialTraffic, geographyTraffic, mediaReferrals,
            campaignNewsletters, SFcampaigns, donations, campaignPosts, targetCampaign,
            contentSummary)

dfs <- dfs %>% 
  set_names(c('allTraffic', 'socialTraffic', 'geographyTraffic', 'mediaReferrals', 
              'campaignNewsletters', 'SFcampaigns', 'donations', 'campaignPosts', 
              'targetCampaign', 'contentSummary'))


for (i in seq_along(dfs)) {
  if (nrow(dfs[[i]]) < 1) {
    # identify which data frame is empty by list item name
    stop('Empty dataframe: ', names(dfs)[i])
  }
}



# Delete all existing content for campaign from database
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_influence_campaign',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath(ssl)
)

tables <- dbListTables(con)

dbDisconnect(con)

# compare audience values from targetcampaign in database and data frame
# drop records already in database
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_influence_campaign',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath(ssl)
)


# converting to string
tags_str <- paste(shQuote(campaign_tags, type = "cmd"), collapse = ", ")
# 
# query <- paste0("SELECT audiences, metrics FROM targetcampaign WHERE campaign_tags IN (", tags_str, ")")

query <- paste0("SELECT audiences, metrics FROM targetcampaign WHERE campaignID IN (", tags_str, ")")

check_monday <- dbSendQuery(con, query) %>% dbFetch() %>% as.data.frame()


check_monday <- check_monday %>%
  mutate(check = paste(audiences, metrics))

targetCampaign <- targetCampaign %>% 
  mutate(check = paste(audiences, metrics)) %>%
  left_join(select(check_monday, check), by = 'check', keep = T) %>%
  filter(is.na(check.y)) %>%
  select(-c(check.x, check.y))


# tables <- tables %>%
#   str_replace("targetcampaign", "")


con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_influence_campaign',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath(ssl)
)
# 
# for (i in tables){
# query <- paste0("DELETE FROM ",i, " where campaignID ='",campaignID, "'")
# dbExecute(con, query)
# dbDisconnect(con)
# }

# v1, original
# for (i in tables){
#   tryCatch({
#     query <- paste0("DELETE FROM ", i, " WHERE campaignID = '", campaignID, "'")
#     dbSendQuery(con, query)
#   }, error = function(e) {
#     print(paste0("Error: ", e))
#   })
# 
# }

#v2 - might need to move below to where campaign id is assigned?
campaign_ids <- c("cop28", "2023-2025_coalvgas", "oci+", "nycw24", "OCI", "2023_CoalvGas", "transition-narrative")
campaign_ids <- paste0("'", campaign_ids, "'", collapse = ", ")

for (i in tables){
  tryCatch({
    #comma-separatedstring
#    query <- paste0("DELETE FROM ", i, " WHERE campaignID IN (", campaign_ids, ")")
    query <- paste0("DELETE FROM ", i, " WHERE campaignID IN (", campaign_ids, ") OR campaignID IS NULL OR campaignID = ''")
    
        dbSendQuery(con, query)
  }, error = function(e) {
    print(paste0("Error: ", e))
  })
  
}


#   for (i in tables){
#     tryCatch({
#       query <- paste0("DELETE FROM ", i, " WHERE campaignID = '", campaignID, "'")
# 
#       dbSendQuery(con, query)
#     }, error = function(e) {
#       print(paste0("Error: ", e))
#     })
# 
# }

#i="alltraffic"


#########################################################3
#used to be left as right

geographyTraffic <- geographyTraffic %>%
  rename(regionPageViews = "Region Page Views")


# Add campaignID to all dfs

# allTraffic$campaignID = campaignID
# socialTraffic$campaignID = campaignID
# geographyTraffic$campaignID = campaignID
# mediaReferrals$campaignID = campaignID
# campaignNewsletters$campaignID = campaignID
# SFcampaigns$campaignID = campaignID
# donations$campaignID = campaignID
# campaignPosts$campaignID = campaignID


allTraffic <- allTraffic %>%
  left_join(campaignPages %>%
    select(pageTitle, campaign_tag) %>%
    distinct(pageTitle, .keep_all = TRUE), 
    by = "pageTitle") %>%  
  rename(campaignID = campaign_tag)

socialTraffic <- socialTraffic %>%
  mutate(campaignID = dashboardCampaign)

geographyTraffic <- geographyTraffic %>%
  mutate(campaignID = dashboardCampaign)

mediaReferrals <- mediaReferrals %>%
  left_join(campaignPages %>%
              select(pageTitle, campaign_tag) %>%
              distinct(pageTitle, .keep_all = TRUE), 
            by = "pageTitle") %>%  
  rename(campaignID = campaign_tag)
  
campaignNewsletters <- campaignNewsletters

SFcampaigns <- SFcampaigns

# come back to this later
donations <- donations

campaignPosts <- campaignPosts %>%
  mutate(campaignID = tolower(tag_name)) %>%
  mutate(campaignID = ifelse(campaignID == "[1] 17. energy transition narrative [transition-narrative]", 
                             "transition-narrative", campaignID))

#campaignPosts$date_updated <- Sys.time() 

#fix
if (nrow(targetCampaign) > 0){

 # targetCampaign$campaignID = targetCampaign$campaignID
  targetCampaign <- select(targetCampaign, -c(campaignID, name))
  } else { }

#contentSummary$campaignID = campaignID

# Content summary
# end

#### CREATE NEW CONTENT SUMMARY ####

#' SUMMARY
#' 
#' 1. Create content summary table for Campaign Summary tab on dashboard
#'    by getting instances of all social media posts, Salesforce campaigns - all reports, events, or newsletters - 
#'    and media referrals 
#' 2. Write dataset

socialContent <- campaignPosts %>% 
  mutate(type = 'Social Media Posts') %>% 
  select(type, name = post_type, campaignID) 

#come back to, salesforce sf
salesforceContent <- SFcampaigns %>% 
  select(name = CampaignName, EngagementType, campaignID) %>% 
  distinct() %>% 
  mutate(type = ifelse(EngagementType == 'Newsletter', 'Newsletters', 
                       ifelse(EngagementType == 'Report Download', 'Reports', 
                              ifelse(EngagementType == 'Event', 'Events', '')))) %>% 
  select(type, name, campaignID)

mediaContent <- mediaReferrals %>% 
  mutate(type = 'Media Referrals') %>% 
  ungroup() %>% 
  select(type, name = mediaSubtype, campaignID) 

#come back to, salesforce sf
contentSummary <- socialContent %>% 
  rbind(salesforceContent) %>% 
  rbind(mediaContent)





# Write data to database

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_influence_campaign',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath(ssl)
)

# long for sf campaigns 
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_influence_campaign',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath(ssl),
  connectTimeout = 120,   # Connection timeout in seconds
  waitTimeout = 28800,    # Wait timeout in seconds (8 hours)
  interactiveTimeout = 28800  # Interactive timeout in seconds (8 hours)
)


dfs <- list("allTraffic" = allTraffic, "socialTraffic" = socialTraffic, "geographyTraffic" = geographyTraffic, 
            "mediaReferrals" = mediaReferrals, "campaignNewsletters" =  campaignNewsletters, 
            "SFcampaigns"= SFcampaigns,"donations"= donations, "campaignPosts" = campaignPosts, 
            "targetCampaign"= targetCampaign, "contentSummary"= contentSummary)
# change to not append
if (nrow(targetCampaign) > 0){
  dbWriteTable(con, name = 'targetCampaign', value = targetCampaign, append = TRUE)
  } else {print('No targetCampaign data to import')}

dbWriteTable(con, name = 'allTraffic', value = allTraffic, append = T)
#dbWriteTable(con, name = 'allTraffic', value = allTraffic, overwrite = TRUE)
dbWriteTable(con, name = 'socialTraffic', value = socialTraffic, append = T)
#dbWriteTable(con, name = 'socialTraffic', value = socialTraffic, overwrite = TRUE)
dbWriteTable(con, name = 'geographyTraffic', value = geographyTraffic, append = T)
#dbWriteTable(con, name = 'geographyTraffic', value = geographyTraffic, overwrite = TRUE)
dbWriteTable(con, name = 'mediaReferrals', value = mediaReferrals, append = T)
#dbWriteTable(con, name = 'mediaReferrals', value = mediaReferrals, overwrite = TRUE)
dbWriteTable(con, name = 'campaignNewsletters', value = campaignNewsletters, append = T)
#dbWriteTable(con, name = 'campaignNewsletters', value = campaignNewsletters, overwrite = TRUE)
dbWriteTable(con, name = 'campaignPosts', value = campaignPosts, append = T)
#dbWriteTable(con, name = 'campaignPosts', value = campaignPosts, overwrite = TRUE)
dbWriteTable(con, name = 'SFcampaigns', value = SFcampaigns, append = T)
#dbWriteTable(con, name = 'SFcampaigns', value = SFcampaigns, overwrite = TRUE)
dbWriteTable(con, name = 'contentSummary', value = contentSummary, append = T)
#dbWriteTable(con, name = 'contentSummary', value = contentSummary, overwrite = TRUE)
dbWriteTable(con, name = 'donations', value = donations, append = T)
#dbWriteTable(con, name = 'donations', value = donations, overwrite = TRUE)


# query <- paste0("DELETE FROM targetCampaign where campaignID ='",campaignID, "'")
# dbSendQuery(con, query)

dbDisconnect(con)



# for(i in dfs){
# 
#   dbWriteTable(con, i, i, append = T)
# }

write_xlsx(dfs, path = ss)


