
#############
#' FUNCTIONS #
#############

#' function that 1. binds the Campaign ID to all final dataframes and 2. pushes the data to a google sheet
#' if data already exists in sheet, df is bound to existing data and pushed
#' pushData <- function(df, sheetName){
#'   
#'   #' bind campaign ID
#'   df <- df %>% 
#'     mutate(CAMPAIGN_ID = campaignID) %>% 
#'     relocate(CAMPAIGN_ID, .before = 1)
#'   
#'   tryCatch({ 
#'     
#'     existingData <- read.xlsx(file = ss, sheet = sheetName) %>% 
#'       rbind(df)
#'     
#'     #' if development mode is on, overwrite data in sheet
#'     if(mode == 'development'){
#'       write.xlsx(df, file = ss, sheetName = sheetName, overwrite = T, append = T) 
#'       return(df)
#'     } else {
#'       write.xlsx(existingData, file = ss, sheetName = sheetName, overwrite = F) 
#'       return(existingData)
#'     }
#'     
#'     
#'   }, error = function(e) { 
#'     write.xlsx(df, file = ss, sheetName = sheetName) 
#'     return(df) 
#'   })
#' 
#' }


#### GOOGLE ANALYTICS ####

#trying again take 2

# library(rvest)
# library(dplyr)
# library(rvest)
# library(dplyr)
# 
# # Function to check the status of a URL
# check_url_status <- function(url) {
#   tryCatch({
#     status <- httr::status_code(httr::HEAD(url))
#     return(status)
#   }, error = function(e) {
#     return(NA) # Return NA if there's an error
#   })
# }
# 
# # Function to crawl a website and get all internal URLs recursively
# crawl_website <- function(base_url, visited_urls = c()) {
#   
#   # Don't process URLs we've already visited or from other domains
#   if (base_url %in% visited_urls || !grepl("^https://rmi.org", base_url)) {
#     return(visited_urls)
#   }
#   
#   # Check if URL returns a 200 status code
#   status_code <- check_url_status(base_url)
#   if (is.na(status_code) || status_code != 200) {
#     message(paste("Skipping URL due to non-200 status:", base_url))
#     return(visited_urls)
#   }
#   
#   # Add URL to visited list
#   visited_urls <- c(visited_urls, base_url)
#   
#   # Attempt to read the page
#   tryCatch({
#     page <- read_html(base_url)
#     
#     # Extract all links from the page
#     urls <- page %>%
#       html_nodes("a") %>%
#       html_attr("href")
#     
#     # Filter internal URLs only
#     internal_urls <- urls[grepl("^/|^https://rmi.org", urls)]
#     internal_urls <- unique(gsub("#.*", "", internal_urls))  # Remove fragment identifiers
#     
#     # Convert relative URLs to absolute URLs
#     full_urls <- ifelse(grepl("^/", internal_urls), paste0("https://rmi.org", internal_urls), internal_urls)
#     
#     # Recursively visit each new internal URL
#     for (url in full_urls) {
#       if (!url %in% visited_urls) {
#         visited_urls <- crawl_website(url, visited_urls)
#       }
#     }
#     
#   }, error = function(e) {
#     message(paste("Error crawling:", base_url, e))
#   })
#   
#   return(visited_urls)
# }
# 
# # Start crawling from the homepage
# start_url <- "https://rmi.org"
# all_urls <- crawl_website(start_url)
# 
# # Print the number of unique URLs found
# print(paste("Number of unique URLs found:", length(all_urls)))
# 
# # View all extracted URLs
# print(all_urls)
# 
# 
# 
# ####
# 
# scrape_campaigntag <- function(url, campaign_tags) {
#   if (!grepl("^http", url)) {
#     url <- paste0("https://rmi.org", url)
#   }
#   
#   tryCatch({
#     page <- read_html(url) 
#     
#     #extract metadata text from this specific script tag
#     json_scripts <- page %>%
#       html_nodes('script[type="application/ld+json"]') %>%
#       html_text() 
#     
#     #initialize campaign tag var  
#     #campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+") # add to or change
#     campaign_tag <- NULL #initializing
#     
#     #loops through each element, parsing json string
#     for (i in json_scripts) {
#       json_content <- fromJSON(i)
#       
#      # print("Extracted JSON content:") #testing
#      # print(json_content)
#       
#       # checks if has keyword field and is of type character; lowercase to avoid case sensitivity 
#       if ("keywords" %in% names(json_content) && is.character(json_content$keywords)) {
#         lowercase_keywords <- tolower(json_content$keywords)
#         
#         # if tag is found, assign it to campaign tag variable and return value
#         for (tag in campaign_tags) {
#           if (tag %in% lowercase_keywords){
#             campaign_tag <- tag
#             print(paste("Campaign tag found:", campaign_tag)) #testing
#             break
#           }
#         }
#       }
#       if(!is.null(campaign_tag)) 
#         break
#     }
#     return(campaign_tag)
#   }, error = function(e) {
#     message(paste("Error in scraping URL:", url, e))
#     return(NULL)
#   })
# }
# 
# find_urls_with_campaign_tags <- function(df, campaign_tags) {
#   base_domain <- "https://rmi.org"
#   
#   filtered_urls <- data.frame(pagePath = character(), campaign_tag = character(), stringsAsFactors = FALSE)
#   
#   for(i in 1:nrow(df)){
#     full_url <- paste0(base_domain, df$pagePath[i])
#     campaign_tag <- scrape_campaigntag(full_url, campaign_tags)
#     
#     if (!is.null(campaign_tag)) {
#       filtered_urls <- rbind(filtered_urls, data.frame(pagePath = full_url, campaign_tag = campaign_tag, stringsAsFactors = FALSE))
#     
#     }
#   }
#   
#   return(filtered_urls)
# }
# 
# url_data <- data.frame(pagePath = c("/insight/kyog/", "/theres-no-one-size-fits-all-for-achieving-universal-energy-access-in-africa/",
#                                     "/rmi-at-cop28/", "/reality-check-natural-gas-true-climate-risk/"))
# 
# campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+")
# 
# filtered_urls <- find_urls_with_campaign_tags(url_data, campaign_tags)
# print(filtered_urls)
# 
# 
# # MODIFYING NEW FUNCTIONS take 1
# campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+")
# 
# scrape_campaigntag <- function(url, campaign_tags) {
#   if (!grepl("^http", url)) {
#     url <- paste0("https://rmi.org", url)
#   }
#   
#   tryCatch({
#     page <- read_html(url) 
#     
#     #extract metadata text from this specific script tag
#     json_scripts <- page %>%
#       html_nodes('script[type="application/ld+json"]') %>%
#       html_text() 
#     
#     #initialize campaign tag var  
#     #campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+") # add to or change
#     campaign_tag <- NULL #initializing
#     
#     #loops through each element, parsing json string
#     for (i in json_scripts) {
#       json_content <- fromJSON(i)
#       
#       # checks if has keyword field and is of type character; lowercase to avoid case sensitivity 
#       if ("keywords" %in% names(json_content) && is.character(json_content$keywords)) {
#         lowercase_keywords <- tolower(json_content$keywords)
#         
#         # if tag is found, assign it to campaign tag variable and return value
#         for (tag in campaign_tags) {
#           if (tag %in% lowercase_keywords){
#             campaign_tag <- tag
#             print(paste("Campaign tag found:", campaign_tag)) #testing
#             break
#           }
#         }
#       }
#       if(!is.null(campaign_tag)) 
#         break
#     }
#     return(campaign_tag)
#   }, error = function(e) {
#     message(paste("Error in scraping URL:", url, e))
#     return(NULL)
#   })
# }
# 
# url1 <- "https://rmi.org/insight/kyog/" 
# url2 <- "https://rmi.org/theres-no-one-size-fits-all-for-achieving-universal-energy-access-in-africa/"
# url3 <- "https://rmi.org/rmi-at-cop28/"
# url4 <- "https://rmi.org/reality-check-natural-gas-true-climate-risk/"
# campaign_tag <- scrape_campaigntag(url4)
# print(campaign_tag)
# 
# test_url <- "https://rmi.org/people/nicole-leonard/" # A URL path from GA4
# 
# # Call the scraping function and print the result
# campaign_tag <- scrape_campaigntag(test_url)
# print(paste("Campaign tag found:", campaign_tag)) # testing to see whether this url will work
# 
# # other function
# get_filtered_URLs <- function(propertyID, campaign_tags) {
#   all_data <- ga_data(
#     propertyID,
#     date_range = dateRangeGA,
#     dimensions = c("pagePath"),
#     metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
#     limit = -1
#   )
#   
#   #convert to a dataframe
#   url_data <- as.data.frame(all_data)
#   
#   # initialize new df for filtered urls
#   filtered_urls <- data.frame(pagePath = character(), campaign_tag = character(), stringsAsFactors = FALSE)
#   
#   base_domain <- "https://rmi.org" 
#   
#   for (url in url_data$pagePath) {
#     full_url <- paste0(base_domain, url)
#     #print(paste("Processing URL:",full_url))  # Print each URL being processed
#     campaign_tag <- scrape_campaigntag(full_url, campaign_tags)
#     
#     if (!is.null(campaign_tag)){
#       filtered_urls <- rbind(filtered_urls, data.frame(pagePath = full_url, campaign_tag = campaign_tag, stringsAsFactors = FALSE))
#       
#     }
#   }
#   
#   filtered_metrics <- url_data %>%
#     filter(paste0(base_domain, pagePath) %in% filtered_urls$pagePath)
#   
#   filtered_urls_metrics <- merge(filtered_urls, filtered_metrics, by.x = "pagePath", by.y = "pagePath", all.x = TRUE)
#   
#   
#   return(filtered_urls_metrics)
# }
# 
# #url_data <- as.data.frame(all_data)
# #print(url_data$pagePath)
# 
# 
# # Print the URLs
# #print(url_data$pagePath)
# 
# #getWebsiteURLs(propertyID = rmiPropertyID, campaign_tags = campaign_tags)
# rmiPropertyID <- 354053620
# campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+")
# 
# result <- get_filtered_URLs(rmiPropertyID, campaign_tags)
# print(result)
# 
# #return(filtered_urls)
# 
# 
# 
# 
# 
# 
# # END MODIFYING NEW FUNCTIONS



# NEW: Build function that uses campaign tag in webpage metadata to identify related URLs

# function that scrapes to get campaign

# OLD Function to scrape CAMPAIGN TAG from a URL
scrape_campaigntag <- function(url) {
  if (!grepl("^http", url)) {
    url <- paste0("https://rmi.org", url)
  }
  
  tryCatch({
  page <- read_html(url) 
  
  #extract metadata text from this specific script tag
  json_scripts <- page %>%
    html_nodes('script[type="application/ld+json"]') %>%
    html_text() 
  
  # close connection to prevent leak
  closeAllConnections()
  
  #initialize campaign tag var  
  campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+", "nycw24", "transition-narrative", 
                     "cop29", "fapp24", "cera25", "sapp25", "nycw25") # add to or change
  campaign_tag <- NULL #initializing

  #loops through each element, parsing json string
  for (i in json_scripts) {
    json_content <- fromJSON(i)
    
    print("Extracted JSON content:") #testing
    print(json_content)

    # checks if has keyword field and is of type character; lowercase to avoid case sensitivity 
    if ("keywords" %in% names(json_content) && is.character(json_content$keywords)) {
      lowercase_keywords <- tolower(json_content$keywords)
      
      # if tag is found, assign it to campaign tag variable and return value
      for (tag in campaign_tags) {
        if (tag %in% lowercase_keywords){
          campaign_tag <- tag
          print(paste("Campaign tag found:", campaign_tag)) #testing
          break
        }
      }
    }
    if(!is.null(campaign_tag)) 
      break
  }
  return(campaign_tag)
  }, error = function(e) {
    message(paste("Error in scraping URL:", url, e))
    return(NULL)
  })
}

url1 <- "https://rmi.org/insight/kyog/" 
url2 <- "https://rmi.org/theres-no-one-size-fits-all-for-achieving-universal-energy-access-in-africa/"
url3 <- "https://rmi.org/rmi-at-cop28/"
url4 <- "https://rmi.org/reality-check-natural-gas-true-climate-risk/"
url5 <- "https://rmi.org/insight/toward-a-shared-zero-carbon-energy-future/"
campaign_tag <- scrape_campaigntag(url5)
eprint(campaign_tag)


#test_url <- "https://rmi.org/people/nicole-leonard/" # A URL path from GA4

# Call the scraping function and print the result
#campaign_tag <- scrape_campaigntag(test_url)
#print(paste("Campaign tag found:", campaign_tag)) # testing to see whether this url will work

# NEW: Function that gets all urls, scrapes each url, and then adds to dataframe
# need to add campaign filter

# getWebsiteURLs <- function(propertyID, campaign_tags, date_range = dateRangeGA) {
#   # Step 1: Retrieve all pages data
#   all_data <- ga_data(
#     propertyID,
#     date_range = dateRangeGA,
#     dimensions = c("pagePath", "pageTitle", "fullPageUrl"),
#     metrics = c('screenPageViews'),
#     limit = -1
#   )
# 
#   # Convert to a dataframe
#   url_data <- as.data.frame(all_data)
# 
#   # Step 2: Apply filtering
#   filtered_data <- url_data %>%
#    # filter(grepl('rmi.org', fullPageUrl)) %>%
#     filter(pageTitle != 'Page not found - RMI') %>%
#     filter(pageTitle != '(not set)') %>%
#    # filter(pageTitle != '') %>%
#     filter(!grepl('rmi.org/people', fullPageUrl))
#   #  filter(screenPageViews > 10) %>%
#     # mutate(pageURL = paste('https://', fullPageUrl, sep = ''),
#     #        pageTitle = gsub(' - RMI', '', pageTitle),
#     #        site = 'rmi.org', metadata = '', pageType = '')
#     #
#   # Step 3: Initialize a new dataframe for URLs with campaign tags
#   filtered_urls <- data.frame(pagePath = character(),
#                               campaign_tag = character(),
#                               stringsAsFactors = FALSE)
# 
#   # Step 4: Extract and filter by campaign tags
#   base_domain <- "https://rmi.org"
# 
#   for (url in filtered_data$pagePath) {
#     full_url <- paste0(base_domain, url)
#     # Print each URL being processed (optional)
#     # print(paste("Processing URL:", full_url))
# 
#     campaign_tag <- scrape_campaigntag(full_url)
# 
#     if (!is.null(campaign_tag) && campaign_tag %in% campaign_tags) {
#       filtered_urls <- rbind(filtered_urls, data.frame(pagePath = full_url, campaign_tag = campaign_tag, stringsAsFactors = FALSE))
#     }
#   }
# 
#   return(filtered_urls)
# }


getWebsiteURLs <- function(propertyID, campaign_tags, date_range = dateRangeGA) {
  all_data <- ga_data(
    propertyID,
    date_range = dateRangeGA,
    dimensions = c("pagePath"),
    metrics = c('screenPageViews'),
    limit = -1
  )

  #convert to a dataframe
  url_data <- as.data.frame(all_data)

  # Print to verify url  format
  #print("Retrieved URLs:")
  #print(url_data$pagePath)


  # initialize new df for filtered urls
  filtered_urls <- data.frame(pagePath = character(), campaign_tag = character(), stringsAsFactors = FALSE)

  base_domain <- "https://rmi.org"

  for (url in url_data$pagePath) {
    full_url <- paste0(base_domain, url)
    #print(paste("Processing URL:",full_url))  # Print each URL being processed
    campaign_tag <- scrape_campaigntag(full_url)

    if (!is.null(campaign_tag) && campaign_tag %in% campaign_tags){
      filtered_urls <- rbind(filtered_urls, data.frame(pagePath = full_url, campaign_tag = campaign_tag, stringsAsFactors = FALSE))

    }
  }

  return(filtered_urls)
}


getWebsiteURLs2 <- function(propertyID, campaign_tags, date_range = dateRangeGA, pages_filter_criteria) {
  # Retrieve all data from GA4
  all_data <- ga_data(
    rmiPropertyID,
    metrics = c('screenPageViews'),
    dimensions = c("pageTitle",'fullPageUrl'),
    date_range = dateRangeGA,
    limit = -1)
  
  # Convert to dataframe
  url_data <- as.data.frame(all_data)
  
  # Apply filtering criteria to the GA4 data
  filtered_pages <- url_data %>%
    filter(grepl('rmi.org', fullPageUrl)) %>%
    filter(pageTitle != 'Page not found - RMI') %>%
    filter(pageTitle != '(not set)') %>%
    filter(pageTitle != '') %>%
    filter(!grepl('rmi.org/people', fullPageUrl)) %>%
    filter(screenPageViews > 10) %>%
    mutate(
      pageURL = paste0('https://', fullPageUrl),
      pageTitle = gsub(' - RMI', '', pageTitle),
      site = 'rmi.org',
      metadata = '',
      pageType = ''
    )
  
  # Initialize a dataframe to hold the filtered URLs with campaign tags
  filtered_urls <- data.frame(pagePath = character(), campaign_tag = character(), stringsAsFactors = FALSE)
  
  # Scrape and filter URLs based on campaign tags
  for (url in filtered_pages$pageURL) {
    # Process only filtered URLs
    campaign_tag <- scrape_campaigntag(url)
    
    if (!is.null(campaign_tag) && campaign_tag %in% campaign_tags) {
      filtered_urls <- rbind(filtered_urls, data.frame(
        pagePath = url,
        campaign_tag = campaign_tag,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(filtered_urls)
}


#url_data <- as.data.frame(all_data)
#print(url_data$pagePath)


# Print the URLs
#print(url_data$pagePath)

#getWebsiteURLs(propertyID = rmiPropertyID, campaign_tags = campaign_tags)
rmiPropertyID <- 354053620
campaign_tags <- c("cop28", "2023-2025_coalvgas", "oci+", "nycw24", "transition-narrative", "cop29", "fapp24", "cera25", "sapp25", "nycw25")

# Call the function with the correct arguments
#filtered_urls <- getWebsiteURLs(propertyID = rmiPropertyID, campaign_tags = campaign_tags)
#view(filtered_urls)

#return(filtered_urls)

# NEW: Function that scrapes the given urls for titles.
scrape_pagetitle <- function(url) {
    webpage <- read_html(url) 
    page_title <- webpage %>%
      html_nodes("title") %>%
      html_text() %>%
      trimws()
    
    cleaned_title <- sub(" - RMI$", "", page_title)
    # prevent memory leak
    closeAllConnections()
    
    return(cleaned_title)
}

#url3 <- "https://rmi.org/event/later-is-too-late/"
#cleaned_title <- scrape_pagetitle(url3)
#print(cleaned_title)

# NEW: Function that scrapes the given urls for content type.
scrape_pagetype <- function(url) {
  path <- gsub("https://rmi.org/", "", url)
  
  if (grepl("^insight", path)) {
    return("Report")
  } else if (grepl("^event", path)){
    return("Event")
  } else if (grepl("^press-release", path)) {
    return("News Release")
  } else if (grepl("rmi-at", path)){
    return("Hub") #maybe hardcode in the future? not sure if this logic will apply to all hubs
  } else {
    return("Article")
  }
}

example1 <- "https://rmi.org/event/sustainable-aluminum-finance-framework-launch-at-cop28/"
example2 <- "https://rmi.org/insight/the-rural-equitable-climate-transition-toward-carbon-neutrality-and-shared-prosperity/"
example3 <- "https://rmi.org/organic-waste-an-untapped-solution-waste-authorities-in-nigeria-tackle-food-waste-as-a-climate-solution-in-lagos/"
example4 <- "https://rmi.org/press-release/caribbean-countries-on-the-frontline-of-climate-change-get-lasting-support-to-unlock-critical-finance/"
example5 <- "https://rmi.org/insight/toward-a-shared-zero-carbon-energy-future/"

print(scrape_pagetype(example1))
print(scrape_pagetype(example2))
print(scrape_pagetype(example3))
print(scrape_pagetype(example4))
print(scrape_pagetype(example5))


# new function to get report_id and event_id for future campaigns
scrape_report_id <- function(df) {
  
  report_ids <- c()
  
  for (url in df$pagePath) {
    tryCatch({
      page <- read_html(url)
      
      json_scripts <- page %>%
        html_nodes('script[type="application/ld+json"]') %>%
        html_text()
      
      report_id <- NULL
      
      for (i in json_scripts) {
        json_content <- fromJSON(i)
        
        print("Extracted JSON content:")
        print(json_content)
        
        if ("report_id" %in% names(json_content)) {
          report_id <- json_content$report_id
          print(paste("Report ID found:", report_id)) # Testing
          break
        }
      }
      
      report_ids <- c(report_ids, ifelse(is.null(report_id), NA, report_id))
      
    }, error = function(e) {
      message(paste("Error scraping URL:", url))
      report_ids <- c(report_ids, NA)
    })
  }
  
  df$report_id <- report_ids
  
  return(df)
}



scrape_event_id <- function(df) {
  
  event_ids <- c()
  
  for (url in df$pagePath) {
    tryCatch({
      page <- read_html(url)
      
      json_scripts <- page %>%
        html_nodes('script[type="application/ld+json"]') %>%
        html_text()
      
      event_id <- NULL
      
      for (i in json_scripts) {
        json_content <- fromJSON(i)
        
        print("Extracted JSON content:")
        print(json_content)
        
        if ("event_id" %in% names(json_content)) {
          event_id <- json_content$event_id
          print(paste("Event ID found:", event_id)) # Testing
          break
        }
      }
      
      event_ids <- c(event_ids, ifelse(is.null(event_id), NA, event_id))
      
    }, error = function(e) {
      message(paste("Error scraping URL:", url))
      event_ids <- c(event_ids, NA)
    })
  }
  
  df$event_id <- event_ids
  
  return(df)
}



#this code should be added to dashboard eventually; not sure if it will keep updating, but it should? 
#sapply makes it a vector
# campaign_urls <- filtered_urls %>%
#   mutate(pageTitle = sapply(pagePath, scrape_pagetitle)) %>%
#   mutate(pageType = sapply(pagePath, scrape_pagetype)) %>%
#   select(pageTitle, pageType, pagePath, campaign_tag) %>%
#   mutate(propertyID = 354053620) #can create if-else for coal vs gas
#   #mutate(reportID) %>%
#   #mutate(eventID)
# 

# view(campaign_urls)

# NEW: Function that populates new dataframe to replicate previous "campaignkey" xls
#str(filtered_urls)  # might not need this.

# new_campaignkey_df <- function(filtered_urls) {
#   campaignkey_df <- data.frame(
#     title = character(),
#     type = character(),
#     page = character(),
# #    reportid = numeric(),
#  #   eventid = numeric(),
#     stringAsFactors = FALSE
#   )
# 
#   for (i in 1:nrow(filtered_urls)) {
#     url <- filtered_urls$pagePath[i]
#     campaign_tag <- filtered_urls$campaign_tag[i]
#     print(paste("Processing URL:", url))  # Debugging 
#     
#     scraped_info <- scrape_campaigntag(url)
#     print("Scraped info:")
#     print(scraped_info)
#     
#     if (!is.null(scraped_info) && all(c('headline', '@type', 'url') %in% names(scraped_info))) {
#       title <- scraped_info$headline
#       type <- scraped_info$`@type`
#       page <- scraped_info$url
#     } else {
#       # If scraping failed or fields are missing, populate with NA
#       title <- NA
#       type <- NA
#       page <- NA
#       print(paste("No valid scraped info for URL:", url)) 
#     }
#       
#     campaignkey_df <- rbind(campaignkey_df, data.frame(
#         title = title,
#         type = type,
#         page = page,
#  #       reportid = NA,
#   #      eventid = NA,
#         stringAsFactors = False
#       ))
#     
#     print(paste("Iteration", i, ": Current dataframe state:"))
#     print(campaignkey_df)
#   }
#   return(campaignkey_df)
#   
# }
# 
# result_df <- new_campaignkey_df(filtered_urls)
# print(campaignkey_df)
# head(campaignkey_df)

# end.

#' get page title and content type by web scraping page URLs

getPageData <- function(df){
  
  # add new column for datePublished
  if(!"datePublished" %in% colnames(df)){
    df$datePublished <- NA
  }
  
  for(i in 1:nrow(df)){
    
    url <- as.character(df[i, 'pageURL'])
    
    # --- get page title ---
    tryCatch({ 
      url_tb <- url %>%
        read_html() %>% 
        html_nodes('head > title') %>% 
        html_text() %>% 
        as.data.frame() %>% 
        rename(title = 1) 
      
      df[i, 'pageTitle'] <- url_tb[1, 'title']
      
    }, error = function(e){
      df[i, 'pageTitle'] <- NA
    })
    
    # --- get metadata + program ---
    if(df[i, 'site'] == 'rmi.org'){
      tryCatch({ 
        url_tb <- url %>%
          read_html() %>% 
          html_nodes('script') %>% 
          html_text() %>% 
          as.data.frame() %>%
          rename(node = 1) %>% 
          filter(grepl('schema.org', node)) %>% 
          mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
                 program = gsub('articleSection":"',"",program),
                 program = gsub('"', "", program)) %>%
          mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
                 keywords = gsub('\\].*', "", keywords))
        
        df[i, 'metadata'] <- url_tb[2, 'keywords']
        df[i, 'program'] <- url_tb[2, 'program']
        
      }, error = function(e){
        df[i, 'metadata'] <- NA
      })
    } else {
      # categorize as "New Website" if no metadata
      df[i, 'metadata'] <- NA
      df[i, 'pageType'] <- 'New Website'
    }
    
    # --- get datePublished from JSON-LD ---
    tryCatch({
      date_data <- url %>%
        read_html() %>%
        html_nodes('script[type="application/ld+json"]') %>%
        html_text() %>%
        lapply(fromJSON, simplifyVector = TRUE)
      
      date_published <- NA
      for(script in date_data){
        if('datePublished' %in% names(script)){
          date_published <- script$datePublished
          break
        }
      }
      
      df[i, 'datePublished'] <- date_published
    }, error = function(e){
      df[i, 'datePublished'] <- NA
    })
  }
  
  # --- categorize pageType + icon ---
  df <- df %>% 
    mutate(pageType = case_when(
      grepl('reality check', tolower(metadata)) ~ 'Reality Check', 
      grepl('101', tolower(metadata)) ~ '101',
      grepl('presentation', tolower(metadata)) ~ 'Presentation',  
      grepl('story', tolower(metadata)) ~ 'Story', 
      grepl('article', tolower(metadata)) ~ 'Article',
      grepl('report', tolower(metadata)) ~ 'Report',
      grepl('news|press-release', tolower(pageURL)) ~ 'News Release',
      grepl('event', tolower(pageURL)) ~ 'Event',
      grepl('technology ecosystem', pageTitle) ~ 'Article',
      grepl('hub|rmi', tolower(pageURL)) ~ 'Hub',
      TRUE ~ pageType),
      icon = case_when(
        grepl('reality check', tolower(metadata)) ~ 8,
        grepl('101', tolower(metadata)) ~ 7,
        grepl('presentation', tolower(metadata)) ~ 9, 
        grepl('story', tolower(metadata)) ~ 10, 
        grepl('article', tolower(metadata)) ~ 4,
        grepl('report', tolower(metadata)) ~ 1,
        grepl('news|press-release', tolower(pageURL)) ~ 2,  
        grepl('event', tolower(pageURL)) ~ 3,
        grepl('technology ecosystem', pageTitle) ~ 4, 
        grepl('hub|rmi', tolower(pageURL)) ~ 6,
        TRUE ~ 5)) %>%
    distinct(pageTitle, .keep_all = TRUE)
  
  return(df)
}

getPageDataLATEST <- function(df){
  
  for(i in 1:nrow(df)){
    
    url <- as.character(df[i, 'pageURL'])
    
    #' get page titles
    tryCatch( { 
      url_tb <- url %>%
        read_html() %>% 
        html_nodes('head > title') %>% 
        html_text() %>% 
        as.data.frame() %>% 
        rename(title = 1) 
      
      df[i, 'pageTitle'] <- url_tb[1, 'title']
      
    }, error = function(e){
      df[i, 'pageTitle'] <- NA
    })
    
    #' get content type from page metadata
    if(df[i, 'site'] == 'rmi.org'){
      tryCatch( { 
        url_tb <- url %>%
          read_html() %>% 
          html_nodes('script') %>% 
          html_text() %>% 
          as.data.frame() %>%
          rename(node = 1) %>% 
          filter(grepl('schema.org', node)) %>% 
          mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
                 program = gsub('articleSection":"',"",program),
                 program = gsub('"', "", program)) %>%
          mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
                 keywords = gsub('\\].*', "", keywords))
        
        df[i, 'metadata'] <- url_tb[2, 'keywords']
        df[i, 'program'] <- url_tb[2, 'program']
        
      }, error = function(e){
        df[i, 'metadata'] <- NA
      })
    } else {
      #' categorize as 'New Website' if no metadata is detected
      df[i, 'metadata'] <- NA
      df[i, 'pageType'] <- 'New Website'
    }
    
  }
  
  #' categorize as 'Article' or 'Report' if these terms are detected in the metadata
  df <- df %>% 
    # mutate(pageType = ifelse(grepl('article', tolower(metadata)), 'Article',
    #                          ifelse(grepl('report', tolower(metadata)), 'Report',
    #                                 ifelse(grepl('news release', tolower(metadata)), 'News Release',
    #                                        pageType)),
      mutate(pageType = case_when(grepl('reality check', tolower(metadata)) ~ 'Reality Check', 
                                  grepl('101', tolower(metadata)) ~ '101',
                                  grepl('presentation', tolower(metadata)) ~ 'Presentation',  
                                  grepl('story', tolower(metadata)) ~ 'Story', 
                                  grepl('article', tolower(metadata)) ~ 'Article',
                                  grepl('report', tolower(metadata)) ~ 'Report',
                                  grepl('news|press-release', tolower(pageURL)) ~ 'News Release',  # Updated to catch all
                                  grepl('event', tolower(pageURL)) ~ 'Event',
                                #  grepl('101|reality check', tolower(metadata)) ~ 'Article',  # Added condition for '101' or 'Reality Check'
                                  grepl('technology ecosystem', pageTitle) ~ 'Article',  # Added condition for "Toward a Technology Ecosystem for Carbon Accounting"
                                  grepl('hub|rmi', tolower(pageURL)) ~ 'Hub',
                                  TRUE ~ pageType),
            icon = case_when(grepl('reality check', tolower(metadata)) ~ 8,
                             grepl('101', tolower(metadata)) ~ 7,
                             grepl('presentation', tolower(metadata)) ~ 9, 
                             grepl('story', tolower(metadata)) ~ 10, 
                             grepl('article', tolower(metadata)) ~ 4,
                             grepl('report', tolower(metadata)) ~ 1,
                             grepl('news|press-release', tolower(pageURL)) ~ 2,  
                             grepl('event', tolower(pageURL)) ~ 3,
                            # grepl('101|reality check', tolower(metadata)) ~ 4,  # Added condition for '101' or 'Reality Check'
                             grepl('technology ecosystem', pageTitle) ~ 4, 
                             grepl('hub|rmi', tolower(pageURL)) ~ 6,
                             TRUE ~ 5)) %>%
              
              # ifelse(grepl('article', tolower(metadata)), 4,
              #            ifelse(grepl('report', tolower(metadata)), 1, 5))) %>% 
    distinct(pageTitle, .keep_all = TRUE)
  
}

#AMENDING THIS FUNCTION TO INCLUDE CAMPAIGN TAG
#' 
#' getPageData <- function(df){
#'   
#'   for(i in 1:nrow(df)){
#'     
#'     url <- as.character(df[i, 'pageURL'])
#'     
#'     #' get page titles
#'     tryCatch( { 
#'       url_tb <- url %>%
#'         read_html() %>% 
#'         html_nodes('head > title') %>% 
#'         html_text() %>% 
#'         as.data.frame() %>% 
#'         rename(title = 1) 
#'       
#'       df[i, 'pageTitle'] <- url_tb[1, 'title']
#'       
#'     }, error = function(e){
#'       df[i, 'pageTitle'] <- NA
#'     })
#'     
#'     #' get content type from page metadata
#'     if(df[i, 'site'] == 'rmi.org'){
#'       tryCatch( { 
#'         url_tb <- url %>%
#'           read_html() %>% 
#'           html_nodes('script') %>% 
#'           html_text() %>% 
#'           as.data.frame() %>%
#'           rename(node = 1) %>% 
#'           filter(grepl('schema.org', node)) %>% 
#'           mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
#'                  program = gsub('articleSection":"',"",program),
#'                  program = gsub('"', "", program)) %>%
#'           mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
#'                  keywords = gsub('\\].*', "", keywords))
#'         
#'         df[i, 'metadata'] <- url_tb[2, 'keywords']
#'         df[i, 'program'] <- url_tb[2, 'program']
#'         
#'       }, error = function(e){
#'         df[i, 'metadata'] <- NA
#'       })
#'     } else {
#'       #' categorize as 'New Website' if no metadata is detected
#'       df[i, 'metadata'] <- NA
#'       df[i, 'pageType'] <- 'New Website'
#'     }
#'     
#'   }
#'   
#'   #' categorize as 'Article' or 'Report' if these terms are detected in the metadata
#'   df <- df %>% 
#'     # mutate(pageType = ifelse(grepl('article', tolower(metadata)), 'Article',
#'     #                          ifelse(grepl('report', tolower(metadata)), 'Report',
#'     #                                 ifelse(grepl('news release', tolower(metadata)), 'News Release',
#'     #                                        pageType)),
#'     mutate(pageType = case_when(grepl('article', tolower(metadata)) ~ 'Article',
#'                                 grepl('report', tolower(metadata)) ~ 'Report',
#'                                 grepl('news', tolower(metadata)) ~ 'News Release',
#'                                 grepl('event', tolower(pageURL)) ~ 'Event',
#'                                 grepl('hub', tolower(pageURL)) ~ 'Hub',
#'                                 TRUE ~ pageType),
#'            icon = case_when(grepl('article', tolower(metadata)) ~ 4,
#'                             grepl('report', tolower(metadata)) ~ 1,
#'                             grepl('news', tolower(metadata)) ~ 2,
#'                             grepl('event', tolower(pageURL)) ~ 3,
#'                             grepl('hub', tolower(pageURL)) ~ 6,
#'                             TRUE ~ 5)) %>%
#'     
#'     # ifelse(grepl('article', tolower(metadata)), 4,
#'     #            ifelse(grepl('report', tolower(metadata)), 1, 5))) %>% 
#'     distinct(pageTitle, .keep_all = TRUE)
#'   
#' }
#' # End of new function.

#' get web traffic metrics for all pages
getPageMetrics <- function(propertyID, pages){
  campaignPages <- ga_data(
    propertyID,
    metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
    dimensions = c("pageTitle"),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    #' calculate average engagement duration from userEngagementDuration and convert seconds to mm:ss format
    mutate(engagementDuration = userEngagementDuration / totalUsers,
           sec = round(engagementDuration %% 60, 0),
           min = (engagementDuration / 60) |> floor(),
           avgEngagementDuration = paste0(min, ':', ifelse(nchar(sec) == 1, paste0('0', sec), sec))) %>% 
    select(pageTitle, screenPageViews, totalUsers, engagementDuration, avgEngagementDuration) %>% 
    left_join(select(pageData, c(pageTitle,program, pageType, icon)), by = c('pageTitle')) %>% 
    #' remove " - RMI" from end of page titles
    mutate(pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(campaignPages)
}

getPageMetrics2 <- function(propertyID, pages) {
  
  # Define the special case for shorter date range
  donations_page <- "Make a Gift - RMI"  
  shortDateRangeGA <- c("2024-11-01", "2025-02-10")  # Adjust as needed
  dateRangeGA <- c("2023-01-01", as.character(Sys.Date()))  # Default date range
  
  # Initialize an empty dataframe
  allPageMetrics <- data.frame()
  
  for (page in pages) {
    message(paste("Processing page:", page))  # Debug message
    
    # Assign date range: short for donations page, default for others
    date_range <- if (page == donations_page) shortDateRangeGA else dateRangeGA
    message(paste("Using date range:", paste(date_range, collapse = " to ")))  # Debug message
    
    # Fetch data for the current page
    pageMetrics <- tryCatch(
      {
        message("Fetching data from GA4...")  # Debug message
        data <- ga_data(
          propertyID,
          metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
          dimensions = c("pageTitle"),
          date_range = date_range,  # Apply selected date range
          dim_filters = ga_data_filter("pageTitle" == page),
          limit = -1
        )
        
        message("Data successfully retrieved.")  # Debug message
        message(paste("Number of rows retrieved:", nrow(data)))  # Debug message
        
        # If no data found, print warning
        if (nrow(data) == 0) {
          message(paste("No data found for page:", page))
          return(NULL)
        }
        
        # Process data
        data %>%
          mutate(
            engagementDuration = userEngagementDuration / totalUsers,
            sec = round(engagementDuration %% 60, 0),
            min = floor(engagementDuration / 60),
            avgEngagementDuration = paste0(min, ':', ifelse(nchar(sec) == 1, paste0('0', sec), sec))
          ) %>%
          select(pageTitle, screenPageViews, totalUsers, engagementDuration, avgEngagementDuration) %>%
          left_join(select(pageData, c(pageTitle, program, pageType, icon)), by = c('pageTitle')) %>%
          mutate(pageTitle = gsub(' - RMI', '', pageTitle))
      },
      error = function(e) {
        message(paste("Error processing page:", page, "Error message:", e$message))
        return(NULL)
      }
    )
    
    # Append data if it exists
    if (!is.null(pageMetrics)) {
      allPageMetrics <- bind_rows(allPageMetrics, pageMetrics)
    }
  }
  
  return(allPageMetrics)
}


getPageMetrics3 <- function(propertyID, pageAndTag) {
  
  # Define campaign-specific date ranges
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10"),
    nycw24 = c("2024-09-01", "2024-10-05"),
    nycw25 = c("2025-08-31", "2025-10-04"),
    cera25 = c("2025-03-07", "2025-04-11"),
    cop29 = c("2024-11-11", "2024-12-16"),
    cop28 = c("2023-11-30", "2023-12-30")
   #transition-narrative = c("2024-11-01", "2025-02-10")
   #oci+ = c("2024-11-01", "2025-02-10")
   #2023-2025_coalvgas = c("2024-11-01", "2025-02-10")
    
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  # Clean: turn empty strings into NA
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allPageMetrics <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])  # could be NA
    
    message(paste("Processing page:", page))
    
    # Safely assign date range
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    message(paste("Using date range:", paste(date_range, collapse = " to ")))
    
    pageMetrics <- tryCatch(
      {
        message("Fetching data from GA4...")
        data <- ga_data(
          propertyID,
          metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
          dimensions = c("pageTitle"),
          date_range = date_range,
          dim_filters = ga_data_filter("pageTitle" == page),
          limit = -1
        )
        
        if (nrow(data) == 0) {
          message(paste("No data found for page:", page))
          return(NULL)
        }
        
        data %>%
          mutate(
            engagementDuration = userEngagementDuration / totalUsers,
            sec = round(engagementDuration %% 60, 0),
            min = floor(engagementDuration / 60),
            avgEngagementDuration = paste0(min, ':', ifelse(nchar(sec) == 1, paste0('0', sec), sec)),
            campaign_tag = tag,
            startDate = date_range[1],
            endDate = date_range[2],
            pageTitle = gsub(" - RMI", "", pageTitle)
          ) %>%
          select(pageTitle, campaign_tag, startDate, endDate, screenPageViews, totalUsers, engagementDuration, avgEngagementDuration)
      },
      error = function(e) {
        message(paste("Error processing page:", page, "Error message:", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(pageMetrics)) {
      allPageMetrics <- bind_rows(allPageMetrics, pageMetrics)
    }
  }
  
  return(allPageMetrics)
}

correctTrafficOLD <- function(df, type) { 
  if (type == 'session') { 
    df <- df %>% 
      rename(
        medium = sessionMedium, 
        source = sessionSource, 
        defaultChannelGroup = sessionDefaultChannelGroup
      )
  } 
  
  df <- df %>% 
    mutate(
      pageTitle = gsub(' - RMI', '', pageTitle),
      medium = ifelse(
        grepl('mail.google.com', source) | grepl('web-email|sf|outlook', medium),
        'email', 
        medium
      ),
      source = ifelse(grepl('linkedin|lnkd\\.in|li', source), 'linkedin', source),
      source = ifelse(grepl('facebook', source), 'facebook', source),
      source = ifelse(grepl('dlvr\\.it|twitter', source) | source == 't.co', 'twitter', source),
      medium = ifelse(
        grepl('linkedin|lnkd\\.in|facebook|twitter|instagram', source) |
          grepl('twitter|fbdvby', medium),
        'social', 
        medium
      ),
      medium = ifelse(grepl('/t\\.co/', pageReferrer), 'social', medium),
      source = ifelse(grepl('/t\\.co/', pageReferrer), 'twitter', source),
      source = ifelse(grepl('instagram', source), 'instagram', source),
      defaultChannelGroup = ifelse(
        medium == 'social', 'Organic Social',
        ifelse(medium == 'email', 'Email', defaultChannelGroup)
      )
    )
  
  return(df)
}




#' correct GA acquisition attribution for social media and email channels 
correctTraffic <- function(df, type){
  if (type == 'session') {
    df <- df %>% 
      rename(
        medium = sessionMedium,
        source = sessionSource,
        defaultChannelGroup = sessionDefaultChannelGroup
      )
  } 
  
  df <- df %>% 
    mutate(
      pageTitle = gsub(' - RMI', '', pageTitle),
      source = tolower(source),
      medium = tolower(medium),
      pageReferrer = tolower(pageReferrer),
      
      # --- Normalize email and social channels ---
      medium = ifelse(
        grepl('mail.google.com', source) | grepl('web-email|sf|outlook', medium),
        'email', medium
      ),
      
      source = case_when(
        grepl('linkedin|lnkd.in|li', source) ~ 'linkedin',
        grepl('facebook|fb|meta', source) ~ 'facebook',
        grepl('(^x$)|x.com|twitter|t.co|dlvr.it', source) ~ 'twitter',   # 👈 merge X + Twitter here
        grepl('instagram|instagr.am', source) ~ 'instagram',
        TRUE ~ source
      ),
      
      medium = case_when(
        grepl('linkedin|facebook|twitter|instagram', source) ~ 'social',
        grepl('twitter|fbdvby', medium) ~ 'social',
        grepl('/t.co/', pageReferrer) ~ 'social',
        TRUE ~ medium
      ),
      
      source = ifelse(grepl('/t.co/', pageReferrer), 'twitter', source),
      
      defaultChannelGroup = ifelse(
        medium == 'social', 'Organic Social',
        ifelse(medium == 'email', 'Email', defaultChannelGroup)
      )
    )
  
  return(df)
}



#' get page traffic (#' sessions) driven by social media channels
#' #add campaignID as an argument being passed in
getTrafficSocial <- function(propertyID, pages, campaignID, site = 'rmi.org'){
  print(paste("Inside getTrafficSocial with campaignID:", campaignID))  # Debugging
  
  aquisitionSocial <- ga_data(
    propertyID,
    metrics = c("sessions", "screenPageViews"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", 'pageReferrer', 'sessionDefaultChannelGroup'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  aquisitionSocial <- correctTraffic(aquisitionSocial, type = 'session') %>% 
    filter(medium == 'social') %>% 
    dplyr::group_by(pageTitle, source) %>% 
    dplyr::summarize(Sessions = sum(sessions),
                     PageViews = sum(screenPageViews)) %>% 
    mutate(site = site,
           dashboardCampaign = campaignID)
  # was campaignID
  
  return(aquisitionSocial)
}

getTrafficSocialWithTags <- function(propertyID, pageAndTag, site = 'rmi.org') {
  
  # Define custom date ranges per campaign tag
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10"),
    nycw24 = c("2024-09-01", "2024-10-05"),
    nycw25 = c("2025-08-31", "2025-10-04"),
    cera25 = c("2025-03-07", "2025-04-11"),
    cop29 = c("2024-11-11", "2024-12-16"),
    cop28 = c("2023-11-30", "2023-12-30")
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  # Clean up tag formatting
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allSocial <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])
    
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    message(paste0("Processing page: ", page))
    message(paste("Using date range:", paste(date_range, collapse = " to ")))
    
    socialData <- tryCatch({
      ga_data(
        propertyID,
        metrics = c("sessions", "screenPageViews"),
        dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", "sessionDefaultChannelGroup"),
        date_range = date_range,
        dim_filters = ga_data_filter("pageTitle" == page),
        limit = -1
      ) %>%
        arrange(pageTitle)
    }, error = function(e) {
      message(paste("❌ GA4 error for", page, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(socialData)) {
      traffic <- correctTraffic(socialData, type = "session") %>%
        filter(medium == "social") %>%
        group_by(pageTitle, source) %>%
        summarize(
          Sessions = sum(sessions),
          PageViews = sum(screenPageViews),
          .groups = "drop"
        ) %>%
        mutate(
          campaign_tag = tag,
          startDate = date_range[1],
          endDate = date_range[2],
          site = site
        )
      
      allSocial <- bind_rows(allSocial, traffic)
    }
  }
  
  return(allSocial)
}



# New and improved.
getTrafficSocial2 <- function(propertyID, pages, campaignPages, site = 'rmi.org'){
  print(paste("Inside getTrafficSocial with campaignID:", campaignID))  # Debugging
  
  aquisitionSocial <- ga_data(
    propertyID,
    metrics = c("sessions", "screenPageViews"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", 'pageReferrer', 'sessionDefaultChannelGroup'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  aquisitionSocial <- correctTraffic(aquisitionSocial, type = 'session') %>% 
    filter(medium == 'social') %>% 
    dplyr::group_by(pageTitle, source) %>% 
    dplyr::summarize(Sessions = sum(sessions),
                     PageViews = sum(screenPageViews)) %>% 
    left_join(
      campaignPages %>%
        select(pageTitle, campaign_tag) %>%
        distinct(pageTitle, .keep_all = TRUE), 
      by = "pageTitle"
    ) %>%
    rename(dashboardCampaign = campaign_tag) %>%
    mutate(site = site) %>%
    select(pageTitle, source, Sessions, PageViews, site, dashboardCampaign)

  return(aquisitionSocial)
}


getTrafficSocial3 <- function(propertyID, pages, campaignPages, site = 'rmi.org') {
  print(paste("Inside getTrafficSocial with propertyID:", propertyID))  # Debugging
  
  # Adjust date range if "make a gift" is included
  if ("Make a Gift - RMI" %in% pages) {
    date_range <- c("2024-01-01", "2024-12-31")  # Custom date range for "make a gift"
  } else {
    date_range <- dateRangeGA  # Default date range
  }
  
  acquisitionSocial <- ga_data(
    propertyID,
    metrics = c("sessions", "screenPageViews"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", "sessionDefaultChannelGroup"),
    date_range = date_range,  # Apply the adjusted date range
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>%
    arrange(pageTitle)
  
  acquisitionSocial <- correctTraffic(acquisitionSocial, type = 'session') %>%
    filter(medium == "social") %>%
    dplyr::group_by(pageTitle, source) %>%
    dplyr::summarize(
      Sessions = sum(sessions),
      PageViews = sum(screenPageViews),
      .groups = "drop"
    ) %>%
    left_join(
      campaignPages %>%
        select(pageTitle, campaign_tag) %>%
        distinct(pageTitle, .keep_all = TRUE), 
      by = "pageTitle"
    ) %>%
    rename(dashboardCampaign = campaign_tag) %>%
    mutate(site = site) %>%
    select(pageTitle, source, Sessions, PageViews, site, dashboardCampaign)
  
  return(acquisitionSocial)
}



# # new social function
# getTrafficSocial2 <- function(propertyID, pages_df, site = 'rmi.org'){
#   # Ensure pages_df has columns 'pageTitle' and 'campaignID'
#   
#   allResults <- list()  # Initialize a list to store results
#   
#   for (i in seq_len(nrow(pages_df))) {
#     page <- pages_df$pageTitle[i]
#     campaignID <- pages_df$campaignID[i]
#     
#     print(paste("Processing page:", page, "with campaignID:", campaignID))  # Debugging
#     
#     aquisitionSocial <- ga_data(
#       propertyID,
#       metrics = c("sessions", "screenPageViews"),
#       dimensions = c("pageTitle", "sessionSource", "sessionMedium", 'pageReferrer', 'sessionDefaultChannelGroup'),
#       date_range = dateRangeGA,
#       dim_filters = ga_data_filter("pageTitle" == page),
#       limit = -1
#     ) %>% 
#       arrange(pageTitle) 
#     
#     aquisitionSocial <- correctTraffic(aquisitionSocial, type = 'session') %>% 
#       filter(medium == 'social') %>% 
#       dplyr::group_by(pageTitle, source) %>% 
#       dplyr::summarize(Sessions = sum(sessions),
#                        PageViews = sum(screenPageViews)) %>% 
#       mutate(site = site,
#              dashboardCampaign = campaignID)
#     
#     allResults[[i]] <- aquisitionSocial  # Store the result in the list
#   }
#   
#   # Combine all results into one dataframe
#   result_df <- do.call(rbind, allResults)
#   
#   return(result_df)
# }

#' get page views broken down by country and region
getTrafficGeography <- function(propertyID, pages, site = 'rmi.org'){
  
  trafficByRegion <- ga_data(
    propertyID,
    metrics = c('screenPageViews'),
    dimensions = c("pageTitle", 'region', 'country'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    #' filter out regions where page views < 5
    filter(screenPageViews > 4) %>% 
    arrange(pageTitle) %>% 
    dplyr::rename('Region Page Views' = screenPageViews) %>% 
    mutate(pageTitle = gsub(' - RMI', '', pageTitle),
           site = site, 
           dashboardCampaign = campaignID) %>%
  
  return(trafficByRegion)
}


getTrafficGeographyWithTags <- function(propertyID, pageAndTag, site = 'rmi.org') {
  
  # Define custom campaign-specific date ranges
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10"),
    nycw24 = c("2024-09-01", "2024-10-05"),
    nycw25 = c("2025-08-31", "2025-10-04"),
    cera25 = c("2025-03-07", "2025-04-11"),
    cop29 = c("2024-11-11", "2024-12-16"),
    cop28 = c("2023-11-30", "2023-12-30")
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  # Clean up campaign tag values
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allGeo <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])
    
    # Assign appropriate date range
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    message(paste0("Processing page: ", page))
    message(paste("Using date range:", paste(date_range, collapse = " to ")))
    
    geoData <- tryCatch({
      ga_data(
        propertyID,
        metrics = c("screenPageViews"),
        dimensions = c("pageTitle", "region", "country"),
        date_range = date_range,
        dim_filters = ga_data_filter("pageTitle" == page),
        limit = -1
      )
    }, error = function(e) {
      message(paste("❌ GA4 error for", page, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(geoData)) {
      cleaned <- geoData %>%
        filter(screenPageViews > 4) %>%
        arrange(pageTitle) %>%
        rename(`Region Page Views` = screenPageViews) %>%
        mutate(
          campaign_tag = tag,
          startDate = date_range[1],
          endDate = date_range[2],
          pageTitle = gsub(" - RMI", "", pageTitle),
          site = site
        )
      
      allGeo <- bind_rows(allGeo, cleaned)
    }
  }
  
  return(allGeo)
}

# it should NOT be having only one instance per location

getTrafficGeography2 <- function(propertyID, pages, campaignPages, site = 'rmi.org'){
  
  trafficByRegion <- ga_data(
    propertyID,
    metrics = c('screenPageViews'),
    dimensions = c("pageTitle", 'region', 'country'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    #' filter out regions where page views < 5
    filter(screenPageViews > 4) %>% 
    arrange(pageTitle) %>% 
    dplyr::rename('Region Page Views' = screenPageViews) %>% 
    mutate(pageTitle = gsub(' - RMI', '', pageTitle),
           site = site) %>%
    #distinct(pageTitle, region, .keep_all = TRUE) %>%
    left_join(
      campaignPages %>%
        select(pageTitle, campaign_tag) %>%
        distinct(pageTitle, .keep_all = TRUE), 
      by = "pageTitle"
    ) %>%
    rename(dashboardCampaign = campaign_tag) %>%
    
  
  return(trafficByRegion)
}

getTrafficGeography3 <- function(propertyID, pages, campaignPages, site = 'rmi.org') {
  
  # Adjust date range if "make a gift" is included
  if ("Make a Gift - RMI" %in% pages) {
    date_range <- c("2024-01-01", "2024-12-31")  # Custom date range for "make a gift"
  } else {
    date_range <- dateRangeGA  # Default date range
  }
  
  trafficByRegion <- ga_data(
    propertyID,
    metrics = c("screenPageViews"),
    dimensions = c("pageTitle", "region", "country"),
    date_range = date_range,  # Apply the adjusted date range
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    # Filter out regions where page views < 5
    filter(screenPageViews > 4) %>% 
    arrange(pageTitle) %>% 
    dplyr::rename("Region Page Views" = screenPageViews) %>% 
    mutate(pageTitle = gsub(" - RMI", "", pageTitle),
           site = site) %>%
    left_join(
      campaignPages %>%
        select(pageTitle, campaign_tag) %>%
        distinct(pageTitle, .keep_all = TRUE), 
      by = "pageTitle"
    ) %>%
    rename(dashboardCampaign = campaign_tag)
  
  return(trafficByRegion)
}


#' get sessions and conversions attributions for acquisition channels (organic, email, social, paid ads, etc.)
#' sessions and conversions use different dimensions so make separate calls for each then bind rows
getAcquisition <- function(propertyID, pages, site = 'rmi.org'){
  
  #' 1) get sessions
  aquisitionSessions <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", 'sessionDefaultChannelGroup'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  acquisition <- correctTraffic(aquisitionSessions, 'session') %>% 
    mutate(defaultChannelGroup = case_when(defaultChannelGroup == 'Organic Video' ~ 'Organic Search',
                                           defaultChannelGroup == 'Organic Social' ~ 'Social Media',
                                           TRUE ~ defaultChannelGroup)) %>%
    group_by(pageTitle, defaultChannelGroup) %>% 
    summarize(Sessions = sum(sessions))

  if(site == 'rmi.org'){
    
    #' 2) get conversions
    aquisitionConversions <- ga_data(
      propertyID,
      metrics = c('conversions:emailFormSubmit', 'conversions:downloadThankYou', 'conversions:donatePageView_Embedded',
                  'conversions:registerThankYou'), # add new custom conversions here
      dimensions = c("pageTitle", "source", "medium", 'pageReferrer', 'defaultChannelGroup'),
      date_range = dateRangeGA,
      dim_filters = ga_data_filter("pageTitle" == pages),
      limit = -1
    ) %>% 
      select(pageTitle, source, medium, pageReferrer, defaultChannelGroup, 
             form_submit = 'conversions:emailFormSubmit', download = 'conversions:downloadThankYou', 
             donate_view = 'conversions:donatePageView_Embedded', event_register = 'conversions:registerThankYou') %>% # Add new conversions in select function
      arrange(pageTitle)
    
    aquisitionConversions <- correctTraffic(aquisitionConversions, 'conversion') %>% 
      group_by(pageTitle, defaultChannelGroup) %>% 
      summarize('Downloads' = sum(download),
                       'Form Submissions' = sum(form_submit), 'DonationPageViews' = sum(donate_view),
                'EventRegistered' = sum(event_register)) # add aggregation for new conversions here 
    
    #' 3) bind sessions + conversions 
    acquisition <- acquisition %>% 
      left_join(aquisitionConversions, by = c('pageTitle', 'defaultChannelGroup')) 
    
  }
  
  acquisition <- acquisition %>% 
    mutate(site = site, pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(acquisition)
  
}

getAcquisitionWithTags <- function(propertyID, pageAndTag, site = 'rmi.org') {
  
  # Define campaign-specific date ranges
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10")
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  # Clean campaign tags
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allAcquisition <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])
    
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    message(paste("Processing page:", page))
    
    # Sessions
    sessions <- tryCatch({
      ga_data(
        propertyID,
        metrics = c("sessions"),
        dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", "sessionDefaultChannelGroup"),
        date_range = date_range,
        dim_filters = ga_data_filter("pageTitle" == page),
        limit = -1
      ) %>%
        arrange(pageTitle)
    }, error = function(e) {
      message(paste("❌ Sessions error for", page, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(sessions)) {
      sessions <- correctTraffic(sessions, 'session') %>%
        mutate(defaultChannelGroup = case_when(
          defaultChannelGroup == "Organic Video" ~ "Organic Search",
          defaultChannelGroup == "Organic Social" ~ "Social Media",
          TRUE ~ defaultChannelGroup
        )) %>%
        group_by(pageTitle, defaultChannelGroup) %>%
        summarize(Sessions = sum(sessions), .groups = "drop")
    }
    
    # Conversions
    conversions <- NULL
    if (site == "rmi.org") {
      conversions <- tryCatch({
        ga_data(
          propertyID,
          metrics = c("conversions:emailFormSubmit", "conversions:downloadThankYou", 
                      "conversions:donatePageView_Embedded", "conversions:registerThankYou"),
          dimensions = c("pageTitle", "source", "medium", "pageReferrer", "defaultChannelGroup"),
          date_range = date_range,
          dim_filters = ga_data_filter("pageTitle" == page),
          limit = -1
        ) %>%
          select(
            pageTitle, source, medium, pageReferrer, defaultChannelGroup,
            form_submit = `conversions:emailFormSubmit`,
            download = `conversions:downloadThankYou`,
            donate_view = `conversions:donatePageView_Embedded`,
            event_register = `conversions:registerThankYou`
          ) %>%
          arrange(pageTitle)
      }, error = function(e) {
        message(paste("⚠️ Conversions error for", page, ":", e$message))
        return(NULL)
      })
      
      if (!is.null(conversions)) {
        conversions <- correctTraffic(conversions, 'conversion') %>%
          group_by(pageTitle, defaultChannelGroup) %>%
          summarize(
            Downloads = sum(download),
            FormSubmissions = sum(form_submit),
            DonationPageViews = sum(donate_view),
            EventRegistered = sum(event_register),
            .groups = "drop"
          )
      }
    }
    
    # Merge sessions + conversions
    if (!is.null(sessions)) {
      acq <- sessions
      
      if (!is.null(conversions)) {
        acq <- left_join(acq, conversions, by = c("pageTitle", "defaultChannelGroup"))
      }
      
      acq <- acq %>%
        mutate(
          campaign_tag = tag,
          startDate = date_range[1],
          endDate = date_range[2],
          site = site,
          pageTitle = gsub(" - RMI", "", pageTitle)
        )
      
      allAcquisition <- bind_rows(allAcquisition, acq)
    }
  }
  
  return(allAcquisition)
}

getAcquisitionWithTags2 <- function(propertyID, pageAndTag, site = 'rmi.org') {
  
  # Define campaign-specific date ranges
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10"),
    nycw24 = c("2024-09-01", "2024-10-05"),
    nycw25 = c("2025-08-31", "2025-10-04"),
    cera25 = c("2025-03-07", "2025-04-11"),
    cop29 = c("2024-11-11", "2024-12-16"),
    cop28 = c("2023-11-30", "2023-12-30")
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  # Clean campaign tags (still good to keep)
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allAcquisition <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    # Use full pageTitle (including ' - RMI') to match GA data properly
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])
    
    # Output diagnostic info to check page title filter
    message(paste("GA query pageTitle filter:", page))
    
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    # Sessions
    sessions <- tryCatch({
      ga_data(
        propertyID,
        metrics = c("sessions"),
        dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", "sessionDefaultChannelGroup"),
        date_range = date_range,
        dim_filters = ga_data_filter("pageTitle" == page),
        limit = -1
      ) %>%
        arrange(pageTitle)
    }, error = function(e) {
      message(paste("❌ Sessions error for", page, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(sessions)) {
      sessions <- correctTraffic(sessions, 'session') %>%
        mutate(defaultChannelGroup = case_when(
          defaultChannelGroup == "Organic Video" ~ "Organic Search",
          defaultChannelGroup == "Organic Social" ~ "Social Media",
          TRUE ~ defaultChannelGroup
        )) %>%
        group_by(pageTitle, defaultChannelGroup) %>%
        summarize(Sessions = sum(sessions), .groups = "drop")
    }
    
    # Conversions
    conversions <- NULL
    if (site == "rmi.org") {
      conversions <- tryCatch({
        ga_data(
          propertyID,
          metrics = c("conversions:emailFormSubmit", "conversions:downloadThankYou", 
                      "conversions:donatePageView_Embedded", "conversions:registerThankYou"),
          dimensions = c("pageTitle", "source", "medium", "pageReferrer", "defaultChannelGroup"),
          date_range = date_range,
          dim_filters = ga_data_filter("pageTitle" == page),
          limit = -1
        ) %>%
          select(
            pageTitle, source, medium, pageReferrer, defaultChannelGroup,
            form_submit = `conversions:emailFormSubmit`,
            download = `conversions:downloadThankYou`,
            donate_view = `conversions:donatePageView_Embedded`,
            event_register = `conversions:registerThankYou`
          ) %>%
          arrange(pageTitle)
      }, error = function(e) {
        message(paste("⚠️ Conversions error for", page, ":", e$message))
        return(NULL)
      })
      
      if (!is.null(conversions)) {
        conversions <- correctTraffic(conversions, 'conversion') %>%
          group_by(pageTitle, defaultChannelGroup) %>%
          summarize(
            Downloads = sum(download),
            FormSubmissions = sum(form_submit),
            DonationPageViews = sum(donate_view),
            EventRegistered = sum(event_register),
            .groups = "drop"
          )
      }
    }
    
    # Merge sessions + conversions
    if (!is.null(sessions)) {
      acq <- sessions
      
      if (!is.null(conversions)) {
        acq <- left_join(acq, conversions, by = c("pageTitle", "defaultChannelGroup"))
      }
      
      # Clean the pageTitle here after joining
      acq <- acq %>%
        mutate(
          campaign_tag = tag,
          startDate = date_range[1],
          endDate = date_range[2],
          site = site,
          pageTitle = gsub(" - RMI", "", pageTitle)
        )
      
      allAcquisition <- bind_rows(allAcquisition, acq)
    }
  }
  
  return(allAcquisition)
}

getAcquisition2 <- function(propertyID, pages, site = 'rmi.org'){
  
  # This page needs a shorter date range to capture only Fall Appeal data
  donations_page <- "Make a Gift - RMI"  
  
  # Date range for Fall Appeal
  shortDateRangeGA <- c("2024-11-01", "2025-02-10")  # Replace with actual dates
  
  # Default date range
  dateRangeGA <- c("2023-01-01", as.character(Sys.Date()))  # Ensure correct format
  
  # Determine appropriate date range
  date_range_used <- if (donations_page %in% pages) shortDateRangeGA else dateRangeGA
  
  #' 1) Get Sessions
  acquisitionSessions <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", 'sessionDefaultChannelGroup'),
    date_range = date_range_used,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  acquisition <- correctTraffic(acquisitionSessions, 'session') %>% 
    mutate(defaultChannelGroup = case_when(
      defaultChannelGroup == 'Organic Video' ~ 'Organic Search',
      defaultChannelGroup == 'Organic Social' ~ 'Social Media',
      TRUE ~ defaultChannelGroup
    )) %>%
    group_by(pageTitle, defaultChannelGroup) %>% 
    summarize(Sessions = sum(sessions, na.rm = TRUE))
  
  if(site == 'rmi.org'){
    
    #' 2) Get Conversions
    acquisitionConversions <- ga_data(
      propertyID,
      metrics = c('conversions:emailFormSubmit', 'conversions:downloadThankYou', 'conversions:donatePageView_Embedded',
                  'conversions:registerThankYou'),
      dimensions = c("pageTitle", "source", "medium", 'pageReferrer', 'defaultChannelGroup'),
      date_range = date_range_used,
      dim_filters = ga_data_filter("pageTitle" == pages),
      limit = -1
    ) %>% 
      select(pageTitle, source, medium, pageReferrer, defaultChannelGroup, 
             form_submit = 'conversions:emailFormSubmit', 
             download = 'conversions:downloadThankYou', 
             donate_view = 'conversions:donatePageView_Embedded', 
             event_register = 'conversions:registerThankYou') %>% 
      arrange(pageTitle)
    
    acquisitionConversions <- correctTraffic(acquisitionConversions, 'conversion') %>% 
      group_by(pageTitle, defaultChannelGroup) %>% 
      summarize(
        Downloads = sum(download, na.rm = TRUE),  
        Form_Submissions = sum(form_submit, na.rm = TRUE), 
        DonationPageViews = sum(donate_view, na.rm = TRUE),
        EventRegistered = sum(event_register, na.rm = TRUE)
      )
    
    #' 3) Bind Sessions + Conversions 
    acquisition <- acquisition %>% 
      left_join(acquisitionConversions, by = c('pageTitle', 'defaultChannelGroup')) 
  }
  
  acquisition <- acquisition %>% 
    mutate(site = site, pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(acquisition)
}



#' get page traffic (#sessions) driven by referral sources that have been identified as “Media” 
#' these sources are defined in the referralSites file
getReferrals <- function(propertyID, pages, site = 'rmi.org'){
  
  referrals <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer"),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) %>% 
    group_by(pageTitle, sessionSource) %>% 
    filter(sessionMedium == 'referral') %>% 
    inner_join(select(referralSites, c(media, sessionSource, mediaType, mediaSubtype)), by = 'sessionSource') %>% 
    mutate(referrer = sub('(.*)https://', '', pageReferrer),
           referrer = sub('/(.*)', '', referrer)) %>% 
    filter(referrer != 'rmi.org') %>% 
    group_by(pageTitle, sessionSource, media, mediaType, mediaSubtype) %>% 
    summarise(sessions = sum(sessions)) %>% 
    filter(sessions > 2) %>% 
    mutate(site = site,
           pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(referrals)
  
}


getReferralsWithTag <- function(propertyID, pageAndTag, site = 'rmi.org') {
  
  # Define campaign-specific date ranges
  customDateRanges <- list(
    sapp25 = c("2025-03-30", "2025-06-30"),
    fapp24 = c("2024-11-01", "2025-02-10"),
    nycw24 = c("2024-09-01", "2024-10-05"),
    nycw25 = c("2025-08-31", "2025-10-04"),
    cera25 = c("2025-03-07", "2025-04-11"),
    cop29 = c("2024-11-11", "2024-12-16"),
    cop28 = c("2023-11-30", "2023-12-30")
  )
  
  defaultDateRange <- c("2023-01-01", as.character(Sys.Date()))
  
  pageAndTag <- pageAndTag %>%
    mutate(campaign_tag = na_if(trimws(campaign_tag), ""))
  
  allReferrals <- data.frame()
  
  for (i in 1:nrow(pageAndTag)) {
    page <- pageAndTag$pageTitle[i]
    tag <- tolower(pageAndTag$campaign_tag[i])
    
    date_range <- if (!is.null(tag) && !is.na(tag) && tag %in% names(customDateRanges)) {
      customDateRanges[[tag]]
    } else {
      defaultDateRange
    }
    
    message(paste("Processing page:", page))
    message(paste("Using date range:", paste(date_range, collapse = " to ")))
    
    referralData <- tryCatch({
      ga_data(
        propertyID,
        metrics = c("sessions"),
        dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer"),
        date_range = date_range,
        dim_filters = ga_data_filter("pageTitle" == page),
        limit = -1
      )
    }, error = function(e) {
      message(paste("❌ Error processing", page, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(referralData) && nrow(referralData) > 0) {
      cleaned <- referralData %>%
        filter(sessionMedium == "referral") %>%
        inner_join(select(referralSites, media, sessionSource, mediaType, mediaSubtype), by = "sessionSource") %>%
        mutate(
          referrer = sub("(.*)https://", "", pageReferrer),
          referrer = sub("/(.*)", "", referrer)
        ) %>%
        filter(referrer != "rmi.org") %>%
        group_by(pageTitle, sessionSource, media, mediaType, mediaSubtype) %>%
        summarise(sessions = sum(sessions), .groups = "drop") %>%
        filter(sessions > 2) %>%
        mutate(
          pageTitle = gsub(" - RMI", "", pageTitle),
          campaign_tag = tag,
          startDate = date_range[1],
          endDate = date_range[2],
          site = site
        )
      
      allReferrals <- bind_rows(allReferrals, cleaned)
    } else {
      message(paste("⚠️ No referral data found for:", page))
    }
  }
  
  return(allReferrals)
}



getAcquisition2 <- function(propertyID, pages, site = 'rmi.org'){
  
  # This page needs a shorter date range to capture only Fall Appeal data
  donations_page <- "Make a Gift - RMI"  
  
  # Date range for Fall Appeal
  shortDateRangeGA <- c("2024-11-01", "2025-02-10")  # Replace with actual dates
  
  # Default date range
  dateRangeGA <- c("2023-01-01", as.character(Sys.Date()))  # Ensure correct format
  
  # Determine appropriate date range
  if (donations_page %in% pages) {
    date_range_used <- shortDateRangeGA
  } else {
    date_range_used <- dateRangeGA
  }
  
  #' 1) Get Sessions
  acquisitionSessions <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", 'sessionDefaultChannelGroup'),
    date_range = date_range_used,  # Always a valid 2-element vector
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  acquisition <- correctTraffic(acquisitionSessions, 'session') %>% 
    mutate(defaultChannelGroup = case_when(defaultChannelGroup == 'Organic Video' ~ 'Organic Search',
                                           defaultChannelGroup == 'Organic Social' ~ 'Social Media',
                                           TRUE ~ defaultChannelGroup)) %>%
    group_by(pageTitle, defaultChannelGroup) %>% 
    summarize(Sessions = sum(sessions))
  
  if(site == 'rmi.org'){
    
    #' 2) Get Conversions
    acquisitionConversions <- ga_data(
      propertyID,
      metrics = c('conversions:emailFormSubmit', 'conversions:downloadThankYou', 'conversions:donatePageView_Embedded',
                  'conversions:registerThankYou'),
      dimensions = c("pageTitle", "source", "medium", 'pageReferrer', 'defaultChannelGroup'),
      date_range = date_range_used,  # Always a valid 2-element vector
      dim_filters = ga_data_filter("pageTitle" == pages),
      limit = -1
    ) %>% 
      select(pageTitle, source, medium, pageReferrer, defaultChannelGroup, 
             form_submit = 'conversions:emailFormSubmit', download = 'conversions:downloadThankYou', 
             donate_view = 'conversions:donatePageView_Embedded', event_register = 'conversions:registerThankYou') %>% 
      arrange(pageTitle)
    
    acquisitionConversions <- correctTraffic(acquisitionConversions, 'conversion') %>% 
      group_by(pageTitle, defaultChannelGroup) %>% 
      summarize('Downloads' = sum(download),
                'Form Submissions' = sum(form_submit), 
                'DonationPageViews' = sum(donate_view),
                'EventRegistered' = sum(event_register)) 
    
    #' 3) Bind Sessions + Conversions 
    acquisition <- acquisition %>% 
      left_join(acquisitionConversions, by = c('pageTitle', 'defaultChannelGroup')) 
  }
  
  acquisition <- acquisition %>% 
    mutate(site = site, pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(acquisition)
}
getAcquisition2 <- function(propertyID, pages, site = 'rmi.org'){
  
  # This page needs a shorter date range to capture only Fall Appeal data
  donations_page <- "Make a Gift - RMI"  
  
  # Date range for Fall Appeal
  shortDateRangeGA <- c("2024-11-01", "2025-02-10")  # Replace with actual dates
  
  # Default date range
  dateRangeGA <- c("2023-01-01", as.character(Sys.Date()))  # Ensure correct format
  
  # Determine appropriate date range
  date_range_used <- if (donations_page %in% pages) shortDateRangeGA else dateRangeGA
  
  #' 1) Get Sessions
  acquisitionSessions <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle", "sessionSource", "sessionMedium", "pageReferrer", 'sessionDefaultChannelGroup'),
    date_range = date_range_used,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  acquisition <- correctTraffic(acquisitionSessions, 'session') %>% 
    mutate(defaultChannelGroup = case_when(
      defaultChannelGroup == 'Organic Video' ~ 'Organic Search',
      defaultChannelGroup == 'Organic Social' ~ 'Social Media',
      TRUE ~ defaultChannelGroup
    )) %>%
    group_by(pageTitle, defaultChannelGroup) %>% 
    summarize(Sessions = sum(sessions, na.rm = TRUE))
  
  if(site == 'rmi.org'){
    
    #' 2) Get Conversions
    acquisitionConversions <- ga_data(
      propertyID,
      metrics = c('conversions:emailFormSubmit', 'conversions:downloadThankYou', 'conversions:donatePageView_Embedded',
                  'conversions:registerThankYou'),
      dimensions = c("pageTitle", "source", "medium", 'pageReferrer', 'defaultChannelGroup'),
      date_range = date_range_used,
      dim_filters = ga_data_filter("pageTitle" == pages),
      limit = -1
    ) %>% 
      select(pageTitle, source, medium, pageReferrer, defaultChannelGroup, 
             form_submit = 'conversions:emailFormSubmit', 
             download = 'conversions:downloadThankYou', 
             donate_view = 'conversions:donatePageView_Embedded', 
             event_register = 'conversions:registerThankYou') %>% 
      arrange(pageTitle)
    
    acquisitionConversions <- correctTraffic(acquisitionConversions, 'conversion') %>% 
      group_by(pageTitle, defaultChannelGroup) %>% 
      summarize(
        Downloads = sum(download, na.rm = TRUE),  
        Form_Submissions = sum(form_submit, na.rm = TRUE), 
        DonationPageViews = sum(donate_view, na.rm = TRUE),
        EventRegistered = sum(event_register, na.rm = TRUE)
      )
    
    #' 3) Bind Sessions + Conversions 
    acquisition <- acquisition %>% 
      left_join(acquisitionConversions, by = c('pageTitle', 'defaultChannelGroup')) 
  }
  
  acquisition <- acquisition %>% 
    mutate(site = site, pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(acquisition)
}


#### NEWSLETTERS ####

##' get newsletter stats

#' get all Spark and Market Catalyst newsletters
getAllEmailStats <- function(){
  
  #' get Spark newsletters
  emailStatsSpark <- read_csv('allSparkStats.csv', show_col_types = FALSE) %>% 
    mutate(date = as.Date(date))
    #remove this -- now bc of SF incompatibility 
   # filter(!str_detect(id, "a6hQk")) 

  
  #' get Market Catalyst newsletters
  emailStatsPEM <- read_csv('allMCStats.csv', show_col_types = FALSE) %>% 
    mutate(date = as.Date(date))
  
  #' bind - would need to bind scandinavia house emails here
  allEmailStats <- emailStatsSpark %>% 
    plyr::rbind.fill(emailStatsPEM) %>% 
    mutate(date = as.Date(date),
           #' change name format
           name = ifelse(grepl('Spark', name), paste0(date, ': Spark'), paste0(date, ':', sub('(.*)-[0-9]{2}', '', name))))
        #   id = as.numeric(id))
  
  return(allEmailStats)
}

cleanURL <- function(url) {
  url %>%
    stringr::str_remove("\\?.*") %>%     # Remove query string
    stringr::str_remove("#.*") %>%       # Remove fragment (if any)
    stringr::str_replace("/?$", "/")     # Ensure exactly one trailing slash
}


#' filter all newsletters data frame by page URLs
getCampaignEmails <- function(pageURLs){
  
  #' filter each of the 3 "story_url" columns for campaign web page URLs
  # df1 <- allEmailStats %>% select(c('id':'COR_S1')) %>% 
  #   rename(story_url = url_1,
  #          story_title = title_1,
  #          story_clicks = clicks_1, 
  #          story_COR = COR_S1)
  df1 <- allEmailStats %>% select(c('id':'COR')) %>% 
    mutate(url = cleanURL(url)) %>%
    rename(story_url = url,
           story_title = title,
           story_clicks = clicks, 
           story_COR = COR)
  
  # 
  # testing_filtered_rows <- df1 %>%
  #   filter(name == "2024-09-05: Spark" & story_title == "RMI at Climate Week 2024")
  # 
  # # Print the filtered rows
  # print(testing_filtered_rows)
  

  # df2 <- allEmailStats %>% select(c('id':'plaintext_rate', 'url_2':'COR_S2'))
  # names(df2) <- names(df1)
  # 
  # df3 <- allEmailStats %>% select(c('id':'plaintext_rate', 'url_3':'COR_S3'))
  # names(df3) <- names(df1)
  # 
  # df4 <- allEmailStats %>% select(c('id':'plaintext_rate', 'url_4':'COR_S4'))
  # names(df4) <- names(df1)
  
  #' bind matches
  # allStoryStats <- df1 %>% 
  #   rbind(df2) %>% 
  #   rbind(df3) %>% 
  #   filter(grepl(paste(pageURLs, collapse = '|'), story_url)) %>% 
  #   mutate(date = as.Date(date),
  #          story_title = gsub(' - RMI', '', story_title))
  
  allStoryStats <- df1 %>% 
#    mutate(story_url = cleanURL(story_url)) %>%
    filter(grepl(paste(pageURLs, collapse = '|'), story_url)) %>% 
    mutate(date = as.Date(date),
           story_title = gsub(' - RMI', '', story_title)) %>%
    # this line needs to change, wasn't delivered before
   # distinct(name, story_title, delivered, .keep_all = TRUE)
    group_by(name, id, story_url, story_title, date) %>%
    mutate(story_clicks = sum(story_clicks, na.rm = TRUE)) %>%
    slice(1) %>%  # keep only one representative row, now that clicks are summed
    ungroup()
  
  #' order by date and keep relevant columns
  allStoryStats <- allStoryStats[rev(order(allStoryStats$date)),] %>% 
    select(id, name, date, delivered_ = delivered, unique_opens, open_rate, unique_clicks,
           unique_CTR, UCTRvsAvg, story_url, story_title, story_clicks, story_COR)
  
  return(allStoryStats)
}

#n_distinct(allStoryStats$story_url)


##' get individual newsletter clicks - for Salesforce campaign member data frame

#' make a GET call through Pardot API
get <- function(url, header) {
  request <- GET(url, add_headers(.headers = header))
  response <- jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8"))
}

#' get individual link clicks from a list email
#' - limit of 200 rows returned are returned at a time so use the nextDate field to get the next page of results
#' - break loop when query returns a data frame with < 200 rows
getBatchClicks <- function(emailId, df){
  allClicks <- data.frame(id = '', prospect_id = '', url = '', list_email_id = '', email_template_id = '', created_at = '')[0,]
  
  for(i in 1:25){
    if(i == 1){
      email_clicks <- GET(paste0("https://pi.pardot.com/api/emailClick/version/4/do/query?format=json&list_email_id=", df[emailId, 'id']),
                          add_headers(.headers = header4))
      getClicks <- jsonlite::fromJSON(content(email_clicks, as = "text", encoding = "UTF-8"))
      clicksQuery <- getClicks[["result"]][["emailClick"]]
      
      #new
      # clicksQuery <- clicksQuery %>%
      #   mutate(across(c(id, prospect_id, list_email_id, email_template_id), as.character))
      # 

      digit <- sub('(.*) ', '', str_match(clicksQuery[200, 'created_at'], " *(.*?)\\s*(:)")[,2])
      if(grepl('^0', digit)){ hour <- '%2' } else { hour <- '%20' }
      nextDate <- gsub(' ', hour, clicksQuery[200, 'created_at'])
      
      allClicks <- allClicks %>% rbind(clicksQuery)
      #allClicks <- dplyr::bind_rows(allClicks, clicksQuery)
      
    } else {
      email_clicks <- GET(paste0("https://pi.pardot.com/api/emailClick/version/4/do/query?format=json&created_after=", nextDate, "&list_email_id=", df[emailId, 'id']),
                          add_headers(.headers = header5))
      getClicks <- jsonlite::fromJSON(content(email_clicks, as = "text", encoding = "UTF-8"))
      clicksQuery <- as.data.frame(getClicks[["result"]][["emailClick"]])
      
      allClicks <- allClicks %>% rbind(clicksQuery)
     # allClicks <- dplyr::bind_rows(allClicks, clicksQuery)
      
      if (nrow(clicksQuery) < 200) break
      
      digit <- sub('(.*) ', '', str_match(clicksQuery[200, 'created_at'], " *(.*?)\\s*(:)")[,2])
      if(grepl('^0', digit)){ hour <- '%2' } else { hour <- '%20' }
      nextDate <- gsub(' ', hour, clicksQuery[200, 'created_at'])
      
    }
  }
  return(allClicks)
}

#' apply getBatchClicks to a list of emailIDs
#' - returns df with all link clicks attached to those emails
getProspectClicksOld <- function(df){
  clicksTotal <- data.frame(id = '', prospect_id = '', url = '', list_email_id = '', email_template_id = '', created_at = '')[0,]
  
  for(i in 1:nrow(df)){
    clicks <- getBatchClicks(i, df)
    clicksTotal <- clicksTotal %>% rbind(clicks)
  }
 
  return(clicksTotal)
}



getProspectClicks <- function(emailIDs){
 # clicksTotal <- data.frame(id = '', prospect_id = '', url = '', list_email_id = '', email_template_id = '', created_at = '')[0,]
  clicksTotal <- data.frame(id = numeric(),
                            prospect_id = numeric(),
                            url = character(),
                            list_email_id = numeric(),
                            email_template_id = numeric(),
                            created_at = character(),  # Keep created_at as character for date handling
                            stringsAsFactors = FALSE)  # Prevent automatic conversion to factors
  
  for(i in 1:nrow(emailIDs)){
    clicks <- getBatchClicks(i, emailIDs)
    # print(paste("Iteration:", i))
    # print(paste("Rows in clicks:", nrow(clicks)))
    # print(paste("Rows in clicksTotal before bind:", nrow(clicksTotal)))

    # clicksTotal <- clicksTotal %>% rbind(clicks)
    clicksTotal <- dplyr::bind_rows(clicksTotal, clicks)
    #print(paste("Rows in clicksTotal after bind:", nrow(clicksTotal)))
    
  }
  
  return(clicksTotal)
}

### New Salesforce/MC section

getCampaignNewslettersSF <- function(){
  message("Getting newsletter clicks from Salesforce API")
  
  emailIDs <- data.frame(id = unique(campaignNewsletters$id))
  
  emailIDs <- emailIDs %>%
    filter(str_detect(id, "a6hQk0"))
  
  storyLinks <- unique(campaignNewsletters$story_url)
  
  clicksAll <- getBatchClicksSF(emailIDs)
  
  
  #' clean and filter links for clicks on story URLs only
  #linkClicks <- clicksAll %>% 
  #  mutate(url = sub('\\?(.*)', '', url)) %>% 
  #  filter(grepl(paste(storyLinks, collapse = '|'), url))

  
  
  #' clean clicks df
  clicksByProspect <- clicksAll %>%
    #select(clicksAll, c(emailId = list_email_id, Pardot_URL = prospect_id, url, created_at)) %>% 
    #' pardot call doesn't supply contact IDs or Account IDs so join using Pardot IDs embedded in Pardot URLs
    #mutate(Pardot_URL = paste0("http://pi.pardot.com/prospect/read?id=", as.character(Pardot_URL)),
    #       EngagementDate = as.Date(created_at, format="%Y-%m-%d")) %>%
    left_join(prospects, by = c("et4ae5__Email__c" = "Email")) %>%
    #mutate(emailId = as.character(emailId))%>%
    #left_join(select(allEmailStats, c(emailId = id, CampaignName = name)), by = 'emailId')%>% 
    #left_join(
    #  select(allEmailStats, c(emailId = id, CampaignName = name)), by = c("et4ae5__SendDefinition__c" = "emailId"))
    #  %>%
    mutate(Status = 'Email',
           EngagementType = 'Newsletter') %>% 
    mutate(Account = ifelse(grepl('unknown|not provided|contacts created', tolower(Account))|Account == '', 'Unknown', Account))%>% 
    #' left join all accounts using account name
    left_join(select(accountsUnique, c(AccountsName = Account, AccountDomain, Industry, AccountType, TotalGiving, 
                                       NumGifts, NumOpenOpps, AccountId2 = AccountId)), 
              by = c('Account' = 'AccountsName'))  %>% 
    #switched Domain to AccountDomain since Domain didn't exist
    select(CampaignName = Name.x, EngagementType, Id = Id.y, RecordType, Status, EngagementDate = Last_Email_Open_Date__c,
           Name = Name.y, Email = et4ae5__Email__c, Domain, Account, AccountType, Industry,
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, 
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
    mutate(CampaignName = sub("^(\\d{4}-\\d{2}-\\d{2})\\s+", "\\1: ", CampaignName)) %>%
    mutate(identifyingID = NA) %>%
    mutate(campaignID = NA)
  
  # 🔹 NEW SECTION: Query Constituent Groups + Join
  ConGroupTable <- sf_query(
    "SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c 
     FROM Constituent_Group__c",
    object_name = "Constituent_Group__c",
    api_type = "Bulk 2.0",
    verbose = TRUE
  )
  
  # Clean and normalize
  ConGroupTable <- ConGroupTable %>%
    filter(!is.na(Account__c)) %>%
    mutate(Parent_Group__c = if_else(is.na(Parent_Group__c), Group_Name__c, Parent_Group__c)) %>%
    rename(ConId = Id)
  
  # Join: creates multiple rows per account if multiple groups exist
  clicksByProspect <- clicksByProspect %>%
    left_join(ConGroupTable, by = c("AccountId" = "Account__c"))
  
  
  #' clean df and set audience groups
  campaignMembersEmail <- cleanCampaignDF(clicksByProspect)
  
  campaignNewsletters_unique <- campaignNewsletters %>%
    #maybe switch to change from name to story_url? 
    #select(name, campaignID) %>%
    select(name, campaignID) %>%
    distinct()  # Remove any duplicate rows based on 'name' and 'campaignID'
  
  campaignMembersEmail <- campaignMembersEmail %>%
    left_join(campaignNewsletters_unique, by = c("CampaignName" = "name")) %>%
    mutate(campaignID = coalesce(campaignID.x, campaignID.y)) %>%
    select(-campaignID.x, -campaignID.y)  # Remove the old campaignID columns after updating
  
  
  return(campaignMembersEmail)
  
}

getConstituentGroupsPardot <- function(campaignNewslettersPardot) {
  
  # 🔹 NEW SECTION: Query Constituent Groups + Join
  ConGroupTable <- sf_query(
    "SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c 
     FROM Constituent_Group__c",
    object_name = "Constituent_Group__c",
    api_type = "Bulk 2.0",
    verbose = TRUE
  )
  
  # Clean and normalize
  ConGroupTable <- ConGroupTable %>%
    filter(!is.na(Account__c)) %>%
    mutate(Parent_Group__c = if_else(is.na(Parent_Group__c), Group_Name__c, Parent_Group__c)) %>%
    rename(ConId = Id)
  
  # Join: creates multiple rows per account if multiple groups exist
  campaignNewslettersPardot <- campaignNewslettersPardot %>%
    left_join(ConGroupTable, by = c("AccountId" = "Account__c"))
  
  
  campaignNewslettersPardot <- campaignNewslettersPardot %>%
    distinct(Email, CampaignName, EngagementDate, Group_Name__c, .keep_all = TRUE)
  
  
  return(campaignNewslettersPardot)
  
}

getCampaignNewslettersSFOLD <- function(){
  message("Getting newsletter clicks from Salesforce API")
  
  emailIDs <- data.frame(id = unique(campaignNewsletters$id))
  
  emailIDs <- emailIDs %>%
    filter(str_detect(id, "a6hQk0"))
  
  storyLinks <- unique(campaignNewsletters$story_url)
  
  clicksAll <- getBatchClicksSF(emailIDs)

  
  #' clean and filter links for clicks on story URLs only
  linkClicks <- clicksAll %>% 
    mutate(url = sub('\\?(.*)', '', url)) %>% 
    filter(grepl(paste(storyLinks, collapse = '|'), url))
  
  
  #' clean clicks df
  clicksByProspect <- linkClicks %>%
    #select(clicksAll, c(emailId = list_email_id, Pardot_URL = prospect_id, url, created_at)) %>% 
    #' pardot call doesn't supply contact IDs or Account IDs so join using Pardot IDs embedded in Pardot URLs
    #mutate(Pardot_URL = paste0("http://pi.pardot.com/prospect/read?id=", as.character(Pardot_URL)),
    #       EngagementDate = as.Date(created_at, format="%Y-%m-%d")) %>%
    left_join(prospects, by = c("et4ae5__Email__c" = "Email")) %>%
    #mutate(emailId = as.character(emailId))%>%
    #left_join(select(allEmailStats, c(emailId = id, CampaignName = name)), by = 'emailId')%>% 
    #left_join(
    #  select(allEmailStats, c(emailId = id, CampaignName = name)), by = c("et4ae5__SendDefinition__c" = "emailId"))
    #  %>%
    mutate(Status = 'Email',
           EngagementType = 'Newsletter') %>% 
    mutate(Account = ifelse(grepl('unknown|not provided|contacts created', tolower(Account))|Account == '', 'Unknown', Account))%>% 
    #' left join all accounts using account name
    left_join(select(accountsUnique, c(AccountsName = Account, AccountDomain, Industry, AccountType, TotalGiving, 
                                       NumGifts, NumOpenOpps, AccountId2 = AccountId)), 
              by = c('Account' = 'AccountsName'))  %>% 
    #switched Domain to AccountDomain since Domain didn't exist
    select(CampaignName = Name.x, EngagementType, Id = Id.y, RecordType, Status, EngagementDate = Last_Email_Open_Date__c,
           Name = Name.y, Email = et4ae5__Email__c, Domain, Account, AccountType, Industry,
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, 
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
    mutate(CampaignName = sub("^(\\d{4}-\\d{2}-\\d{2})\\s+", "\\1: ", CampaignName)) %>%
    mutate(identifyingID = NA) %>%
    mutate(campaignID = NA)
  
  
  
  #' clean df and set audience groups
  campaignMembersEmail <- cleanCampaignDF(clicksByProspect)
  
  campaignNewsletters_unique <- campaignNewsletters %>%
    #maybe switch to change from name to story_url? 
    #select(name, campaignID) %>%
    select(name, campaignID) %>%
    distinct()  # Remove any duplicate rows based on 'name' and 'campaignID'
  
  campaignMembersEmail <- campaignMembersEmail %>%
    left_join(campaignNewsletters_unique, by = c("CampaignName" = "name")) %>%
    mutate(campaignID = coalesce(campaignID.x, campaignID.y)) %>%
    select(-campaignID.x, -campaignID.y)  # Remove the old campaignID columns after updating
  
  
  return(campaignMembersEmail)

}
  

getBatchClicksSF <- function(emailIDs) {
  # Collapse email IDs into a quoted, comma-separated string for SOQL
  ids <- paste0("'", emailIDs$id, "'", collapse = ",")
  
  # Build SOQL query
  my_soql <- sprintf("
    SELECT Id, Name, OwnerId, Campaign_Id__c, CreatedById, CreatedDate,
           et4ae5__BatchId__c, et4ae5__CampaignMemberId__c, et4ae5__Contact_Id__c,
           et4ae5__Contact__c, et4ae5__Clicked__c, et4ae5__DateSent__c,
           et4ae5__DateUnsubscribed__c, et4ae5__Opened__c, et4ae5__Email__c,
           et4ae5__JobId__c, et4ae5__Lead_ID__c, et4ae5__Email_ID__c, et4ae5__ListId__c,
           et4ae5__SendDefinition__c, et4ae5__SubjectLine__c, et4ae5__SubscriberId__c,
           et4ae5__TriggeredSendDefinitionName__c
    FROM et4ae5__IndividualEmailResult__c
    WHERE et4ae5__Clicked__c = true
      AND et4ae5__SendDefinition__c IN (%s)
  ", ids)
  
  # Call Salesforce Bulk API
  clicks <- sf_query(my_soql, 
                     object_name = "et4ae5__IndividualEmailResult__c", 
                     api_type = "Bulk 1.0")
  
  return(clicks)
}



#### SALESFORCE ####

getProspectsOLD <- function() {
  
  my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
                             pi__url__c, pi__score__c, pi__last_activity__c,
                             Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId,
                             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c,
                             Last_Email_Open_Date__c
                           
                    FROM Contact
                    WHERE pi__last_activity__c != null 
                    ORDER BY pi__last_activity__c DESC")
  
  allContacts <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Account_Name_Text__c, Email, 
           Pardot_URL = pi__url__c, Pardot_Score = pi__score__c, 
           Giving_Circle = Giving_Circle__c, Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c, pi__last_activity__c,
           Last_Email_Open_Date__c, AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Contact')
  
  
  my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c,
   Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c, Last_Email_Open_Date__c

                    FROM Lead
                    WHERE pi__last_activity__c != null 
                    ORDER BY pi__last_activity__c DESC")
  
  allLeads <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c, pi__last_activity__c,
           Last_Email_Open_Date__c, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Lead') %>% 
    #' remove instances of duplicate Pardot URLs (which contains a prosepct's Pardot ID) where one is both a Lead and Contact
    #' by using anti_join to get rid of all Leads if a Contact has the same Pardot URL
    anti_join(allContacts, by = 'Pardot_URL')
  
  #' bind contacts and leads
  prospects <- allContacts %>% 
    plyr::rbind.fill(allLeads) %>% 
    filter(!is.na(Email))
  
  #' identify and remove duplicates
  dup <- prospects[duplicated(prospects[,c("Pardot_URL")]) | duplicated(prospects[,c("Pardot_URL")], fromLast = TRUE), ] 
  
  #' from list of duplicated, filter for records where there is no account name
  dup1 <- dup %>% 
    filter(grepl('unknown|not provided|contacts created', tolower(Account))|Account == ''|is.na(Account)) %>% 
    distinct(Pardot_URL, .keep_all = TRUE)
  
  #' remove these prospects
  prospects_keep <- prospects %>% 
    anti_join(dup1) 
  
  #' finally, apply distinct() to remove all duplicate instances 
  unique_prospects <- prospects_keep %>% 
    distinct(Pardot_URL, .keep_all = TRUE)
  
  #' final df contains 188,615 unique prospects
  return(unique_prospects)
}



#' get all prospects (all contacts and leads with Pardot Activity)
getProspects <- function() {
  
  my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email, Last_Email_Open_Date__c,
                             pi__url__c, pi__score__c, pi__last_activity__c,
                             Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId,
                             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c
                           
                    FROM Contact
                    WHERE Last_Email_Open_Date__c != null 
                    ORDER BY Last_Email_Open_Date__c DESC")
  
  allContacts <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Account_Name_Text__c, Email, Last_Email_Open_Date__c,
           Pardot_URL = pi__url__c, Pardot_Score = pi__score__c, 
           Giving_Circle = Giving_Circle__c, Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
           AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Contact')
  
  
  my_soql <- sprintf("SELECT Id,  Name, Company, Email, Last_Email_Open_Date__c, pi__url__c, pi__score__c, pi__last_activity__c,
   Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c

                    FROM Lead
                    WHERE Last_Email_Open_Date__c != null 
                    ORDER BY Last_Email_Open_Date__c DESC")
  
  allLeads <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Company, Email, Last_Email_Open_Date__c, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Lead') %>% 
    #' remove instances of duplicate Pardot URLs (which contains a prosepct's Pardot ID) where one is both a Lead and Contact
    #' by using anti_join to get rid of all Leads if a Contact has the same Pardot URL
    anti_join(allContacts, by = 'Pardot_URL')
  
  #' bind contacts and leads
  prospects <- allContacts %>% 
    plyr::rbind.fill(allLeads) %>% 
    filter(!is.na(Email))
  
  #' identify and remove duplicates
  dup <- prospects[duplicated(prospects[,c("Pardot_URL")]) | duplicated(prospects[,c("Pardot_URL")], fromLast = TRUE), ] 
  
  #' from list of duplicated, filter for records where there is no account name
  dup1 <- dup %>% 
    filter(grepl('unknown|not provided|contacts created', tolower(Account))|Account == ''|is.na(Account)) %>% 
    distinct(Pardot_URL, .keep_all = TRUE)
  
  #' remove these prospects
  prospects_keep <- prospects %>% 
    anti_join(dup1) 
  
  #' finally, apply distinct() to remove all duplicate instances 
  unique_prospects <- prospects_keep %>% 
    distinct(Pardot_URL, .keep_all = TRUE)
  
  #' final df contains 188,615 unique prospects
  return(unique_prospects)
}


#' get all accounts from SF; ADD CONSTITUENT GROUP HERE?
getAllAccounts <- function(){
  
  my_soql <- sprintf("SELECT Id, Name, Type, Industry, Constituent_Groups__c, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
                      D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
                      D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
                      Top_Account__c, Total_Opportunity_Payments__c, of_Gifts__c, Number_of_Open_Opportunities__c,
                      Prospect_Program_Interests__c, Affinity__c, Program_Interest_Multi_Select_Text__c, Website, D_B_Web_Address__c, X18_Char_ID__c

                    FROM Account")
  
  all_accounts <- sf_query(my_soql, "Account", api_type = "Bulk 1.0") %>% 
    select(AccountId = Id, Account = Name, AccountType = Type, Industry, Constituent_Group = Constituent_Groups__c, AccountDomain = Email_Domain__c,
           Website, DB_Website = D_B_Web_Address__c, DB_IndustryCategory = D_B_Major_Industry_Category_Name__c, NAICS1 = D_B_NAICS_Description_1__c, 
           NAICS2 = D_B_NAICS_Description_2__c, SIC1 = D_B_SIC4_Code_1_Description__c, SIC2 = D_B_SIC4_Code_2_Description__c,
           TopAccount = Top_Account__c, TotalGiving = Total_Opportunity_Payments__c, NumGifts = of_Gifts__c, NumOpenOpps = Number_of_Open_Opportunities__c)
  
  all_accounts <- all_accounts %>%
    separate(Constituent_Group, into = c("Constituent_Group_1", "Constituent_Group_2"), sep = ";", fill = "right", extra = "merge")
  
  return(all_accounts)
}


# testing_all_accounts <- getAllAccounts()
# 
# print(head(testing_all_accounts))
# 


# getAllAccounts <- function(){
#   
#   my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
#                       D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
#                       D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
#                       Top_Account__c, Total_Opportunity_Payments__c, of_Gifts__c, Number_of_Open_Opportunities__c,
#                       Prospect_Program_Interests__c, Affinity__c, Program_Interest_Multi_Select_Text__c, Website, D_B_Web_Address__c, X18_Char_ID__c
# 
#                     FROM Account")
#   
#   all_accounts <- sf_query(my_soql, "Account", api_type = "Bulk 1.0") %>% 
#     select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
#            Website, DB_Website = D_B_Web_Address__c, DB_IndustryCategory = D_B_Major_Industry_Category_Name__c, NAICS1 = D_B_NAICS_Description_1__c, 
#            NAICS2 = D_B_NAICS_Description_2__c, SIC1 = D_B_SIC4_Code_1_Description__c, SIC2 = D_B_SIC4_Code_2_Description__c,
#            TopAccount = Top_Account__c, TotalGiving = Total_Opportunity_Payments__c, NumGifts = of_Gifts__c, NumOpenOpps = Number_of_Open_Opportunities__c)
#   
#   return(all_accounts)
# }

#' functions to get reports and events

#' get list of all campaigns created after 2022-01-01
# getCampaignList <- function(nameOfInterest){
#   
#   if (length(nameOfInterest) == 0) {
#     stop("nameOfInterest must contain at least one valid campaign name.")
#   }
#   
#   nameList <- paste0("'", nameOfInterest, "'", collapse = ", ")
#   
#   my_soql <- sprintf("SELECT Id, 
#                            Name,
#                            Type,
#                            Category__c,
#                            CreatedDate,
#                            Description,
#                            Contact_Report__c,
#                            Members_in_Campaign__c
#                     FROM Campaign
#                     WHERE Name in ('%s')", nameList)
#   print(my_soql)
#   
#   
#   campaignList <- sf_query(my_soql) %>% 
#     select(Id, Name, Type, CreatedDate, Members = Members_in_Campaign__c)
#   
#   return(campaignList)
# }

getCampaignList <- function(){
  my_soql <- sprintf("SELECT Id,
                           Name,
                           Type,
                           Category__c,
                           CreatedDate,
                           Description,
                           Contact_Report__c,
                           Members_in_Campaign__c
                    FROM Campaign
                    WHERE CreatedDate > 2022-01-01T01:02:03Z")

  campaignList <- sf_query(my_soql) %>%
    select(Id, Name, Type, CreatedDate, Members = Members_in_Campaign__c)

  return(campaignList)
}

#new, good, for events
getCampaignMembersEvents <- function(campaignIdList, campaignType) {
  
  #' get campaign members
  my_soql <- sprintf("SELECT CampaignId,
                             Name,
                             Status,
                             HasResponded,
                             ContactId,
                             LeadId,
                             CreatedDate

                      FROM CampaignMember
                      WHERE CampaignId in ('%s')",
                     paste0(campaignIdList, collapse = "','"))
  
  campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0") %>% 
    left_join(select(campaignList, c(CampaignId = Id, CampaignName = Name)), by = 'CampaignId') %>% 
    filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  
  if (nrow(campaign_members) == 0) {
    message("No campaign members found for the provided campaign IDs.")
    return(data.frame())
  }
  
  campaignContacts <- campaign_members %>% 
    filter(!is.na(ContactId)) %>% 
    distinct(ContactId, .keep_all = TRUE) 
  
  campaignLeads <- campaign_members %>% 
    filter(!is.na(LeadId) & is.na(ContactId)) %>% 
    distinct(LeadId, .keep_all = TRUE) 
  
  #' get info for contacts in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Account.Constituent_Groups__c
  if (nrow(campaignContacts) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
                               pi__url__c, pi__score__c, pi__last_activity__c,
                               Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
                               Account_Constituent_Sub_Groups__c
                             
                        FROM Contact
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignContacts$ContactId, collapse = "','"))
    
    campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Account_Name_Text__c, Email,
             Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,  Giving_Circle = Giving_Circle__c,
             Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
             AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Contact') %>% 
      filter(!is.na(AccountId))
  } else {
    campaignContactsQuery <- data.frame()
  }
  
  #' get info for leads in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Likely_Account__r.Constituent_Groups__c
  if (nrow(campaignLeads) > 0) {
    my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c, 
    Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c

                        FROM Lead
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignLeads$LeadId, collapse = "','"))
    
    campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Lead') 
  } else {
    campaignLeadsQuery <- data.frame()
  } 
  
  # print(head(campaignLeadsQuery))
  
  #' get info for accounts that match the AccountID field pulled from contacts query ADDING CONSTITUENTS HERE?
  if (!is.null(campaignContactsQuery$AccountId) && length(campaignContactsQuery$AccountId) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
                        D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
                        D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
                        Top_Account__c, Total_Opportunity_Payments__c, Number_of_Open_Opportunities__c,
                        Constituent_Groups__c, Constituent_Sub_Groups__c
                        FROM Account 
                        WHERE Id in ('%s')", 
                       paste0(campaignContactsQuery$AccountId, collapse = "','"))
    
    campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>% 
      select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
             TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c, Constituent_Groups__c,
             Constituent_Sub_Groups__c)
  } else {
    campaignContactAccounts <- data.frame()
  }
  
  #' join
  if (nrow(campaignContactsQuery) > 0) {
    campaignContactsQuery <- campaignContactsQuery %>% 
      left_join(select(campaignContactAccounts, -Account), by = c('AccountId'))
  }
  
  if (nrow(campaignLeadsQuery) > 0) {
    campaignLeadsQuery <- campaignLeadsQuery %>% 
      left_join(campaignContactAccounts, by = c('Account'))
  }
  
  contactsLeads <- plyr::rbind.fill(campaignContactsQuery, campaignLeadsQuery) %>% 
    mutate(Giving_Circle = as.character(Giving_Circle))
  
  
  #' join to campaign members df
  CampaignMembers <- campaign_members %>% 
    mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>% 
    select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>% 
    left_join(contactsLeads, by = c('Id', 'Name'))
  
  #' specify campaign type
  if(campaignType == 'Event'){
    
    #' CampaignMembers <- CampaignMembers %>% 
    #'   mutate(EngagementType = 'Event',
    #'          #' rename status categories and filter for rows where status is "registered" or "attended"
    #'          #' Updated on 1/17/24 to included registrations as attended
    #'          #' "Registered - In Person", "WAITLIST" , "Registered - Virtual", and "Clicked to Register"
    #'          # Status = ifelse(grepl('Register', Status), 'Registered (Did Not Attend)', Status),
    #'          Status = case_when(grepl('Register', Status) ~ 'Attended',
    #'                             grepl('WAITLIST', Status) ~ 'Attended',
    #'                             TRUE ~ Status)) %>%
    #'   new
    CampaignMembers <- CampaignMembers %>% 
      mutate(
        EngagementType = 'Event',
        Status = case_when(
          grepl("Attended|Speaker|\\+1 Speaker Guest", Status) ~ 'Attended',
          
          grepl("Attended|Speaker|\\+1 Speaker Guest|Registered|Waitlist|Attending|WAITLIST|De-registered", Status) ~ 'Registered',
          
          # Keep the original Status for other values
          TRUE ~ Status
        )
      ) %>%
    #used to have Attending
      filter(grepl('Registered|Attended', Status))
      #campaignIdList = c(test_campaign_id)
    
  } else if (campaignType == 'Report'){
    
    CampaignMembers <- CampaignMembers %>% 
      #changed from report download
      mutate(EngagementType = 'Report Download') 
    
  }
  
  CampaignMembers <- CampaignMembers %>% 
    #' get domain from email
    mutate(Domain = sub("(.*)\\@", "", Email),
           EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>% 
    #' select and reorder columns
    select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry, 
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, Account_Constituent_Groups__c,
           Account_Constituent_Sub_Groups__c)
  
  return(CampaignMembers)
  
}



# testing for eventid that isnt working, 10/23
#new, good, for events

testingGetCampaignMembersEvents <- function(campaignIdList, campaignType) {
  
  #' get campaign members
  my_soql <- sprintf("SELECT CampaignId,
                             Name,
                             Status,
                             HasResponded,
                             ContactId,
                             LeadId,
                             CreatedDate

                      FROM CampaignMember
                      WHERE CampaignId in ('%s')",
                     paste0(campaignIdList, collapse = "','"))
  
  campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0") %>% 
    left_join(select(campaignList, c(CampaignId = Id, CampaignName = Name)), by = 'CampaignId') %>% 
    filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  
  print(str(campaign_members$CampaignId))  # Check data type of CampaignId
  print(str(campaignList$Id))  # Check data type of Id
  
  print("Campaign Members:")
  print(head(campaign_members))
  
  if (nrow(campaign_members) == 0) {
    message("No campaign members found for the provided campaign IDs.")
    return(data.frame())
  }
  
  campaignContacts <- campaign_members %>% 
    filter(!is.na(ContactId)) %>% 
    distinct(ContactId, .keep_all = TRUE) 
  
  
  message("Preview of campaignContacts:")
  print(head(campaignContacts)) 
  
  # used to be &
  campaignLeads <- campaign_members %>% 
    filter(!is.na(LeadId) & is.na(ContactId)) %>% 
    distinct(LeadId, .keep_all = TRUE) 
  
  message("Preview of campaignLeads:")
  print(head(campaignLeads))   
  
  #' get info for contacts in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Account.Constituent_Groups__c
  if (nrow(campaignContacts) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
                               pi__url__c, pi__score__c, pi__last_activity__c,
                               Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
                               Account_Constituent_Sub_Groups__c
                             
                        FROM Contact
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignContacts$ContactId, collapse = "','"))
    
    campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Account_Name_Text__c, Email,
             Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,  Giving_Circle = Giving_Circle__c,
             Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
             AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Contact') %>% 
      filter(!is.na(AccountId))
  } else {
    campaignContactsQuery <- data.frame()
  }
  
  message("Preview of campaignContactsQuery:")
  print(head(campaignContactsQuery))   
  
  #' get info for leads in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Likely_Account__r.Constituent_Groups__c
  if (nrow(campaignLeads) > 0) {
    my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c, 
    Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c

                        FROM Lead
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignLeads$LeadId, collapse = "','"))
    
    campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Lead') 
  } else {
    campaignLeadsQuery <- data.frame()
  } 
  
  
  message("Preview of campaignLeadsQuery:")
  print(head(campaignLeadsQuery))   
  
  # print(head(campaignLeadsQuery))
  
  #' get info for accounts that match the AccountID field pulled from contacts query ADDING CONSTITUENTS HERE?
  if (!is.null(campaignContactsQuery$AccountId) && length(campaignContactsQuery$AccountId) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
                        D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
                        D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
                        Top_Account__c, Total_Opportunity_Payments__c, Number_of_Open_Opportunities__c,
                        Constituent_Groups__c, Constituent_Sub_Groups__c
                        FROM Account 
                        WHERE Id in ('%s')", 
                       paste0(campaignContactsQuery$AccountId, collapse = "','"))
    
    campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>% 
      select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
             TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c, Constituent_Groups__c,
             Constituent_Sub_Groups__c)
  } else {
    campaignContactAccounts <- data.frame()
  }
  
  message("Preview of campaignContactAccounts:")
  print(head(campaignContactAccounts))   
  
  
  #' join
  if (nrow(campaignContactsQuery) > 0) {
    campaignContactsQuery <- campaignContactsQuery %>% 
      left_join(select(campaignContactAccounts, -Account), by = c('AccountId'))
  }
  
  if (nrow(campaignLeadsQuery) > 0) {
    campaignLeadsQuery <- campaignLeadsQuery %>% 
      left_join(campaignContactAccounts, by = c('Account'))
  }
  
  contactsLeads <- plyr::rbind.fill(campaignContactsQuery, campaignLeadsQuery) %>% 
    mutate(Giving_Circle = as.character(Giving_Circle))
  
  
  
  message("Preview of contactsLeads:")
  print(head(contactsLeads))   
  
  #' join to campaign members df
  CampaignMembers <- campaign_members %>% 
    mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>% 
    select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>% 
    left_join(contactsLeads, by = c('Id', 'Name'))
  
  
  message("Preview of CampaignMembers:")
  print(head(CampaignMembers)) 
  
  #message("Campaign Type: ", campaignType)
  
  #' specify campaign type
  if(campaignType == 'Event'){
    
    message("Unique Status values before transformation:")
    print(unique(CampaignMembers$Status))
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(
        EngagementType = 'Event',
        Status = case_when(
          
          #removing attended category for now BECAUSE anyone who's attended should be counted as registered.
          #grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest", Status) ~ 'Attended',
          
          #grepl("Attended|Speaker|\\+1 Speaker Guest|Registered|Waitlist|Attending|WAITLIST|De-registered|Clicked to Register", Status) ~ 'Registered',
          
          #          grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest|Registered - Full Day|De-registered|Registered - High Priority|Registered - Staff|Waitlist|Registered but did not attend|Registered - Hybrid|Registered - In Person|Registered - Virtual|Attending|WAITLIST", Status) ~ 'Registered',
          
          grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest|Registered( - Full Day| - High Priority| - Staff| but did not attend| - Hybrid| - In Person| - Virtual)?|De-registered|Waitlist|Attending|WAITLIST", Status) ~ 'Registered',
          
          #grepl("Registered (Did Not Attend)", Status) ~ 'Registered (Did Not Attend)',
          
          TRUE ~ 'Other'
        )
      ) %>%
      # this changed
      # { 
      #   message("Unique Status values before filtering: ", 
      #           paste(unique(.$Status), collapse = ", "))
      #   . 
      # } %>%
      # 
      #used to be register?
      filter(grepl('Registered|Attended|Other', Status))
    # 
  } else if (campaignType == 'Report'){
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(EngagementType = 'Report Download') 
    
  }
  
  message("Preview of CampaignMembers:")
  print(head(CampaignMembers)) 
  
  CampaignMembers <- CampaignMembers %>% 
    #' get domain from email
    mutate(Domain = sub("(.*)\\@", "", Email),
           EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>% 
    #' select and reorder columns
    select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry, 
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, Account_Constituent_Groups__c,
           Account_Constituent_Sub_Groups__c)
  
  message("Preview of CampaignMembers:")
  print(CampaignMembers)
  return(CampaignMembers)
  
  
}

testingGetCampaignMembersEventsOLD <- function(campaignIdList, campaignType) {
  
  #' get campaign members
  my_soql <- sprintf("SELECT CampaignId,
                             Name,
                             Status,
                             HasResponded,
                             ContactId,
                             LeadId,
                             CreatedDate

                      FROM CampaignMember
                      WHERE CampaignId in ('%s')",
                     paste0(campaignIdList, collapse = "','"))
  
  campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0") %>% 
    left_join(select(campaignList, c(CampaignId = Id, CampaignName = Name)), by = 'CampaignId') %>% 
    filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  
  print(str(campaign_members$CampaignId))  # Check data type of CampaignId
  print(str(campaignList$Id))  # Check data type of Id
  
  print("Campaign Members:")
  print(head(campaign_members))
  
  if (nrow(campaign_members) == 0) {
    message("No campaign members found for the provided campaign IDs.")
    return(data.frame())
  }
  
  campaignContacts <- campaign_members %>% 
    filter(!is.na(ContactId)) %>% 
    distinct(ContactId, .keep_all = TRUE) 
  
  
  message("Preview of campaignContacts:")
  print(head(campaignContacts)) 
  
  # used to be &
  campaignLeads <- campaign_members %>% 
    filter(!is.na(LeadId) & is.na(ContactId)) %>% 
    distinct(LeadId, .keep_all = TRUE) 
  
  message("Preview of campaignLeads:")
  print(head(campaignLeads))   
  
  #' get info for contacts in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Account.Constituent_Groups__c
  if (nrow(campaignContacts) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
                               pi__url__c, pi__score__c, pi__last_activity__c,
                               Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
                               Account_Constituent_Sub_Groups__c
                             
                        FROM Contact
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignContacts$ContactId, collapse = "','"))
    
    campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Account_Name_Text__c, Email,
             Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,  Giving_Circle = Giving_Circle__c,
             Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
             AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Contact') %>% 
      filter(!is.na(AccountId))
  } else {
    campaignContactsQuery <- data.frame()
  }
  
  message("Preview of campaignContactsQuery:")
  print(head(campaignContactsQuery))   
  
  #' get info for leads in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Likely_Account__r.Constituent_Groups__c
  if (nrow(campaignLeads) > 0) {
    my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c, 
    Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c

                        FROM Lead
                        WHERE Id in ('%s')
                        ORDER BY pi__last_activity__c DESC NULLS LAST",
                       paste0(campaignLeads$LeadId, collapse = "','"))
    
    campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
      select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
      mutate(RecordType = 'Lead') 
  } else {
    campaignLeadsQuery <- data.frame()
  } 
  
  
  message("Preview of campaignLeadsQuery:")
  print(head(campaignLeadsQuery))   
  
  # print(head(campaignLeadsQuery))
  
  #' get info for accounts that match the AccountID field pulled from contacts query ADDING CONSTITUENTS HERE?
  if (!is.null(campaignContactsQuery$AccountId) && length(campaignContactsQuery$AccountId) > 0) {
    my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
                        D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
                        D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
                        Top_Account__c, Total_Opportunity_Payments__c, Number_of_Open_Opportunities__c,
                        Constituent_Groups__c, Constituent_Sub_Groups__c
                        FROM Account 
                        WHERE Id in ('%s')", 
                       paste0(campaignContactsQuery$AccountId, collapse = "','"))
    
    campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>% 
      select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
             TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c, Constituent_Groups__c,
             Constituent_Sub_Groups__c)
  } else {
    campaignContactAccounts <- data.frame()
  }
  
  message("Preview of campaignContactAccounts:")
  print(head(campaignContactAccounts))   
  
  
  #' join
  if (nrow(campaignContactsQuery) > 0) {
    campaignContactsQuery <- campaignContactsQuery %>% 
      left_join(select(campaignContactAccounts, -Account), by = c('AccountId'))
  }
  
  if (nrow(campaignLeadsQuery) > 0) {
    campaignLeadsQuery <- campaignLeadsQuery %>% 
      left_join(campaignContactAccounts, by = c('Account'))
  }
  
  contactsLeads <- plyr::rbind.fill(campaignContactsQuery, campaignLeadsQuery) %>% 
    mutate(Giving_Circle = as.character(Giving_Circle))
  
  
  
  message("Preview of contactsLeads:")
  print(head(contactsLeads))   
  
  #' join to campaign members df
  CampaignMembers <- campaign_members %>% 
    mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>% 
    select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>% 
    left_join(contactsLeads, by = c('Id', 'Name'))
  
  
  message("Preview of CampaignMembers:")
  print(head(CampaignMembers)) 
  
  #message("Campaign Type: ", campaignType)
  
  #' specify campaign type
  if(campaignType == 'Event'){
    
    message("Unique Status values before transformation:")
    print(unique(CampaignMembers$Status))
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(
        EngagementType = 'Event',
        Status = case_when(
          
          #removing attended category for now BECAUSE anyone who's attended should be counted as registered.
          #grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest", Status) ~ 'Attended',
          
          #grepl("Attended|Speaker|\\+1 Speaker Guest|Registered|Waitlist|Attending|WAITLIST|De-registered|Clicked to Register", Status) ~ 'Registered',
          
          #          grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest|Registered - Full Day|De-registered|Registered - High Priority|Registered - Staff|Waitlist|Registered but did not attend|Registered - Hybrid|Registered - In Person|Registered - Virtual|Attending|WAITLIST", Status) ~ 'Registered',
          
          grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest|Registered( - Full Day| - High Priority| - Staff| but did not attend| - Hybrid| - In Person| - Virtual)?|De-registered|Waitlist|Attending|WAITLIST", Status, ignore.case = TRUE) ~ 'Registered',
          
          #grepl("Registered (Did Not Attend)", Status) ~ 'Registered (Did Not Attend)',
          
          TRUE ~ 'Other'
        )
      ) %>%
      # this changed
      # { 
      #   message("Unique Status values before filtering: ", 
      #           paste(unique(.$Status), collapse = ", "))
      #   . 
      # } %>%
      # 
      #used to be register?
      filter(grepl('Registered|Attended|Other', Status))
    # 
  } else if (campaignType == 'Report'){
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(EngagementType = 'Report Download') 
    
  }
  
  message("Preview of CampaignMembers:")
  print(head(CampaignMembers)) 
  
  CampaignMembers <- CampaignMembers %>% 
    #' get domain from email
    mutate(Domain = sub("(.*)\\@", "", Email),
           EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>% 
    #' select and reorder columns
    select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry, 
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, Account_Constituent_Groups__c,
           Account_Constituent_Sub_Groups__c)
  
  message("Preview of CampaignMembers:")
  print(CampaignMembers)
  return(CampaignMembers)
  
  
}

# 
#current_eventID <- "701Qk00000Ka1dJIAR"
#campaignMembers <- testingGetCampaignMembersEvents(current_eventID, 'Event')

#campaignmembersfiltered <- campaignMembers %>%
 # filter(Status == "Registered")
# 
#current_eventID2 <- "701Qk00000Jj5QPIAZ"
#campaignMembers2 <- testingGetCampaignMembersEvents(current_eventID2, 'Event')
# 
# 
# test_campaign_id <- "701Qk00000HVMUGIA5"

#result <- testingGetCampaignMembersEvents(campaignIdList = c(current_eventID), campaignType = "Event")



#og
#' for a list of campaign IDs, get campaign members and join to contact/lead/account info
getCampaignMembersOLD <- function(campaignIdList, campaignType) {
  
  
  message("Starting getCampaignMembers")
  message("Campaign IDs: ", paste(campaignIdList, collapse = ", "))
  message("Campaign Type: ", campaignType)
  
  #' get campaign members
  my_soql <- sprintf("SELECT CampaignId,
                             Name,
                             Status,
                             HasResponded,
                             ContactId,
                             LeadId,
                             CreatedDate

                      FROM CampaignMember
                      WHERE CampaignId in ('%s')",
                     paste0(campaignIdList, collapse = "','"))
  
  campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0") %>% 
    left_join(select(campaignList, c(CampaignId = Id, CampaignName = Name)), by = 'CampaignId') %>% 
    filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  
  
  message("Preview of campaign_members:")
  print(head(campaign_members))

  campaignContacts <- campaign_members %>% 
    filter(!is.na(ContactId)) %>% 
    distinct(ContactId, .keep_all = TRUE) 
  
  message("Preview of campaignContacts:")
  print(head(campaignContacts))
  
  campaignLeads <- campaign_members %>% 
    filter(!is.na(LeadId) & is.na(ContactId)) %>% 
    distinct(LeadId, .keep_all = TRUE) 
  
  
  message("Preview of campaignLeads:")
  print(head(campaignLeads))
  
  #' get info for contacts in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Account.Constituent_Groups__c
  my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
                             pi__url__c, pi__score__c, pi__last_activity__c,
                             Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
                             Account_Constituent_Sub_Groups__c
                           
                    FROM Contact
                    WHERE Id in ('%s')
                    ORDER BY pi__last_activity__c DESC NULLS LAST",
                    paste0(campaignContacts$ContactId, collapse = "','"))
  
  campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Account_Name_Text__c, Email,
           Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,  Giving_Circle = Giving_Circle__c,
           Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
           AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Contact') %>% 
    filter(!is.na(AccountId))
  
  
  message("Preview of campaignContactsQuery:")
  print(head(campaignContactsQuery))

  #' get info for leads in campaign SHOULD WE BE ADDING CONSITUENTS HERE? Likely_Account__r.Constituent_Groups__c
  my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c, 
  Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c


                      FROM Lead
                      WHERE Id in ('%s')
                      ORDER BY pi__last_activity__c DESC NULLS LAST",
                     paste0(campaignLeads$LeadId, collapse = "','"))
  
  campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
    select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>% 
    mutate(RecordType = 'Lead') 

 # print(head(campaignLeadsQuery))
  
  message("Preview of campaignLeadsQuery:")
  print(head(campaignLeadsQuery))
  
  
  #' get info for accounts that match the AccountID field pulled from contacts query ADDING CONSTITUENTS HERE?
  my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
                      D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
                      D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
                      Top_Account__c, Total_Opportunity_Payments__c, of_Gifts__c, Number_of_Open_Opportunities__c,
                      Prospect_Program_Interests__c, Affinity__c, Program_Interest_Multi_Select_Text__c, Constituent_Groups__c,
                      Constituent_Sub_Groups__c

                      FROM Account 
                      WHERE Id in ('%s')", 
                     paste0(campaignContactsQuery$AccountId, collapse = "','"))
  
  campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>% 
    select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
           TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c, Constituent_Groups__c,
           Constituent_Sub_Groups__c)


  #' join
  campaignContactsQuery <- campaignContactsQuery %>% 
  #  left_join(select(campaignContactAccounts, AccountId, Constituent_Groups__c, everything()), by = c('AccountId')) 
    left_join(select(campaignContactAccounts, -Account), by = c('AccountId')) 
#    mutate(ConstituentGroups = Account.Constituent_Groups__c)  # Added constituent groups for contacts
  
  campaignLeadsQuery <- campaignLeadsQuery %>% 
    left_join(campaignContactAccounts, by = c('Account'))
 #   mutate(ConstituentGroups = Likely_Account_Constituent_Groups)  # Added constituent groups for leads
  
  contactsLeads <- campaignContactsQuery %>%
    plyr::rbind.fill(campaignLeadsQuery) 
  
  contactsLeads <- contactsLeads %>% mutate(Giving_Circle = as.character(Giving_Circle))
  
  #' join to campaign members df
  CampaignMembers <- campaign_members %>% 
    mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>% 
    select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>% 
    left_join(contactsLeads, by = c('Id', 'Name'))
  
  #' specify campaign type
  if(campaignType == 'Event'){
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(EngagementType = 'Event',
             #' rename status categories and filter for rows where status is "registered" or "attended"
             #' Updated on 1/17/24 to included registrations as attended
             #' "Registered - In Person", "WAITLIST" , "Registered - Virtual", and "Clicked to Register"
            # Status = ifelse(grepl('Register', Status), 'Registered (Did Not Attend)', Status),
             Status = case_when(grepl('Register', Status) ~ 'Attended',
                                grepl('WAITLIST', Status) ~ 'Attended',
                                TRUE ~ Status)) %>%
      
      filter(grepl('Register|Attended', Status))
    
  } else if (campaignType == 'Report'){
    
    CampaignMembers <- CampaignMembers %>% 
      mutate(EngagementType = 'Report Download') 
    
  }
  
  CampaignMembers <- CampaignMembers %>% 
    #' get domain from email
    mutate(Domain = sub("(.*)\\@", "", Email),
          EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>% 
    #' select and reorder columns
    select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry, 
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, Account_Constituent_Groups__c,
           Account_Constituent_Sub_Groups__c)
  
  message("Preview of CampaignMembers after transformation:")
  print(head(CampaignMembers))
  
  return(CampaignMembers)
  
}



getCampaignMembers <- function(campaignIdList, campaignType) {
  message("Starting getCampaignMembers")
  message("Campaign IDs: ", paste(campaignIdList, collapse = ", "))
  message("Campaign Type: ", campaignType)
  
  #' build SOQL query for CampaignMember
  my_soql <- sprintf(
    "SELECT CampaignId,
            Name,
            Status,
            HasResponded,
            ContactId,
            LeadId,
            CreatedDate
     FROM CampaignMember
     WHERE CampaignId in ('%s')",
    paste0(campaignIdList, collapse = "','")
  )
  
  #' safely query and join
  campaign_members <- tryCatch({
    members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0")
    if (nrow(members) == 0) {
      message("No campaign members found for given Campaign IDs.")
      return(tibble::tibble())
    }
    
    if ("Id" %in% names(campaignList) && "Name" %in% names(campaignList)) {
      members <- members %>%
        left_join(select(campaignList, CampaignId = Id, CampaignName = Name), by = "CampaignId")
    }
    
    members %>% filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  }, error = function(e) {
    message("Error fetching campaign members: ", e$message)
    return(tibble::tibble())
  })
  
  if (nrow(campaign_members) == 0) return(tibble::tibble())
  
  message("Preview of campaign_members:")
  print(head(campaign_members))
  
  #' split Contacts and Leads
  campaignContacts <- campaign_members %>%
    filter(!is.na(ContactId)) %>%
    distinct(ContactId, .keep_all = TRUE)
  
  campaignLeads <- campaign_members %>%
    filter(!is.na(LeadId) & is.na(ContactId)) %>%
    distinct(LeadId, .keep_all = TRUE)
  
  #' get Contact info
  campaignContactsQuery <- tibble::tibble()
  if (nrow(campaignContacts) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Account_Name_Text__c, Email,
              pi__url__c, pi__score__c, pi__last_activity__c,
              Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
              Account_Constituent_Sub_Groups__c
       FROM Contact
       WHERE Id in ('%s')
       ORDER BY pi__last_activity__c DESC NULLS LAST",
      paste0(campaignContacts$ContactId, collapse = "','")
    )
    
    campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>%
      select(Id, Name, Account = Account_Name_Text__c, Email,
             Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Giving_Circle = Giving_Circle__c,
             Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
             AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
      mutate(RecordType = 'Contact') %>%
      filter(!is.na(AccountId))
  }
  
  message("Preview of campaignContactsQuery:")
  print(head(campaignContactsQuery))
  
  #' get Lead info
  campaignLeadsQuery <- tibble::tibble()
  if (nrow(campaignLeads) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c,
              Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c
       FROM Lead
       WHERE Id in ('%s')
       ORDER BY pi__last_activity__c DESC NULLS LAST",
      paste0(campaignLeads$LeadId, collapse = "','")
    )
    
    campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>%
      select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
      mutate(RecordType = 'Lead')
  }
  
  message("Preview of campaignLeadsQuery:")
  print(head(campaignLeadsQuery))
  
  #' get Account info for contacts
  campaignContactAccounts <- tibble::tibble()
  if (nrow(campaignContactsQuery) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Type, Industry, Email_Domain__c, Total_Opportunity_Payments__c, Number_of_Open_Opportunities__c,
              Constituent_Groups__c, Constituent_Sub_Groups__c
       FROM Account
       WHERE Id in ('%s')",
      paste0(campaignContactsQuery$AccountId, collapse = "','")
    )
    
    campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>%
      select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
             TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c,
             Constituent_Groups__c, Constituent_Sub_Groups__c)
  }
  
  #' join accounts to contacts and leads
  if (nrow(campaignContactsQuery) > 0 && "AccountId" %in% names(campaignContactsQuery)) {
    campaignContactsQuery <- campaignContactsQuery %>%
      left_join(select(campaignContactAccounts, -Account), by = "AccountId")
  }
  
  if (nrow(campaignLeadsQuery) > 0 && "Account" %in% names(campaignLeadsQuery) && nrow(campaignContactAccounts) > 0) {
    campaignLeadsQuery <- campaignLeadsQuery %>%
      left_join(campaignContactAccounts, by = "Account")
  }
  
  contactsLeads <- plyr::rbind.fill(campaignContactsQuery, campaignLeadsQuery) %>%
    mutate(Giving_Circle = as.character(Giving_Circle))
  
  #' join back to campaign member base
  if (nrow(campaign_members) > 0) {
    CampaignMembers <- campaign_members %>%
      mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>%
      select(CampaignId, CampaignName, Name, Id, Status, CreatedDate)
    
    if (nrow(contactsLeads) > 0) {
      CampaignMembers <- CampaignMembers %>%
        left_join(contactsLeads, by = c("Id", "Name"))
    }
    
    #' type-based enrichment
    if (campaignType == 'Event') {
      CampaignMembers <- CampaignMembers %>%
        mutate(EngagementType = 'Event',
               Status = case_when(
                 grepl('Register', Status) ~ 'Attended',
                 grepl('WAITLIST', Status) ~ 'Attended',
                 TRUE ~ Status
               )) %>%
        filter(grepl('Register|Attended', Status))
    } else if (campaignType == 'Report') {
      CampaignMembers <- CampaignMembers %>%
        mutate(EngagementType = 'Report Download')
    }
    
    #' final formatting
    CampaignMembers <- CampaignMembers %>%
      mutate(Domain = ifelse("Email" %in% names(CampaignMembers), sub("(.*)\\@", "", Email), NA),
             EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>%
      select(any_of(c(
        "CampaignName", "EngagementType", "Id", "RecordType", "Status", "EngagementDate",
        "Name", "Email", "Domain", "Account", "AccountType", "Industry",
        "TotalGiving", "NumOpenOpps", "Pardot_Score", "Pardot_URL", "Giving_Circle",
        "Last_Gift", "AccountId", "Account_Constituent_Groups__c", "Account_Constituent_Sub_Groups__c"
      )))
  } else {
    CampaignMembers <- tibble::tibble()
  }
  
  message("Preview of CampaignMembers after transformation:")
  print(head(CampaignMembers))
  
  return(CampaignMembers)
}

getCampaignMembersTesting2 <- function(campaignIdList, campaignType) {
  message("Starting getCampaignMembersHybrid")
  message("Campaign IDs: ", paste(campaignIdList, collapse = ", "))
  message("Campaign Type: ", campaignType)
  
  #' Build SOQL query for CampaignMember
  my_soql <- sprintf(
    "SELECT CampaignId,
            Name,
            Status,
            HasResponded,
            ContactId,
            LeadId,
            CreatedDate
     FROM CampaignMember
     WHERE CampaignId in ('%s')",
    paste0(campaignIdList, collapse = "','")
  )
  
  #' Safely query and join campaignList
  campaign_members <- tryCatch({
    members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0")
    
    if (nrow(members) == 0) {
      message("No campaign members found for given Campaign IDs.")
      return(tibble::tibble())
    }
    
    if ("Id" %in% names(campaignList) && "Name" %in% names(campaignList)) {
      members <- members %>%
        left_join(select(campaignList, CampaignId = Id, CampaignName = Name), by = "CampaignId")
    }
    
    members %>% filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
  }, error = function(e) {
    message("Error fetching campaign members: ", e$message)
    return(tibble::tibble())
  })
  
  if (nrow(campaign_members) == 0) return(tibble::tibble())
  
  message("Preview of campaign_members:")
  print(head(campaign_members))
  
  #' Split Contacts and Leads
  campaignContacts <- campaign_members %>%
    filter(!is.na(ContactId)) %>%
    distinct(ContactId, .keep_all = TRUE)
  
  campaignLeads <- campaign_members %>%
    filter(!is.na(LeadId) & is.na(ContactId)) %>%
    distinct(LeadId, .keep_all = TRUE)
  
  #' Get Contact info
  campaignContactsQuery <- tibble::tibble()
  if (nrow(campaignContacts) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Account_Name_Text__c, Email,
              pi__url__c, pi__score__c, pi__last_activity__c,
              Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId, Account_Constituent_Groups__c,
              Account_Constituent_Sub_Groups__c
       FROM Contact
       WHERE Id in ('%s')
       ORDER BY pi__last_activity__c DESC NULLS LAST",
      paste0(campaignContacts$ContactId, collapse = "','")
    )
    
    campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>%
      select(Id, Name, Account = Account_Name_Text__c, Email,
             Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Giving_Circle = Giving_Circle__c,
             Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
             AccountId, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
      mutate(RecordType = 'Contact') %>%
      filter(!is.na(AccountId))
  }
  
  message("Preview of campaignContactsQuery:")
  print(head(campaignContactsQuery))
  
  #' Get Lead info
  campaignLeadsQuery <- tibble::tibble()
  if (nrow(campaignLeads) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c,
              Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c
       FROM Lead
       WHERE Id in ('%s')
       ORDER BY pi__last_activity__c DESC NULLS LAST",
      paste0(campaignLeads$LeadId, collapse = "','")
    )
    
    campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>%
      select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c,
             Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
      mutate(RecordType = 'Lead')
  }
  
  message("Preview of campaignLeadsQuery:")
  print(head(campaignLeadsQuery))
  
  #' Get Account info for contacts with extra fields
  campaignContactAccounts <- tibble::tibble()
  if (nrow(campaignContactsQuery) > 0) {
    my_soql <- sprintf(
      "SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c,
              D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
              D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
              Top_Account__c, Total_Opportunity_Payments__c, Number_of_Open_Opportunities__c,
              Constituent_Groups__c, Constituent_Sub_Groups__c
       FROM Account
       WHERE Id in ('%s')",
      paste0(campaignContactsQuery$AccountId, collapse = "','")
    )
    
    campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>%
      select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
             TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c,
             Constituent_Groups__c, Constituent_Sub_Groups__c)
  }
  
  message("Preview of campaignContactAccounts:")
  print(head(campaignContactAccounts))
  
  #' Join accounts to contacts and leads
  if (nrow(campaignContactsQuery) > 0) {
    campaignContactsQuery <- campaignContactsQuery %>%
      left_join(select(campaignContactAccounts, -Account), by = "AccountId")
  }
  
  if (nrow(campaignLeadsQuery) > 0) {
    campaignLeadsQuery <- campaignLeadsQuery %>%
      left_join(campaignContactAccounts, by = "Account")
  }
  
  contactsLeads <- plyr::rbind.fill(campaignContactsQuery, campaignLeadsQuery) %>%
    mutate(Giving_Circle = as.character(Giving_Circle))
  
  #' Join back to campaign member base
  CampaignMembers <- campaign_members %>%
    mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>%
    select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>%
    left_join(contactsLeads, by = c("Id", "Name"))
  
  #' Type-based enrichment
  if (campaignType == 'Event') {
    message("Unique Status values before transformation:")
    print(unique(CampaignMembers$Status))
    
    CampaignMembers <- CampaignMembers %>%
      mutate(
        EngagementType = 'Event',
        Status = case_when(
          grepl("Attended( - Staff)?|Speaker|\\+1 Speaker Guest|Registered( - Full Day| - High Priority| - Staff| but did not attend| - Hybrid| - In Person| - Virtual)?|De-registered|Waitlist|Attending|WAITLIST", Status) ~ 'Registered',
          TRUE ~ 'Other'
        )
      ) %>%
      filter(grepl('Registered|Other', Status))
  } else if (campaignType == 'Report') {
    CampaignMembers <- CampaignMembers %>%
      mutate(EngagementType = 'Report Download')
  }
  
  #' Final formatting
  CampaignMembers <- CampaignMembers %>%
    mutate(
      Domain = ifelse("Email" %in% names(CampaignMembers), sub("(.*)\\@", "", Email), NA),
      EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")
    ) %>%
    select(any_of(c(
      "CampaignName", "EngagementType", "Id", "RecordType", "Status", "EngagementDate",
      "Name", "Email", "Domain", "Account", "AccountType", "Industry",
      "TotalGiving", "NumOpenOpps", "Pardot_Score", "Pardot_URL", "Giving_Circle",
      "Last_Gift", "AccountId", "Account_Constituent_Groups__c", "Account_Constituent_Sub_Groups__c"
    )))
  
  message("Preview of CampaignMembers after transformation:")
  print(head(CampaignMembers))
  
  return(CampaignMembers)
}



#testing
# my_soql_test <- "SELECT Id, Name, Constituent_Groups__c FROM Account WHERE Constituent_Groups__c != NULL LIMIT 10"
# test_result <- sf_query(my_soql_test, "Account", api_type = "Bulk 1.0")
# print(test_result)
# 
# #testing
# my_soql_test2 <- "SELECT Id, Name, Account.Constituent_Groups__c FROM Contact WHERE Account.Constituent_Groups__c != NULL LIMIT 10"
# test_result2 <- sf_query(my_soql_test, "Contact", api_type = "Bulk 2.0")
# print(test_result2)
# 
# my_soql_test3 <- "SELECT Id, Name, Likely_Account__r.Constituent_Groups__c FROM Lead LIMIT 10"
# test_result3 <- sf_query(my_soql_test, "Lead", api_type = "REST")
# print(test_result3)




## NEW GET CAMPAIGNMEMBERS
#' getCampaignMembers2 <- function(nameOfInterest, campaignType) {
#'   
#'   #' get campaign members
#'   my_soql <- sprintf("SELECT CampaignId,
#'                              Name,
#'                              Status,
#'                              HasResponded,
#'                              ContactId,
#'                              LeadId,
#'                              CreatedDate
#' 
#'                       FROM CampaignMember
#'                       WHERE Name in ('%s')",
#'                      paste0(nameOfInterest, collapse = "','"))
#'   
#'   campaign_members <- sf_query(my_soql, 'CampaignMember', api_type = "Bulk 1.0")%>% 
#'     left_join(select(campaignList, c(CampaignId = Id, CampaignName = Name)), by = 'CampaignId') %>% 
#'     filter(!(is.na(as.character(ContactId)) & is.na(LeadId)))
#'   
#'   campaignContacts <- campaign_members %>% 
#'     filter(!is.na(ContactId)) %>% 
#'     distinct(ContactId, .keep_all = TRUE) 
#'   
#'   campaignLeads <- campaign_members %>% 
#'     filter(!is.na(LeadId) & is.na(ContactId)) %>% 
#'     distinct(LeadId, .keep_all = TRUE) 
#'   
#'   #' get info for contacts in campaign
#'   my_soql <- sprintf("SELECT Id, Name, Account_Name_Text__c, Email,
#'                              pi__url__c, pi__score__c, pi__last_activity__c,
#'                              Giving_Circle__c, Number_of_Days_since_Last_Gift_or_SC__c, AccountId
#'                            
#'                     FROM Contact
#'                     WHERE Id in ('%s')
#'                     ORDER BY pi__last_activity__c DESC NULLS LAST",
#'                      paste0(campaignContacts$ContactId, collapse = "','"))
#'   
#'   campaignContactsQuery <- sf_query(my_soql, 'Contact', api_type = "Bulk 1.0") %>% 
#'     select(Id, Name, Account = Account_Name_Text__c, Email, 
#'            Pardot_URL = pi__url__c, Pardot_Score = pi__score__c, 
#'            Giving_Circle = Giving_Circle__c, Last_Gift = Number_of_Days_since_Last_Gift_or_SC__c,
#'            AccountId) %>% 
#'     mutate(RecordType = 'Contact') %>% 
#'     filter(!is.na(AccountId))
#'   
#'   #' get info for leads in campaign
#'   my_soql <- sprintf("SELECT Id,  Name, Company, Email, pi__url__c, pi__score__c, pi__last_activity__c
#' 
#'                       FROM Lead
#'                       WHERE Id in ('%s')
#'                       ORDER BY pi__last_activity__c DESC NULLS LAST",
#'                      paste0(campaignLeads$LeadId, collapse = "','"))
#'   
#'   campaignLeadsQuery <- sf_query(my_soql, 'Lead', api_type = "Bulk 1.0") %>% 
#'     select(Id, Name, Account = Company, Email, Pardot_URL = pi__url__c, Pardot_Score = pi__score__c) %>% 
#'     mutate(RecordType = 'Lead') 
#'   
#'   #' get info for accounts that match the AccountID field pulled from contacts query
#'   my_soql <- sprintf("SELECT Id, Name, Type, Industry, Email_Domain__c, D_B_Business_Name__c, D_B_Company_Description__c, 
#'                       D_B_Major_Industry_Category_Name__c, D_B_NAICS_Description_1__c, D_B_NAICS_Description_2__c,
#'                       D_B_SIC4_Code_1_Description__c, D_B_SIC4_Code_2_Description__c,
#'                       Top_Account__c, Total_Opportunity_Payments__c, of_Gifts__c, Number_of_Open_Opportunities__c,
#'                       Prospect_Program_Interests__c, Affinity__c, Program_Interest_Multi_Select_Text__c
#' 
#'                       FROM Account 
#'                       WHERE Id in ('%s')", 
#'                      paste0(campaignContactsQuery$AccountId, collapse = "','"))
#'   
#'   campaignContactAccounts <- sf_query(my_soql, 'Account', api_type = "Bulk 1.0") %>% 
#'     select(AccountId = Id, Account = Name, AccountType = Type, Industry, AccountDomain = Email_Domain__c,
#'            TotalGiving = Total_Opportunity_Payments__c, NumOpenOpps = Number_of_Open_Opportunities__c)
#'   
#'   #' join
#'   campaignContactsQuery <- campaignContactsQuery %>% 
#'     left_join(select(campaignContactAccounts, -Account), by = c('AccountId'))
#'   
#'   campaignLeadsQuery <- campaignLeadsQuery %>% 
#'     left_join(campaignContactAccounts, by = c('Account')) 
#'   
#'   contactsLeads <- campaignContactsQuery %>%
#'     plyr::rbind.fill(campaignLeadsQuery) 
#'   
#'   #' join to campaign members df
#'   CampaignMembers <- campaign_members %>% 
#'     mutate(Id = ifelse(is.na(ContactId), LeadId, ContactId)) %>% 
#'     select(CampaignId, CampaignName, Name, Id, Status, CreatedDate) %>% 
#'     left_join(contactsLeads, by = c('Id', 'Name'))
#'   
#'   #' specify campaign type
#'   if(campaignType == 'Event'){
#'     
#'     CampaignMembers <- CampaignMembers %>% 
#'       mutate(EngagementType = 'Event',
#'              #' rename status categories and filter for rows where status is "registered" or "attended"
#'              #' Updated on 1/17/24 to included registrations as attended
#'              #' "Registered - In Person", "WAITLIST" , "Registered - Virtual", and "Clicked to Register"
#'              # Status = ifelse(grepl('Register', Status), 'Registered (Did Not Attend)', Status),
#'              Status = case_when(grepl('Register', Status) ~ 'Attended',
#'                                 grepl('WAITLIST', Status) ~ 'Attended',
#'                                 TRUE ~ Status)) %>%
#'       
#'       filter(grepl('Register|Attended', Status))
#'     
#'   } 
#'   
#'   else if (campaignType == 'Report'){
#'     
#'     CampaignMembers <- CampaignMembers %>% 
#'       mutate(EngagementType = 'Report Download') 
#'     
#'   }
#'   
#'   CampaignMembers <- CampaignMembers %>% 
#'     #' get domain from email
#'     mutate(Domain = sub("(.*)\\@", "", Email),
#'            EngagementDate = as.Date(CreatedDate, format = "%Y-%m-%d")) %>% 
#'     #' select and reorder columns
#'     select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry, 
#'            TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId)
#'   
#'   return(CampaignMembers)
#'   
#' }

##' get campaigns and clean 
##' 

cleanCampaignDF <- function(df){
  
  df <- df %>% 
    filter(!is.na(Domain)) %>% 
    #  distinct(Id, CampaignName, .keep_all = TRUE) %>% 
    mutate(
      DonorType = case_when(
        !is.na(Giving_Circle) ~ as.character(Giving_Circle),
        !is.na(Last_Gift)  ~ "Donor",
        TRUE ~ NA_character_
      ),
      Icon = case_when(
        EngagementType == 'Report Download' ~ 1,
        EngagementType == 'Event' ~ 2, 
        EngagementType == 'Newsletter' ~ 3
      ),
      Pardot_ID = sub("(.*)=", "", Pardot_URL)
    ) %>% 
    left_join(select(all_accounts, c(
      AccountsName = Account, Domain = AccountDomain, 
      AccountsIndustry = Industry, AccountType2 = AccountType
    )), by = c('Domain')) %>% 
    mutate(
      Account = ifelse(grepl('unknown|not provided|contacts created', tolower(Account))|Account == '', 'Unknown', Account),
      Industry = ifelse(Account == 'Unknown' & !is.na(Industry), AccountsIndustry, Industry),
      AccountType = ifelse(Account == 'Unknown' | (is.na(AccountType) & !is.na(AccountType2)), AccountType2, AccountType),
      Account = ifelse(Account == 'Unknown' & !is.na(AccountsName), AccountsName, Account),
      Account = ifelse(is.na(Account), 'Unknown', Account)
    ) %>% 
    left_join(select(govDomains, c(govDomain = domain, level, govName = name)), by = c('Domain' = 'govDomain')) %>%
    mutate(
      Account = case_when(
        is.na(Account) ~ 'Unknown',
        grepl('Household', Account) | Account == 'RMI' ~ 'Household',
        Account == 'Unknown' & !is.na(govName) ~ govName,
        TRUE ~ Account
      ),
      Audience1 = case_when(
        level == 'FEDERAL' ~ 'National Gov.',
        level == 'STATE'|grepl('state of|commonwealth of', tolower(Account)) ~ 'State Gov.',
        level == 'LOCAL'|level == 'COUNTY'|grepl('city of|county of', tolower(Account)) ~ 'Local Gov.',
        level == 'INTERNATIONAL' ~ 'International Gov.'
      )
    ) %>% 
    left_join(select(audienceAccounts, c(Account, type)), by = c('Account')) %>%
    mutate(Audience1 = ifelse(!is.na(type) & is.na(Audience1), type, Audience1)) %>% 
    select(-type) %>% 
    left_join(select(audienceDomains, c(Domain, type)), by = c('Domain')) %>%
    mutate(
      Audience1 = ifelse(!is.na(type) & is.na(Audience1), type, Audience1),
      Audience1 = case_when(
        grepl('Corporate', AccountType) & is.na(Audience1) ~ 'Other Corporate',
        grepl('Foundation', AccountType) & is.na(Audience1)  ~ 'Foundation',
        (grepl('Academic', AccountType)) & (Account != '' | is.na(Account) | Account != 'Unknown') ~ 'Academic',
        Account == 'Unknown'|is.na(Audience1) ~ 'N/A',
        TRUE ~ Audience1
      ),
      Audience2 = ifelse(grepl('Gov', Audience1), 'Government', Audience1)
    ) %>% 
    mutate(
      DownloadTF = ifelse(grepl('Download', EngagementType), 1, NA),
      EventTF = ifelse(grepl('Attended', Status), 1, NA),
      EmailClickTF = ifelse(grepl('Newsletter', EngagementType), 1, NA),
      GivingCircleTF = ifelse(DonorType == 'Solutions Council'|DonorType == 'Innovators Circle', 1, NA),
      SolutionsCouncilTF = ifelse(DonorType == 'Solutions Council', 1, NA),
      InnovatorsCircleTF = ifelse(DonorType == 'Innovators Circle', 1, NA),
      OpenOppTF = ifelse(NumOpenOpps == ''|NumOpenOpps == 0|is.na(NumOpenOpps), NA, 1),
      DonorTF = ifelse(DonorType == ''|is.na(DonorType), NA, 1),
      LastGift = as.numeric(Last_Gift),
      LapsedDonorsTF = ifelse((LastGift > 549 & LastGift < 1825), 1, NA),
      Engagements = ifelse(grepl('Attended|Other', Status), 0, 1)
    )
  
  # ---- Final column selection ----
  df <- df %>%
    select(
      CampaignName, EngagementType, Icon, Id, Status, EngagementDate, Domain, Email, 
      DonorType, AccountId, Account, AccountType, Audience1, Audience2, Industry, TotalGiving, 
      Name, Pardot_Score, Pardot_URL, Pardot_ID, GivingCircleTF, SolutionsCouncilTF, 
      InnovatorsCircleTF, OpenOppTF, DonorTF, LapsedDonorsTF, DownloadTF, EventTF, 
      EmailClickTF, Engagements, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c, 
      ConId, Group_Name__c, Parent_Group__c, Type__c, 
      identifyingID, campaignID
    )
  
  return(df)
}


cleanCampaignDFOLD <- function(df){
  
  df <- df %>% 
    filter(!is.na(Domain)) %>% 
    distinct(Id, CampaignName, .keep_all = TRUE) %>% 
    mutate(
      DonorType = 
        #' define donor type
        case_when(
          !is.na(Giving_Circle) ~ as.character(Giving_Circle),
          !is.na(Last_Gift)  ~ "Donor",
          TRUE ~ NA_character_)
      ,
      #' define icon for Power BI
      Icon = 
        case_when(
          EngagementType == 'Report Download' ~ 1,
          EngagementType == 'Event' ~ 2, 
          EngagementType == 'Newsletter' ~ 3),
      #' get Pardot ID from Pardot URL
      Pardot_ID = sub("(.*)=", "", Pardot_URL)) %>% 
    #' join all accounts based on email domain
    #' use account name, account type, and industry if these fields are empty
    left_join(select(all_accounts, c(AccountsName = Account, Domain = AccountDomain, AccountsIndustry = Industry, AccountType2 = AccountType)), by = c('Domain')) %>% 
    mutate(Account = ifelse(grepl('unknown|not provided|contacts created', tolower(Account))|Account == '', 'Unknown', Account),
           Industry = ifelse(Account == 'Unknown' & !is.na(Industry), AccountsIndustry, Industry),
           AccountType = ifelse(Account == 'Unknown' | (is.na(AccountType) & !is.na(AccountType2)), AccountType2, AccountType)) %>% 
    mutate(Account = ifelse(Account == 'Unknown' & !is.na(AccountsName), AccountsName, Account),
           Account = ifelse(is.na(Account), 'Unknown', Account)) %>% 
    #' join info for government accounts using domain info specified in the govDomains file
    left_join(select(govDomains, c(govDomain = domain, level, govName = name)), by = c('Domain' = 'govDomain')) %>%
    mutate(
      Account = 
        case_when(
          is.na(Account) ~ 'Unknown',
          #' label all Household accounts as Household (remove individual names)
          #' label all RMI accounts as Household
          grepl('Household', Account) | Account == 'RMI' ~ 'Household',
          #' use account name matches from govDomains file if account name is unknown
          Account == 'Unknown' & !is.na(govName) ~ govName,
          TRUE ~ Account
        ),
      #' create audience grouping to categorize accounts that fall under Government, NGO, Multilaterals, 
      #' Financial Entities, Utilities/Power Generators, Other Corporate, Academic, or Foundation
      Audience1 = 
        case_when(
          level == 'FEDERAL' ~ 'National Gov.',
          level == 'STATE'|grepl('state of|commonwealth of', tolower(Account)) ~ 'State Gov.',
          level == 'LOCAL'|level == 'COUNTY'|grepl('city of|county of', tolower(Account)) ~ 'Local Gov.',
          level == 'INTERNATIONAL' ~ 'International Gov.'
        )) %>% 
    left_join(select(audienceAccounts, c(Account, type)), by = c('Account')) %>%
    mutate(Audience1 = ifelse(!is.na(type) & is.na(Audience1), type, Audience1)) %>% 
    select(-type) %>% 
    left_join(select(audienceDomains, c(Domain, type)), by = c('Domain')) %>%
    mutate(Audience1 = ifelse(!is.na(type) & is.na(Audience1), type, Audience1)) %>% 
    mutate(
      Audience1 = 
        case_when(
          grepl('Corporate', AccountType) & is.na(Audience1) ~ 'Other Corporate',
          grepl('Foundation', AccountType) & is.na(Audience1)  ~ 'Foundation',
          (grepl('Academic', AccountType)) & (Account != '' | is.na(Account) | Account != 'Unknown') ~ 'Academic',
          Account == 'Unknown'|is.na(Audience1) ~ 'N/A',
          TRUE ~ Audience1
        ),
      Audience2 = ifelse(grepl('Gov', Audience1), 'Government', Audience1)
    ) %>% 
    #' create True or False columns for each of the following variables to make 
    #' data manipulation easier on Power BI
    mutate(DownloadTF = ifelse(grepl('Download', EngagementType), 1, NA),
           EventTF = ifelse(grepl('Attended', Status), 1, NA),
           EmailClickTF = ifelse(grepl('Newsletter', EngagementType), 1, NA),
           GivingCircleTF = ifelse(DonorType == 'Solutions Council'|DonorType == 'Innovators Circle', 1, NA),
           SolutionsCouncilTF = ifelse(DonorType == 'Solutions Council', 1, NA),
           InnovatorsCircleTF = ifelse(DonorType == 'Innovators Circle', 1, NA),
           OpenOppTF = ifelse(NumOpenOpps == ''|NumOpenOpps == 0|is.na(NumOpenOpps), NA, 1),
           DonorTF = ifelse(DonorType == ''|is.na(DonorType), NA, 1),
           LastGift = as.numeric(Last_Gift),
           LapsedDonorsTF = ifelse((LastGift > 549 & LastGift < 1825), 1, NA),
    #       Engagements = ifelse(!grepl('Registered', Status), 1, NA)) %>% 
    # registered = value of 1, 0 for attended or other
           Engagements = ifelse(grepl('Attended|Other', Status), 0, 1)) %>% 
    

    #' select and reorder columns
    # mutate(
    #   Account_Constituent_Groups__c = coalesce(Account_Constituent_Groups__c, AccountsName), 
    #   Account_Constituent_Sub_Groups__c = coalesce(Account_Constituent_Sub_Groups__c, AccountsIndustry)
    # ) %>%
    select(CampaignName, EngagementType, Icon, Id, Status, EngagementDate, Domain, Email, 
           DonorType, AccountId, Account, AccountType, Audience1, Audience2, Industry, TotalGiving, Name, Pardot_Score,
           Pardot_URL, Pardot_ID, GivingCircleTF, SolutionsCouncilTF, InnovatorsCircleTF, OpenOppTF, DonorTF, LapsedDonorsTF, DownloadTF, EventTF, 
           EmailClickTF, Engagements, Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c, identifyingID, campaignID) 
    # separate(Account_Constituent_Groups__c, 
    #          into = c("Constituent Group 1", "Constituent Group 2", "Constituent Group 3"), 
    #          sep = ";", 
    #          fill = "right",   # this will fill any missing values to the right with NA
    #          extra = "drop") %>%
    # separate(Account_Constituent_Sub_Groups__c, 
    #          into = c("Constituent SubGroup 1", "Constituent SubGroup 2", "Constituent SubGroup 3"), 
    #          sep = ";", 
    #          fill = "right",   # this will fill any missing values to the right with NA
    #          extra = "drop") 
    # 
  
  return(df)
}


getSalesforceReports <- function(){
  
  message('getting reports')
  campaignMembersReports <- data.frame()
  
  for (i in seq_along(reportID_data$reportID)) {
    
    # Get the reportID and corresponding campaignID
    current_reportID <- reportID_data$reportID[i]
    current_campaignID <- reportID_data$IDCampaignID[i]
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- getCampaignMembers(current_reportID, 'Report')
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_reportID, campaignID = current_campaignID)
    
    # Combine the results with previous iterations
    campaignMembersReports <- bind_rows(campaignMembersReports, campaignMembers)
  }
  
  reportID_map <- campaignMembersReports %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersReports <- campaignMembersReports %>%
    left_join(reportID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  # 🔹 NEW SECTION: Query Constituent Groups + Join
  ConGroupTable <- sf_query(
    "SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c 
     FROM Constituent_Group__c",
    object_name = "Constituent_Group__c",
    api_type = "Bulk 2.0",
    verbose = TRUE
  )
  
  # Clean and normalize
  ConGroupTable <- ConGroupTable %>%
    filter(!is.na(Account__c)) %>%
    mutate(Parent_Group__c = if_else(is.na(Parent_Group__c), Group_Name__c, Parent_Group__c)) %>%
    rename(ConId = Id)
  
  # Join: creates multiple rows per account if multiple groups exist
  campaignMembersReports <- campaignMembersReports %>%
    left_join(ConGroupTable, by = c("AccountId" = "Account__c"))
  
  campaignMembersReports <- cleanCampaignDF(campaignMembersReports)
  
  return(campaignMembersReports)
  
}


getSalesforceReportsOLD <- function(){

  message('getting reports')
  campaignMembersReports <- data.frame()
  
  for (i in seq_along(reportID_data$reportID)) {
    
    # Get the reportID and corresponding campaignID
    current_reportID <- reportID_data$reportID[i]
    current_campaignID <- reportID_data$IDCampaignID[i]
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- getCampaignMembers(current_reportID, 'Report')
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_reportID, campaignID = current_campaignID)
    
    # Combine the results with previous iterations
    campaignMembersReports <- bind_rows(campaignMembersReports, campaignMembers)
  }
  
  reportID_map <- campaignMembersReports %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersReports <- campaignMembersReports %>%
    left_join(reportID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  
  campaignMembersReports <- cleanCampaignDF(campaignMembersReports)

  return(campaignMembersReports)

}



getSalesforceReports2 <- function(){
  
  message('getting reports')
  campaignMembersReports2 <- data.frame()
  
  for (i in seq_along(reportID_data$reportID)) {
    
    # Get the reportID and corresponding campaignID
    current_reportID <- reportID_data$reportID[i]
    current_campaignID <- reportID_data$IDCampaignID[i]
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- getCampaignMembers(current_reportID, 'Report')
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_reportID, campaignID = current_campaignID)
    
    # Combine the results with previous iterations
    campaignMembersReports2 <- bind_rows(campaignMembersReports, campaignMembers)
  }
  
  reportID_map <- campaignMembersReports2 %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersReports2 <- campaignMembersReports2 %>%
    left_join(reportID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  
  campaignMembersReports2 <- cleanCampaignDF(campaignMembersReports2)
  
  return(campaignMembersReports)
  
}




# old

# getSalesforceReportsOLD <- function(){
#   
#   message('getting reports')
#   campaignMembersReports <- getCampaignMembers(reportID_data$reportID, 'Report')
#   campaignMembersReports <- cleanCampaignDF(campaignMembersReports)
#   
#   return(campaignMembersReports)
#   
# }


# getSalesforceReports2 <- function(nameOfInterest){
#   
#   message('getting reports')
#   campaignMembersReports <- getCampaignMembers2(nameOfInterest, 'Report') 
#   campaignMembersReports <- cleanCampaignDF(campaignMembersReports)
#   
#   return(campaignMembersReports)
#   
# }

##' get campaign member data for events (SF)
##' 
##' 
##' 
##' new, testing
##' 
##' 
##' 

 # unlocking_finance <- eventID_data %>%
 #   filter(eventID == "701Qk00000GiBszIAF")
#   
#   
getSalesforceEventsTesting <- function(){
  
  message('getting events')
  campaignMembersEvents <- data.frame()
  
  for (i in seq_along(unlocking_finance$eventID)) {
    
    # Get the reportID and corresponding campaignID
    current_eventID <- unlocking_finance$eventID[i]
    current_campaignID <- unlocking_finance$IDCampaignID[i]
    message(sprintf('Processing eventID: %s, campaignID: %s', current_eventID, current_campaignID))
    
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- getCampaignMembersEvents(current_eventID, 'Event')
    print(head(campaignMembers))
    
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_eventID, campaignID = current_campaignID)
    
    # Combine the results with previous iterations
    campaignMembersEvents <- bind_rows(campaignMembersEvents, campaignMembers)
  }
  
  message('Combined Campaign Members Events:')
  print(head(campaignMembersEvents))
  
  eventID_map <- campaignMembersEvents %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  message('Event ID Map:')
  print(head(eventID_map))
  
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(eventID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  message('Final Campaign Members Events after join:')
  print(head(campaignMembersEvents))
  
  campaignMembersEvents <- cleanCampaignDF(campaignMembersEvents)
  
  return(campaignMembersEvents)
  
}

#unlockingFinanceTesting <- getSalesforceEventsTesting()

#campaignMembersEvents2 <- getSalesforceEventsTesting()




#og

getSalesforceEvents <- function(){
  
  message('getting events')
  campaignMembersEvents <- data.frame()
  
  for (i in seq_along(eventID_data$eventID)) {
    
    # Get the reportID and corresponding campaignID
    current_eventID <- eventID_data$eventID[i]
    current_campaignID <- eventID_data$IDCampaignID[i]
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- testingGetCampaignMembersEvents(current_eventID, 'Event')
    
    if (!"CampaignId" %in% colnames(campaignMembers)) {
      campaignMembers$CampaignId <- current_eventID
    }
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_eventID, campaignID = current_campaignID)
    
    message('Preview of campaign members:')
    print(head(campaignMembers))
    
    # Combine the results with previous iterations
    campaignMembersEvents <- bind_rows(campaignMembersEvents, campaignMembers)
  }
  
  message('Combined data preview:')
  print(head(campaignMembersEvents))
  
  eventID_map <- campaignMembersEvents %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(eventID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  # 🔹 NEW SECTION: Query Constituent Groups + Join
  ConGroupTable <- sf_query(
    "SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c 
     FROM Constituent_Group__c",
    object_name = "Constituent_Group__c",
    api_type = "Bulk 2.0",
    verbose = TRUE
  )
  
  # Clean and normalize
  ConGroupTable <- ConGroupTable %>%
    filter(!is.na(Account__c)) %>%
    mutate(Parent_Group__c = if_else(is.na(Parent_Group__c), Group_Name__c, Parent_Group__c)) %>%
    rename(ConId = Id)
  
  # Join: creates multiple rows per account if multiple groups exist
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(ConGroupTable, by = c("AccountId" = "Account__c"))
  
  message('Final structure preview:')
  print(str(campaignMembersEvents))
  
  campaignMembersEvents <- cleanCampaignDF(campaignMembersEvents)
  
  return(campaignMembersEvents)
  
}

getSalesforceEvents_single <- function(test_eventID, test_campaignID) {
  
  message('getting event')
  campaignMembersEvents <- data.frame()
  
  # Retrieve the campaign members for this specific reportID
  campaignMembers <- testingGetCampaignMembersEventsOLD(test_eventID, 'Event')
  
  # Add the reportID and campaignID as new columns
  campaignMembers <- campaignMembers %>%
    mutate(identifyingID = test_eventID, campaignID = test_campaignID)
  
  message('Preview of campaign members:')
  print(head(campaignMembers))
  
  # Combine results (just this one event)
  campaignMembersEvents <- bind_rows(campaignMembersEvents, campaignMembers)
  
  eventID_map <- campaignMembersEvents %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(eventID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  # Query Constituent Groups
  ConGroupTable <- sf_query(
    "SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c 
     FROM Constituent_Group__c",
    object_name = "Constituent_Group__c",
    api_type = "Bulk 2.0",
    verbose = TRUE
  )
  
  ConGroupTable <- ConGroupTable %>%
    filter(!is.na(Account__c)) %>%
    mutate(Parent_Group__c = if_else(is.na(Parent_Group__c), Group_Name__c, Parent_Group__c)) %>%
    rename(ConId = Id)
  
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(ConGroupTable, by = c("AccountId" = "Account__c"))
  
  message('Final structure preview:')
  print(str(campaignMembersEvents))
  
  campaignMembersEvents <- cleanCampaignDF(campaignMembersEvents)
  
  return(campaignMembersEvents)
}

#df_test <- getSalesforceEvents_single("701Qk00000Ka1dJIAR", "cop29")



getSalesforceEventsOLD <- function(){
  
  message('getting events')
  campaignMembersEvents <- data.frame()
  
  for (i in seq_along(eventID_data$eventID)) {
    
    # Get the reportID and corresponding campaignID
    current_eventID <- eventID_data$eventID[i]
    current_campaignID <- eventID_data$IDCampaignID[i]
    
    # Retrieve the campaign members for this specific reportID
    campaignMembers <- getCampaignMembersTesting2(current_eventID, 'Event')
    
    # Add the reportID and campaignID as new columns
    campaignMembers <- campaignMembers %>%
      mutate(identifyingID = current_eventID, campaignID = current_campaignID)
    
    message('Preview of campaign members:')
    print(head(campaignMembers))
    
    # Combine the results with previous iterations
    campaignMembersEvents <- bind_rows(campaignMembersEvents, campaignMembers)
  }
  
  message('Combined data preview:')
  print(head(campaignMembersEvents))
  
  eventID_map <- campaignMembersEvents %>%
    filter(!is.na(identifyingID)) %>%
    select(CampaignName, identifyingID, campaignID) %>%
    distinct()
  
  campaignMembersEvents <- campaignMembersEvents %>%
    left_join(eventID_map, by = "CampaignName", suffix = c("", "_filled")) %>%
    mutate(
      identifyingID = coalesce(identifyingID, identifyingID_filled),
      campaignID = coalesce(campaignID, campaignID_filled)
    ) %>%
    select(-identifyingID_filled, -campaignID_filled)
  
  message('Final structure preview:')
  print(str(campaignMembersEvents))
  
  
  campaignMembersEvents <- cleanCampaignDF(campaignMembersEvents)
  
  return(campaignMembersEvents)
  
}
# 
# current_eventID <- "701Qk00000J6WQTIA3"
# campaignMembers <- testingGetCampaignMembersEvents(current_eventID, 'Event')
# # 

# old version
getSalesforceEventsOLD <- function(){
  
  message('getting events')
  campaignMembersEvents <- getCampaignMembers(eventID_data$eventID, 'Event')
  campaignMembersEvents <- cleanCampaignDF(campaignMembersEvents)
  
  return(campaignMembersEvents)
  
}

##' get campaign member data for email clicks (Pardot)
getCampaignNewsletters <- function(){
  
  message('getting newsletter clicks')
  
  #' get email IDs and story URLs from newsletters in this campaign
  emailIDs <- data.frame(id = unique(campaignNewsletters$id))
  storyLinks <- unique(campaignNewsletters$story_url)
  
  #' get clicks for these newsletters
  clicksAll <- getProspectClicks(emailIDs)
  
  #' clean and filter links for clicks on story URLs only
  linkClicks <- clicksAll %>% 
    mutate(url = sub('\\?(.*)', '', url)) %>% 
    filter(grepl(paste(storyLinks, collapse = '|'), url))
  
  #' clean clicks df
  clicksByProspect <- select(linkClicks, c(emailId = list_email_id, Pardot_URL = prospect_id, url, created_at)) %>% 
    #' pardot call doesn't supply contact IDs or Account IDs so join using Pardot IDs embedded in Pardot URLs
    mutate(Pardot_URL = paste0("http://pi.pardot.com/prospect/read?id=", as.character(Pardot_URL)),
           EngagementDate = as.Date(created_at, format="%Y-%m-%d")) %>%
    left_join(prospects, by = c('Pardot_URL'))%>% 
    mutate(emailId = as.character(emailId))%>%
    #left_join(select(allEmailStats, c(emailId = id, CampaignName = name)), by = 'emailId')%>% 
    left_join(
      select(allEmailStats, c(emailId = id, CampaignName = name)), by = 'emailId') %>%
    mutate(Status = 'Email',
           EngagementType = 'Newsletter') %>% 
    mutate(Account = ifelse(grepl('unknown|not provided|contacts created', tolower(Account))|Account == '', 'Unknown', Account))%>% 
    #' left join all accounts using account name
    left_join(select(accountsUnique, c(AccountsName = Account, AccountDomain, Industry, AccountType, TotalGiving, 
                                       NumGifts, NumOpenOpps, AccountId2 = AccountId)), 
              by = c('Account' = 'AccountsName')) %>% 
    #switched Domain to AccountDomain since Domain didn't exist
    select(CampaignName, EngagementType, Id, RecordType, Status, EngagementDate, Name, Email, Domain, Account, AccountType, Industry,
           TotalGiving, NumOpenOpps, Pardot_Score, Pardot_URL, Giving_Circle, Last_Gift, AccountId, 
           Account_Constituent_Groups__c, Account_Constituent_Sub_Groups__c) %>%
    mutate(identifyingID = NA) %>%
    mutate(campaignID = NA)
  
  #' clean df and set audience groups
  campaignMembersEmail <- cleanCampaignDF(clicksByProspect)
  
  campaignNewsletters_unique <- campaignNewsletters %>%
    #maybe switch to change from name to story_url? 
    #select(name, campaignID) %>%
    select(name, campaignID) %>%
    
    distinct()  # Remove any duplicate rows based on 'name' and 'campaignID'
  
  campaignMembersEmail <- campaignMembersEmail %>%
    left_join(campaignNewsletters_unique, by = c("CampaignName" = "name")) %>%
    mutate(campaignID = coalesce(campaignID.x, campaignID.y)) %>%
    select(-campaignID.x, -campaignID.y)  # Remove the old campaignID columns after updating
  
  
  return(campaignMembersEmail)
}


#### Donations ####

#' get opportunities/donations submitted by all contacts in this campaign
#' 1. filter for donations that occurred after interacting with this campaign
#' 2. calculate attributed revenue

#' Donation Revenue Attribution:
#' Donation revenue is attributed if a contact engaged with a campaign component up to 9 months after making a donation. 
#' The full amount is attributed within the first 30 days of a donation. After this point, the value is discounted by 
#' a factor of 0.0041 (1/244 days) for each additional day.
#' If a donor engaged with multiple campaign components, the donation value is divided accordingly.

# testing NEW

#dfTiny <- df[7420:7470, ]

  # for(i in 1:nrow(dfTiny)){
  #   current_id <- trimws(dfTiny[i, 'Pardot_ID'])  # Trim whitespace
  #   print(paste("Querying Pardot_ID:", current_id))
  # 
  #   visitorActivityCall <- GET(paste0('https://pi.pardot.com/api/opportunity/version/4/do/query?format=json&prospect_id=', current_id),
  #                              add_headers(.headers = header4))
  #   visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
  #   print(visitorActivity)

##' get opportunities
getOpportunities2 <- function(dfTiny){
  
  message('getting donations')
  allOpportunities <- data.frame(DonationID = '', DonationDate = '', DonationValue = '', Pardot_ID = '', EngagementDate = '',
                                 EngagementType = '', CampaignName = '', Pardot_Score = '',
                                 TotalGiving = '', DonorType = '')[0,]
  for(i in 1:nrow(dfTiny)){
    current_id <- trimws(dfTiny[i, 'Pardot_ID'])  # Trim whitespace
    print(paste("Querying Pardot_ID:", current_id))
    visitorActivityCall <- GET(paste0('https://pi.pardot.com/api/opportunity/version/4/do/query?format=json&prospect_id=', current_id),
                               add_headers(.headers = header4))
    visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
    print(visitorActivity)
    tryCatch( { 
      if(visitorActivity[["result"]][["total_results"]] != 0){
        donationHistory <- data.frame(
          DonationId = visitorActivity[["result"]][["opportunity"]][["id"]],
          DonationDate = visitorActivity[["result"]][["opportunity"]][["created_at"]],
          DonationValue = visitorActivity[["result"]][["opportunity"]][["value"]],
          DonationStatus = visitorActivity[["result"]][["opportunity"]][["status"]]
        ) %>% 
          filter(DonationStatus == 'Won') %>% 
          cbind(dfTiny[i, 'EngagementDate']) %>% 
          mutate(Pardot_ID = paste(dfTiny[i, 'Pardot_ID']),
                 Id = paste(dfTiny[i, 'Id']),
                 DonationDate = as.Date(DonationDate, format = "%Y-%m-%d"),
                 EngagementDate = as.Date(EngagementDate, format="%Y-%m-%d"),
                 EngagementType = paste(dfTiny[i, 'EngagementType']),
                 CampaignName = paste(dfTiny[i, 'CampaignName']),
                 Pardot_Score = paste(dfTiny[i, 'Pardot_Score']),
                 TotalGiving = paste(dfTiny[i, 'TotalGiving']),
                 DonorType = paste(dfTiny[i, 'DonorType'])) 
        allOpportunities <- allOpportunities %>% rbind(donationHistory)
      }
    }, error = function(e){ NA } )
    
  }
  return(allOpportunities)
}
    

#tinyTest <- getOpportunities2(dfTiny)


# dfTiny <- df[7420:7470, ]
# 
# for(i in 1:nrow(dfTiny)){
#   #used to be v4
#   current_id <- dfTiny[i, 'Pardot_ID']
#   print(paste("Querying Pardot_ID:", current_id))
#   # current_id used to be df[i, 'Pardot_ID']
#   visitorActivityCall <- GET(paste0('https://pi.pardot.com/api/opportunity/version/4/do/query?format=json&prospect_id=', current_id),
#                              add_headers(.headers = header4))
#   visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
#   #new
#   total_results <- visitorActivity[["result"]][["total_results"]]
#   results_summary <- rbind(results_summary, data.frame(Pardot_ID = current_id, Total_Results = total_results))
# }
# print(results_summary)



donationHistory <- data.frame(
  DonationId = character(),
  DonationDate = character(),
  DonationValue = numeric(),
  DonationStatus = character(),
  stringsAsFactors = FALSE
)

##' get opportunities OLD
getOpportunities <- function(dfDonors){
  
  message('getting donations')
  #used to be DonationID
  # allOpportunities <- data.frame(DonationId = '', DonationDate = '', DonationValue = '', Pardot_ID = '', EngagementDate = '',
  #                                EngagementType = '', CampaignName = '', Pardot_Score = '',
  #                                TotalGiving = '', DonorType = '')[0,]
  allOpportunities <- data.frame(
    DonationId = character(),
    DonationDate = as.Date(character()),
    DonationValue = numeric(),
    Pardot_ID = character(),
    EngagementDate = as.Date(character()),
    EngagementType = character(),
    CampaignName = character(),
    Pardot_Score = numeric(),
    TotalGiving = numeric(),
    DonorType = character(),
    stringsAsFactors = FALSE
  )
   
  for(i in 1:nrow(dfDonors)){
    #used to be v4
    current_id <- dfDonors[i, 'Pardot_ID']
    print(paste("Querying Pardot_ID:", current_id))
    # current_id used to be df[i, 'Pardot_ID']
    visitorActivityCall <- GET(paste0('https://pi.pardot.com/api/opportunity/version/4/do/query?format=json&prospect_id=', current_id),
                               add_headers(.headers = header4))
    visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
    print(visitorActivity)
   
    if(status_code(visitorActivityCall) == 200){
      visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
      
     tryCatch( { 
      if(visitorActivity[["result"]][["total_results"]] != 0){
        
        donationHistory <- data.frame(
          DonationId = visitorActivity[["result"]][["opportunity"]][["id"]],
          DonationDate = visitorActivity[["result"]][["opportunity"]][["created_at"]],
          DonationValue = as.numeric(visitorActivity[["result"]][["opportunity"]][["value"]]),
          DonationStatus = visitorActivity[["result"]][["opportunity"]][["status"]],
          stringsAsFactors = FALSE
          ) %>% 
          filter(DonationStatus == 'Won') %>% 
          cbind(EngagementDate = dfDonors[i, 'EngagementDate']) %>% 
          mutate(Pardot_ID = as.character(dfDonors[i, 'Pardot_ID']),
                 Id = as.character(dfDonors[i, 'Id']),
                 DonationDate = as.Date(DonationDate, format = "%Y-%m-%d"),
                 EngagementDate = as.Date(EngagementDate, format="%Y-%m-%d"),
                 EngagementType = as.character(dfDonors[i, 'EngagementType']),
                 CampaignName = as.character(dfDonors[i, 'CampaignName']),
                 Pardot_Score = as.numeric(dfDonors[i, 'Pardot_Score']),
                 TotalGiving = as.numeric(dfDonors[i, 'TotalGiving']),
                 DonorType = as.character(dfDonors[i, 'DonorType'])) 
        
        allOpportunities <- rbind(allOpportunities, donationHistory)
      }
    }
    , error = function(e){        
      print(paste("Error processing Pardot_ID:", current_id, "Error message:", e))
} )
    } else {
      print(paste("API request failed for Pardot_ID:", current_id))
      
    }
    
  }
  return(allOpportunities)
}

getAttributedDonationValueSF <- function(df){
  
  if(nrow(df) > 0){
    
    #' get all donors in this campaign; add campaignID
    donors <- df %>% 
      select(Pardot_ID, AccountId, CampaignName, DonorType, EngagementType, CampaignName, EngagementDate, Id, Pardot_Score, TotalGiving, Account) %>% 
      distinct() %>% 
      filter(!is.na(DonorType))
    
    #' get donation history for these donors
    allOpps <- getOpportunitiesSF(donors) 
    
    #' select donations made after campaign - ONLY 25
    donations <- allOpps %>% 
      filter(CloseDate > EngagementDate)
    
    # find donations that touch multiple campaign components
    uniqueDonations <- donations %>% 
      group_by(AccountId) %>% 
      summarize(count = n())
    
    donations <- donations %>% 
      #' calculate attributed donation value
      mutate(DonationValue = as.numeric(TotalGift),
             TimeDifference = difftime(CloseDate, EngagementDate, units = "days"),
             TimeDifference = as.numeric(gsub("[^0-9.-]", "", TimeDifference)),
             TimeDifferenceAdjusted = ifelse(TimeDifference < 31, 1, TimeDifference),
             AttributtedValue = ifelse(DonationValue / (1/(1.0041 - 0.0041*TimeDifferenceAdjusted)) < 0, 0, DonationValue / (1/(1.0041 - 0.0041*TimeDifferenceAdjusted)))) %>% 
      relocate(DonationValue, .before = AttributtedValue) %>%  
      #' adjust attribution value for donations that attributed to multiple campaign components
      left_join(uniqueDonations) %>% 
      mutate(AttributtedValue = round(AttributtedValue/count, 1)) %>% 
      filter(AttributtedValue > 0) %>% 
      select(-TimeDifferenceAdjusted) 
    
    return(donations)
  } else {
    return( NA )
  }
  
}

# Checked that Total Gift summed  = Total Giving!
getOpportunitiesSF <- function(dfDonors){
  
  message('getting donations')
  ids <- paste0("'", unique(dfDonors$AccountId), "'", collapse = ",")
    
  my_soql <- sprintf("SELECT AccountId, CloseDate, Gift_Amount_Matching_Gift_Amount__c, 
                      Opportunity_Account_Type__c, Type
                    
    FROM Opportunity
    WHERE AccountId IN (%s)
    ", ids)
    
    opportunityAll <- sf_query(my_soql, "Opportunity", api_type = "Bulk 1.0") 
    
    opportunity <- opportunityAll %>%
      group_by(AccountId, CloseDate) %>%
      summarise(
        TotalGift = sum(Gift_Amount_Matching_Gift_Amount__c, na.rm = TRUE), 
        .groups = "drop"
      )
    
    donationHistory <- opportunity %>%
      left_join(dfDonors, by = "AccountId")

    return(donationHistory)
}



# test_id <- df[7380, 'Pardot_ID']  # Replace this with a known valid Pardot_ID
# visitorActivityCall <- GET(paste0('https://pi.pardot.com/api/opportunity/version/4/do/query?format=json&prospect_id=', test_id),
#                            add_headers(.headers = header4))
# visitorActivity <- jsonlite::fromJSON(content(visitorActivityCall, as = "text", encoding = "UTF-8"))
# print(visitorActivity)
# #head(df$Pardot_ID)

#print(test_id)

# if(visitorActivity[["result"]][["total_results"]] != 0) {
#   opportunities <- visitorActivity[["result"]][["opportunity"]]
#   print(opportunities)  # Print the opportunities if they exist
# } else {
#   print("No opportunities found for this prospect ID.")
# }
# 

#' calculate attributed donation value
getAttributedDonationValue <- function(df){
  
  if(nrow(df) > 0){
    
    #' get all donors in this campaign; add campaignID
    donors <- df %>% 
      select(Pardot_ID, CampaignName, DonorType, EngagementType, CampaignName, EngagementDate, Id, Pardot_Score, TotalGiving, Account) %>% 
      distinct() %>% 
      filter(!is.na(DonorType))
    
    #' get donation history for these donors
    allOpps <- getOpportunities(donors) 
    
    #' select donations made after campaign
    donations <- allOpps %>% 
      filter(DonationDate > EngagementDate)
    
    #' find donations that touch multiple campaign components
    uniqueDonations <- donations %>% 
      group_by(DonationId) %>% 
      summarize(count = n())
    
    donations <- donations %>% 
      #' calculate attributed donation value
      mutate(DonationValue = as.numeric(DonationValue),
             TimeDifference = difftime(DonationDate, EngagementDate, units = "days"),
             TimeDifference = as.numeric(gsub("[^0-9.-]", "", TimeDifference)),
             TimeDifferenceAdjusted = ifelse(TimeDifference < 31, 1, TimeDifference),
             AttributtedValue = ifelse(DonationValue / (1/(1.0041 - 0.0041*TimeDifferenceAdjusted)) < 0, 0, DonationValue / (1/(1.0041 - 0.0041*TimeDifferenceAdjusted)))) %>% 
      relocate(DonationValue, .before = AttributtedValue) %>%  
      #' adjust attribution value for donations that attributed to multiple campaign components
      left_join(uniqueDonations) %>% 
      mutate(AttributtedValue = round(AttributtedValue/count, 1)) %>% 
      filter(AttributtedValue > 0) %>% 
      select(-TimeDifferenceAdjusted) 
    
    return(donations)
  } else {
    return( NA )
  }
  
}

#### SPROUT SOCIAL ####

#' metadata request
getMetadata <- function(url) {
  request <- GET(url = paste0('https://api.sproutsocial.com/v1/805215/', url),
                 add_headers(sproutHeader)
  )
  return(jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8")))
}


#sproutHeader <- c(Authorization = paste("Bearer", access_token))

# Function to get metadata for customer tags


# end


#' call for all other requests
getCall <- function(url, args) {
  request <- POST(url = paste0('https://api.sproutsocial.com/v1/805215/', url),
                  body = toJSON(args, auto_unbox = TRUE),
                  add_headers(sproutHeader)
  )
  return(jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8")))
}

#response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))


#' post analytics request - likely where we'll add new topic-level data
sproutPostRequestOLD <- function(page, dateRange, profileIDs, tagged = TRUE){
  
  #' set tagged == FALSE when making call to get all posts (returns distinct posts rather than duplicate posts if a post has multiple tags)
  #' explanation: some posts this far back weren't getting tagged so the internal.tags.id field would return an error
  if(tagged == TRUE) {
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "internal.tags.id",
      "post_type") # added tags
  } else {
    #' remove internal.tags.id field if tagged == FALSE
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "post_type")
  }
  
  #' define arguments
  args <- list(
    "fields" = fields,
    #' profile IDs are specific to an account (e.g. RMI Brand LinkedIn, RMI Buildings Twitter)
    #' you must supply the profile ID of each account you want to include in your request
    "filters" = c(paste0("customer_profile_id.eq", profileIDs),
                  dateRange),
    #' set post metrics
    "metrics" = c("lifetime.impressions", "lifetime.engagements", "lifetime.post_content_clicks", "lifetime.shares_count"), 
    "timezone" = "America/Denver",
    "page" = paste(page))
  
  #' make call
  getStats <- getCall(url = 'analytics/posts', args = args)
  
  #' call returns 50 posts at a time
  #' NULL indicates that you have reached the last page of results so make this call until the paging field returns NULL
  if(is.null(getStats[["paging"]])) {
    postStats <- NULL
  } else if(tagged == TRUE) {
    metrics <- getStats[["data"]][["metrics"]]
    internal <- getStats[["data"]][["internal"]]
    postStats <- getStats[["data"]] %>% 
      select(-c('metrics', 'internal')) %>% 
     # select(-c('metrics')) %>% 
      cbind(metrics) %>% 
      cbind(internal)
  } else if(tagged == FALSE) {
    metrics <- getStats[["data"]][["metrics"]]
    postStats <- getStats[["data"]] %>% 
      select(-c('metrics')) %>% 
      cbind(metrics) 
  }
  
  return(postStats)
}

sproutPostRequest <- function(page, dateRange, profileIDs, tagged = TRUE){
  
  if(tagged == TRUE) {
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "internal.tags.id",
      "post_type")
  } else {
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "post_type")
  }
  
  args <- list(
    "fields" = fields,
    "filters" = c(paste0("customer_profile_id.eq", profileIDs), dateRange),
    "metrics" = c("lifetime.impressions", "lifetime.engagements", "lifetime.post_content_clicks", "lifetime.shares_count"), 
    "timezone" = "America/Denver",
    "page" = paste(page)
  )
  
  getStats <- getCall(url = 'analytics/posts', args = args)
  
  if (is.null(getStats[["paging"]])) {
    postStats <- NULL
  } else {
    df <- getStats[["data"]]
    metrics <- df[["metrics"]]
    internal <- df[["internal"]]
    
    # Remove "metrics" and "internal" safely if they exist
    cols_to_remove <- intersect(c("metrics", "internal"), names(df))
    df <- df %>% select(-all_of(cols_to_remove))
    
    # Bind available pieces
    postStats <- df
    if (!is.null(metrics)) postStats <- cbind(postStats, metrics)
    if (tagged == TRUE && !is.null(internal)) postStats <- cbind(postStats, internal)
  }
  
  return(postStats)
}


# exploreAPIResponse <- function(page, dateRange, profileIDs) {
#   
#   # Define arguments for the API call
#   args <- list(
#     "filters" = c(paste0("customer_profile_id.eq", profileIDs), dateRange),
#     "metrics" = c("lifetime.impressions", "lifetime.engagements", "lifetime.post_content_clicks", "lifetime.shares_count"), 
#     "timezone" = "America/Denver",
#     "page" = paste(page)
#   )
#   
#   # Make the API call
#   getStats <- getCall(url = 'analytics/posts', args = args)
#   
#   if (!is.null(getStats[["data"]])) {
#     str(getStats[["data"]])
#     
#     print(getStats[["data"]])
#   } else {
#     print("No data returned from API.")
#   }
# }
# 
# postStats <- exploreAPIResponse(i, 
#                                dateRange = paste0("created_time.in(", '2023-01-01', "T00:00:00..", currentDate, "T23:59:59)"),
#                                profileIDs = '(3244287, 2528134, 2528107, 2528104, 3354378, 4145669, 4400432, 4613890, 5083459, 5097954, 5098045, 5251820, 5334423, 5403312, 3246632, 5403593, 5403597)') 
# 



#' get all social media posts with tags
#' getAllSocialPosts <- function(){
#'   
#'   #' create data frame to store all posts
#'   allPostsTags <- data.frame(created_time = '', post_type = '', text = '', perma_link = '', 
#'                              lifetime.impressions = '', lifetime.post_content_clicks = '', 
#'                              lifetime.engagements = '', lifetime.shares_count = '', id = '')[0, ]
#'   
#'   #' get all posts from social channels dating back to Jan 1, 2023
#'   #' note: 300 was chosen as an arbitrary ceiling. the call will end well before this point is reached.
#'   for(i in 1:300){
#'     postStats <- sproutPostRequest(i, 
#'                                    dateRange = paste0("created_time.in(", '2023-01-01', "T00:00:00..", currentDate, "T23:59:59)"),
#'                                    profileIDs = '(3244287, 2528134, 2528107, 2528104, 3354378, 4145669, 4400432, 4613890, 5083459, 5097954, 5098045, 5251820, 5334423, 5403312, 3246632, 5403593, 5403597)') 
#'     #' break loop if NULL value is returned (indicates last page reached)
#'     if(is.null(postStats)){ break }
#'     
#'     #' unnest tags
#'     postStats <- postStats %>% unnest(tags)
#'     #' bind query to data frame
#'     allPostsTags <- allPostsTags %>% rbind(postStats)
#'   }
#'   
#'   return(allPostsTags)
#' }

#updated to solve profileIDs issue
#' get all social media posts with tags
getAllSocialPosts <- function(){
  #' create data frame to store all posts
  allPostsTags <- data.frame(created_time = '', post_type = '', text = '', perma_link = '', 
                             lifetime.impressions = '', lifetime.post_content_clicks = '', 
                             lifetime.engagements = '', lifetime.shares_count = '', id = '')[0, ]
  # get current customer profile IDs
  metadeta <- getMetadata(url = 'metadata/customer')
  customers <- metadeta[["data"]]
  
  # filter customers to where network type does not include linkedin
  profileIDs = customers %>%
    filter(!str_detect(network_type, 'linkedin'))
  
  profileIDs <- profileIDs$customer_profile_id
  profileIDs <- paste0('(', paste0(profileIDs, collapse = ', '), ')')
  #' get all posts from social channels dating back to Jan 1, 2023
  #' note: 300 was chosen as an arbitrary ceiling. the call will end well before this point is reached.
  for(i in 1:300){
    postStats <- sproutPostRequest(i, 
                                   dateRange = paste0("created_time.in(", '2023-01-01', "T00:00:00..", currentDate, "T23:59:59)"),
                                   profileIDs = profileIDs)
    
    #' break loop if NULL value is returned (indicates last page reached)
    if(is.null(postStats)){ break }
    #' unnest tags
    postStats <- postStats %>% unnest(tags)
    #' bind query to data frame
    allPostsTags <- allPostsTags %>% rbind(postStats)
  }
  return(allPostsTags)
}

getAllSocialPosts2 <- function(){
  #' create data frame to store all posts
  allPostsTags <- data.frame(created_time = '', post_type = '', text = '', perma_link = '', 
                             lifetime.impressions = '', lifetime.post_content_clicks = '', 
                             lifetime.engagements = '', lifetime.shares_count = '', id = '')[0, ]
  
  # get current customer profile IDs
  metadeta <- getMetadata(url = 'metadata/customer')
  customers <- metadeta[["data"]]
  
  # filter customers to where network type does not include linkedin
  profileIDs = customers %>%
    filter(!str_detect(network_type, 'linkedin'))
  
  profileIDs <- profileIDs$customer_profile_id
  profileIDs <- paste0('(', paste0(profileIDs, collapse = ', '), ')')
  
  #' get all posts from social channels dating back to Jan 1, 2023
  for(i in 1:300){
    postStats <- sproutPostRequest(i, 
                                   dateRange = paste0("created_time.in(", '2023-01-01', "T00:00:00..", currentDate, "T23:59:59)"),
                                   profileIDs = profileIDs)
    
    #' break loop if NULL value is returned (indicates last page reached)
    if(is.null(postStats)){ break }
    
    #' safely unnest tags if present
    if ("tags" %in% names(postStats)) {
      postStats <- postStats %>%
        mutate(tags = purrr::map(tags, ~ if (is.null(.x)) tibble() else as_tibble(.x))) %>%
        unnest(tags)
    }
    
    #' bind query to data frame
    allPostsTags <- allPostsTags %>% rbind(postStats)
  }
  
  return(allPostsTags)
}


#' clean response
cleanPostDFOLD <- function(df, type, linkedin = 'FALSE'){
  
  posts <- df %>% 
    mutate(engagementRate = round(as.numeric(lifetime.engagements)/as.numeric(lifetime.impressions), 3),
           created_time = as.Date(sub('T(.*)', '', created_time)),
           icon = '',
           account = '',
           across(lifetime.impressions:engagementRate, ~ as.numeric(.x))) %>% 
    filter(!is.na(lifetime.impressions)) 
  
  # if (nrow(posts) == 0) {
  #   warning("No posts available after filtering. Returning an empty dataframe.")
  #   return(posts)
  # }
  # 
  for(i in 1:nrow(posts)){
    
    #' rename post types
    #' define platform icon for Power BI
    if(grepl('LINKEDIN_COMPANY_UPDATE', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "LinkedIn"
      posts[i, 'icon'] <- paste(1)
    } else if(grepl('FACEBOOK_POST', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Facebook"
      posts[i, 'icon'] <- paste(2)
    } else if(grepl('TWEET', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Twitter"
      posts[i, 'icon'] <- paste(3)
    } else if(grepl('INSTAGRAM_MEDIA', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Instagram"
      posts[i, 'icon'] <- paste(4)
    }
    
    #' identify accounts using post URLs
    #' explanation: the call doesn't return the account/profileID that made the post so use post links - perma_link field - to identify the account
    #' note: this works for all platforms except LinkedIn, which doesn't include account info in post URLs
    link <- posts[i, 'perma_link']
    
    if(grepl('twitter.com/RockyMtnInst|www.facebook.com/344520634375161|www.instagram.com|linkedin.com|344046974422527', link) & linkedin == 'FALSE'){
      posts[i, 'account'] <- 'RMI Brand'
    } else if(grepl('twitter.com/RMI_Industries', link)){
      posts[i, 'account'] <- 'RMI Industries'
    } else if(grepl('twitter.com/RMIPolicy', link)){
      posts[i, 'account'] <- 'RMI Policy'
    } else if(grepl('twitter.com/RMIBuildings', link)){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(grepl('twitter.com/RMICaribbean|www.facebook.com/101650939303293', link)){
      posts[i, 'account'] <- 'RMI Caribbean'
    } else if(grepl('twitter.com/RMIEmissions', link)){
      posts[i, 'account'] <- 'RMI Emissions'
    } else if(grepl('twitter.com/RMIAfrica', link)){
      posts[i, 'account'] <- 'RMI Africa'
    } else if(grepl('twitter.com/RMIElectricity', link)){
      posts[i, 'account'] <- 'RMI Electricity'
    } else if(grepl('twitter.com/RMICities', link)){
      posts[i, 'account'] <- 'RMI Cities'
    } else if(grepl('twitter.com/ClimateAlignmnt', link)){
      posts[i, 'account'] <- 'CCAF'
    } else if(grepl('https://twitter.com/CFANadvisors', link)){
      posts[i, 'account'] <- 'CFAN'
    } else if(grepl('https://twitter.com/AmoryLovins', link)){
      posts[i, 'account'] <- 'Amory Lovins'
    } else if(grepl('https://twitter.com/KingsmillBond', link)){
      posts[i, 'account'] <- 'Kingsmill Bond'
    } else if(grepl('https://twitter.com/Jon_Creyts', link)){
      posts[i, 'account'] <- 'Jon Creyts'
    }
    
    #' identify LinkedIn program accounts after making separate calls for each program LinkedIn account
    #' after making each call, provide the account name as an argument using the 'linkedin' parameter
    if(linkedin == 'Buildings'){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(linkedin == 'Transportation'){
      posts[i, 'account'] <- 'RMI Transportation'
    } else if(linkedin == 'CFAN'){
      posts[i, 'account'] <- 'CFAN'
    } else if(linkedin == 'CCAF'){
      posts[i, 'account'] <- 'CCAF'
    }
  }
  
  if(type == 'tagged'){
    posts <- posts %>% 
      left_join(select(tags, c(id = tag_id, tag_name = text)), by = 'id') %>% 
      select(created_time, account, post_type, icon, tag_id = id, tag_name, text, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } else {
    posts <- posts %>% 
      select(created_time, account, post_type, icon, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } 
  
  posts <- posts %>% 
    mutate(icon = as.numeric(icon)) %>% 
    filter(grepl('Facebook|LinkedIn|Twitter|Instagram', post_type))
  
  return(posts)
}

#' make separate calls for LinkedIn program accounts - get all LinkedIn posts from these accounts 
#' if requesting tagged posts, call only works if earliest date >= April 2023
getPostsOLD <- function(ids, type){
  
  # AP <- data.frame(created_time = '', post_type = '', text = '', perma_link = '', 
  #                  lifetime.impressions = '', lifetime.post_content_clicks = '', 
  #                  lifetime.engagements = '', lifetime.shares_count = '')[0, ]
  
  AP <- data.frame(created_time = character(), post_type = character(),
                   text = character(), perma_link = character(),
                   lifetime.impressions = numeric(),lifetime.post_content_clicks = numeric(),
                   lifetime.engagements = numeric(), lifetime.shares_count = numeric())
  
  # APT <- data.frame(created_time = '', post_type = '', text = '', perma_link = '', 
  #                   lifetime.impressions = '', lifetime.post_content_clicks = '', 
  #                   lifetime.engagements = '', lifetime.shares_count = '', id = '')[0, ]
  
  APT <- data.frame(created_time = character(), post_type = character(),
                    text = character(), perma_link = character(),
                    lifetime.impressions = numeric(),lifetime.post_content_clicks = numeric(),
                    lifetime.engagements = numeric(), lifetime.shares_count = numeric(),
                    id = integer())
  
  for(i in 1:100){
    if(type == 'tagged'){
      postStats <- sproutPostRequest(i, 
                                     paste0("created_time.in(2023-04-01T00:00:00..", currentDate, "T23:59:59)"), 
                                     profileIDs = ids, 
                                     tagged = TRUE) 
      if(is.null(postStats)){ break }
      postStats <- postStats %>% unnest(tags)
      APT <- APT %>% bind_rows(postStats) # Changed from rbind to bind_rows on 1/31/24 for efficiency
    } else if (type == 'all'){
      postStats <- sproutPostRequest(i, 
                                     dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"), 
                                     profileIDs = ids, 
                                     tagged = FALSE) 
      if(is.null(postStats)){ break }
      AP <- AP %>% bind_rows(postStats) # Changed from rbind to bind_rows on 1/31 for efficiency
    }
  }
  
  if(type == 'tagged') return(APT) else return(AP)
  
}

#' get all posts from LinkedIn program accounts 
#' pass account name as an argument to the linkedin paramater
#' bind all
#' 


# 
# APT <- data.frame(created_time = character(), post_type = character(),
#                     text = character(), perma_link = character(),
#                     lifetime.impressions = numeric(),lifetime.post_content_clicks = numeric(),
#                     lifetime.engagements = numeric(), lifetime.shares_count = numeric(),
#                     id = integer())
# 
# 
# for(i in 1:100){
#   if(type == 'tagged'){
#     postStats <- sproutPostRequest(i, 
#                                    paste0("created_time.in(2023-04-01T00:00:00..", currentDate, "T23:59:59)"), 
#                                    profileIDs = '(5541628)', 
#                                    tagged = TRUE) 
#     if(is.null(postStats)){ break }
#     postStats <- postStats %>% unnest(tags)
#     APT <- APT %>% bind_rows(postStats) }
#    else {}
# }
# 
# type <- 'tagged'
# 
# names(LI_CCAF)
# 
# i <- 5
# postStats <- sproutPostRequest(i, 
#                                paste0("created_time.in(2023-04-01T00:00:00..", currentDate, "T23:59:59)"), 
#                                profileIDs = '(5541628)', 
#                                tagged = TRUE) 
# 
# postStats <- postStats %>% unnest(tags)
# 
# 
# str(LI_CCAF)


#getLIProgramPosts <- function(type){
  #LI_CFAN <- cleanPostDF(getPosts('(5381251)', type = type), type = type, linkedin = 'CFAN')
  #LI_CCAF <- cleanPostDF(getPosts('(5403265)', type = type), type = type, linkedin = 'CCAF')
  #LI_BUILD <- cleanPostDF(getPosts('(5541628)', type = type), type = type, linkedin = 'Buildings')
 # LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  
  #LI_PROGRAM <- LI_CFAN %>% rbind(LI_CCAF) %>% rbind(LI_BUILD)# %>% rbind(LI_TRANSPORT)
  #return(LI_PROGRAM)
#}

getLIProgramPostsOLD <- function(type){
  LI_CFAN <- cleanPostDF(getPosts('(5381251)', type = type), type = type, linkedin = 'CFAN')
  LI_CCAF <- cleanPostDF(getPosts('(5403265)', type = type), type = type, linkedin = 'CCAF')
  LI_BUILD <- cleanPostDF(getPosts('(5541628)', type = type), type = type, linkedin = 'Buildings')
  LI_RMI <- cleanPostDF(getPosts('(2528134)', type = type), type = type, linkedin = 'RMI')
  
  
  # LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  
  LI_PROGRAM <- LI_CFAN %>% rbind(LI_CCAF) %>% rbind(LI_BUILD) %>% rbind(LI_RMI)# %>% rbind(LI_TRANSPORT)
  return(LI_PROGRAM)
}

#' get post metrics (AVGs) based on all posts made over the last year
#' getPostAverages <- function(){
#'   
#'   posts1YR <- data.frame(created_time = '', post_type = '', text = '', perma_link = '',
#'                          lifetime.impressions = '', lifetime.post_content_clicks = '',
#'                          lifetime.engagements = '', lifetime.shares_count = '')[0, ]
#'   
#'   #' get all posts from social channels made over the past year
#'   for(i in 1:300){
#'     postStats <- sproutPostRequest(i, 
#'                                    dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"),
#'                                    profileIDs = '(3244287, 2528134, 2528107, 2528104, 3354378, 4145669, 4400432, 4613890, 5083459, 5097954, 5098045, 5251820, 5334423, 5403312, 3246632, 5403593, 5403597)',
#'                                    tagged = FALSE)
#'     if(is.null(postStats)){ break }
#'     
#'     posts1YR <- posts1YR %>% rbind(postStats)
#'   }
#'   
#'   linkedInAll <- getLIProgramPosts('all')
#'   
#'   allPosts1YR <- cleanPostDF(posts1YR, 'all') %>%
#'     rbind(linkedInAll) %>% 
#'     #' filter out reposts
#'     filter(impressions > 0,
#'            !grepl('ugcPost', perma_link),
#'            account != '')
#'   
#'   posts1YRaverages <- allPosts1YR %>%
#'     group_by(post_type, account) %>%
#'     summarize(
#'       numPosts = n(),
#'       impressionsMedian = round(median(impressions, na.rm = TRUE), 1),
#'       impressionsMean = round(mean(impressions, na.rm = TRUE), 1),
#'       engagementsMedian = round(median(engagements), 1),
#'       engagementsMean = round(mean(engagements), 1),
#'       engrtMedian = round(median(engagementRate), 3),
#'       engrtMean = round(mean(engagementRate), 3)
#'     ) 
#'   
#'   return(posts1YRaverages)
#' }

#' new version, solving for profileID
#' 
getPostAverages <- function(){
  posts1YR <- data.frame(created_time = '', post_type = '', text = '', perma_link = '',
                         lifetime.impressions = '', lifetime.post_content_clicks = '',
                         lifetime.engagements = '', lifetime.shares_count = '')[0, ]
  # get current customer profile IDs
  metadeta <- getMetadata(url = 'metadata/customer')
  customers <- metadeta[["data"]]
  
  profileIDs <- customers$customer_profile_id
  profileIDs <- paste0('(', paste0(profileIDs, collapse = ', '), ')')
  #' get all posts from social channels made over the past year
  for(i in 1:300){
    postStats <- sproutPostRequest(i, 
                                   dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"),
                                   profileIDs = profileIDs,
                                   tagged = FALSE)
    if(is.null(postStats)){ break }
    posts1YR <- posts1YR %>% rbind(postStats)
  }
  linkedInAll <- getLIProgramPosts('all')
  allPosts1YR <- cleanPostDF(posts1YR, 'all') %>%
    rbind(linkedInAll) %>% 
    #' filter out reposts
    filter(impressions > 0,
           !grepl('ugcPost', perma_link),
           account != '')
  posts1YRaverages <- allPosts1YR %>%
    group_by(post_type, account) %>%
    summarize(
      numPosts = n(),
      impressionsMedian = round(median(impressions, na.rm = TRUE), 1),
      impressionsMean = round(mean(impressions, na.rm = TRUE), 1),
      engagementsMedian = round(median(engagements), 1),
      engagementsMean = round(mean(engagements), 1),
      engrtMedian = round(median(engagementRate), 3),
      engrtMean = round(mean(engagementRate), 3)
    ) 
  return(posts1YRaverages)
}


getLIProgramPosts <- function(type){
  LI_CFAN <- cleanPostDF(getPosts('(5381251)', type = type), type = type, linkedin = 'CFAN')
  LI_CCAF <- cleanPostDF(getPosts('(5403265)', type = type), type = type, linkedin = 'CCAF')
  LI_BUILD <- cleanPostDF(getPosts('(5541628)', type = type), type = type, linkedin = 'Buildings')
  LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  LI_RMI <- cleanPostDF(getPosts('(2528134)', type = type), type = type, linkedin = 'RMI')
  
  
  # LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  
  LI_PROGRAM <- LI_CFAN %>% rbind(LI_CCAF) %>% rbind(LI_BUILD) %>% rbind(LI_RMI) %>% rbind(LI_TRANSPORT)
  return(LI_PROGRAM)
}


cleanPostDF <- function(df, type, linkedin = 'FALSE'){
  
  posts <- df %>% 
    mutate(engagementRate = round(as.numeric(lifetime.engagements)/as.numeric(lifetime.impressions), 3),
           created_time = as.Date(sub('T(.*)', '', created_time)),
           icon = '',
           account = '',
           across(lifetime.impressions:engagementRate, ~ as.numeric(.x))) %>% 
    filter(!is.na(lifetime.impressions)) 
  
  # if (nrow(posts) == 0) {
  #   warning("No posts available after filtering. Returning an empty dataframe.")
  #   return(posts)
  # }
  # 
  for(i in 1:nrow(posts)){
    
    #' rename post types
    #' define platform icon for Power BI
    if(grepl('LINKEDIN_COMPANY_UPDATE', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "LinkedIn"
      posts[i, 'icon'] <- paste(1)
    } else if(grepl('FACEBOOK_POST', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Facebook"
      posts[i, 'icon'] <- paste(2)
    } else if(grepl('TWEET', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Twitter"
      posts[i, 'icon'] <- paste(3)
    } else if(grepl('INSTAGRAM_MEDIA', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Instagram"
      posts[i, 'icon'] <- paste(4)
    }
    
    #' identify accounts using post URLs
    #' explanation: the call doesn't return the account/profileID that made the post so use post links - perma_link field - to identify the account
    #' note: this works for all platforms except LinkedIn, which doesn't include account info in post URLs
    link <- posts[i, 'perma_link']
    
    if(grepl('twitter.com/RockyMtnInst|www.facebook.com/344520634375161|www.instagram.com|linkedin.com|344046974422527', link) & linkedin == 'FALSE'){
      posts[i, 'account'] <- 'RMI Brand'
    } else if(grepl('twitter.com/RMI_Industries', link)){
      posts[i, 'account'] <- 'RMI Industries'
    } else if(grepl('twitter.com/RMIPolicy', link)){
      posts[i, 'account'] <- 'RMI Policy'
    } else if(grepl('twitter.com/RMIBuildings', link)){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(grepl('twitter.com/RMICaribbean|www.facebook.com/101650939303293', link)){
      posts[i, 'account'] <- 'RMI Caribbean'
    } else if(grepl('twitter.com/RMIEmissions', link)){
      posts[i, 'account'] <- 'RMI Emissions'
    } else if(grepl('twitter.com/RMIAfrica', link)){
      posts[i, 'account'] <- 'RMI Africa'
    } else if(grepl('twitter.com/RMIElectricity', link)){
      posts[i, 'account'] <- 'RMI Electricity'
    } else if(grepl('twitter.com/Third_Deriv', link)){
      posts[i, 'account'] <- 'RMI Third Derivative'
    } else if(grepl('twitter.com/RMICities', link)){
      posts[i, 'account'] <- 'RMI Cities'
    } else if(grepl('twitter.com/ClimateAlignmnt', link)){
      posts[i, 'account'] <- 'CCAF'
    } else if(grepl('https://twitter.com/CFANadvisors', link)){
      posts[i, 'account'] <- 'CFAN'
    } else if(grepl('https://twitter.com/AmoryLovins', link)){
      posts[i, 'account'] <- 'Amory Lovins'
    } else if(grepl('https://twitter.com/KingsmillBond', link)){
      posts[i, 'account'] <- 'Kingsmill Bond'
    } else if(grepl('https://twitter.com/Jon_Creyts', link)){
      posts[i, 'account'] <- 'Jon Creyts'
    } else if(grepl('facebook.com/photo.php', link)){
      posts[i, 'account'] <- 'RMI Caribbean'
    }
    
    #' identify LinkedIn program accounts after making separate calls for each program LinkedIn account
    #' after making each call, provide the account name as an argument using the 'linkedin' parameter
    if(linkedin == 'Buildings'){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(linkedin == 'Transportation'){
      posts[i, 'account'] <- 'RMI Transportation'
    } else if(linkedin == 'CFAN'){
      posts[i, 'account'] <- 'CFAN'
    } else if(linkedin == 'CCAF'){
      posts[i, 'account'] <- 'CCAF'
    } else if(linkedin == 'RMI'){  # ✅ Add this
      posts[i, 'account'] <- 'RMI Brand'
    }
  }
  
  if(type == 'tagged'){
    posts <- posts %>% 
      left_join(select(tags, c(id = tag_id, tag_name = text)), by = 'id') %>% 
      select(created_time, account, post_type, icon, tag_id = id, tag_name, text, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } else {
    posts <- posts %>% 
      select(created_time, account, post_type, icon, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } 
  
  posts <- posts %>% 
    mutate(icon = as.numeric(icon)) %>% 
    filter(grepl('Facebook|LinkedIn|Twitter|Instagram', post_type))
  
  return(posts)
}


getPosts <- function(ids, type){
  
  AP <- data.frame(created_time = character(), post_type = character(),
                   text = character(), perma_link = character(),
                   lifetime.impressions = numeric(), lifetime.post_content_clicks = numeric(),
                   lifetime.engagements = numeric(), lifetime.shares_count = numeric())
  
  APT <- data.frame(created_time = character(), post_type = character(),
                    text = character(), perma_link = character(),
                    lifetime.impressions = numeric(), lifetime.post_content_clicks = numeric(),
                    lifetime.engagements = numeric(), lifetime.shares_count = numeric(),
                    id = integer())
  
  for(i in 1:100){
    if(type == 'tagged'){
      postStats <- sproutPostRequest(
        i,
        paste0("created_time.in(2023-04-01T00:00:00..", currentDate, "T23:59:59)"),
        profileIDs = ids,
        tagged = TRUE
      )
      
      if (is.null(postStats)) break
      
      # Safely remove "metrics" and "internal" if they exist
      cols_to_remove <- intersect(c("metrics", "internal"), names(postStats))
      if (length(cols_to_remove) > 0) {
        postStats <- postStats %>% select(-all_of(cols_to_remove))
      }
      
      # Fix: handle nested tags
      if ("tags" %in% names(postStats)) {
        postStats <- postStats %>%
          tidyr::unnest_longer(tags) %>%
          tidyr::unnest_wider(tags)
      }
      
      APT <- APT %>% bind_rows(postStats)
      
    } else if (type == 'all') {
      postStats <- sproutPostRequest(
        i,
        dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"),
        profileIDs = ids,
        tagged = FALSE
      )
      
      if (is.null(postStats)) break
      
      cols_to_remove <- intersect(c("metrics", "internal"), names(postStats))
      if (length(cols_to_remove) > 0) {
        postStats <- postStats %>% select(-all_of(cols_to_remove))
      }
      
      AP <- AP %>% bind_rows(postStats)
    }
  }
  
  if(type == 'tagged') return(APT) else return(AP)
}



#### MONDAY.COM ####

#' Monday.com API call
getMondayCall <- function(x) {
  request <- POST(url = "https://api.monday.com/v2",
                  body = list(query = x),
                  encode = 'json',
                  add_headers(Authorization = mondayToken)
  )
  return(jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8")))
}


### new social 

getMetadata <- function(url) {
  request <- GET(url = paste0('https://api.sproutsocial.com/v1/805215/', url),
                 add_headers(sproutHeader)
  )
  return(jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8")))
}

getCall <- function(url, args) {
  request <- POST(url = paste0('https://api.sproutsocial.com/v1/805215/', url),
                  body = toJSON(args, auto_unbox = TRUE),
                  add_headers(sproutHeader)
  )
  return(jsonlite::fromJSON(content(request, as = "text", encoding = "UTF-8")))
}

sproutPostRequest <- function(page, dateRange, profileIDs, tagged = TRUE){
  
  if(tagged == TRUE) {
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "internal.tags.id",
      "post_type")
  } else {
    fields <- c(
      "created_time",
      "perma_link",
      "text",
      "post_type")
  }
  
  args <- list(
    "fields" = fields,
    "filters" = c(paste0("customer_profile_id.eq", profileIDs), dateRange),
    "metrics" = c("lifetime.impressions", "lifetime.engagements", "lifetime.post_content_clicks", "lifetime.shares_count"), 
    "timezone" = "America/Denver",
    "page" = paste(page)
  )
  
  getStats <- getCall(url = 'analytics/posts', args = args)
  
  if (is.null(getStats[["paging"]])) {
    postStats <- NULL
  } else {
    df <- getStats[["data"]]
    metrics <- df[["metrics"]]
    internal <- df[["internal"]]
    
    # Remove "metrics" and "internal" safely if they exist
    cols_to_remove <- intersect(c("metrics", "internal"), names(df))
    df <- df %>% select(-all_of(cols_to_remove))
    
    # Bind available pieces
    postStats <- df
    if (!is.null(metrics)) postStats <- cbind(postStats, metrics)
    if (tagged == TRUE && !is.null(internal)) postStats <- cbind(postStats, internal)
  }
  
  return(postStats)
}

getLIProgramPosts <- function(type){
  LI_CFAN <- cleanPostDF(getPosts('(5381251)', type = type), type = type, linkedin = 'CFAN')
  LI_CCAF <- cleanPostDF(getPosts('(5403265)', type = type), type = type, linkedin = 'CCAF')
  LI_BUILD <- cleanPostDF(getPosts('(5541628)', type = type), type = type, linkedin = 'Buildings')
  LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  LI_RMI <- cleanPostDF(getPosts('(2528134)', type = type), type = type, linkedin = 'RMI')
  
  
  # LI_TRANSPORT <- cleanPostDF(getPosts('(5635317)', type = type), type = type, linkedin = 'Transportation')
  
  LI_PROGRAM <- LI_CFAN %>% rbind(LI_CCAF) %>% rbind(LI_BUILD) %>% rbind(LI_RMI) %>% rbind(LI_TRANSPORT)
  return(LI_PROGRAM)
}



cleanPostDF <- function(df, type, linkedin = 'FALSE'){
  
  posts <- df %>% 
    mutate(engagementRate = round(as.numeric(lifetime.engagements)/as.numeric(lifetime.impressions), 3),
           created_time = as.Date(sub('T(.*)', '', created_time)),
           icon = '',
           account = '',
           across(lifetime.impressions:engagementRate, ~ as.numeric(.x))) %>% 
    filter(!is.na(lifetime.impressions)) 
  
  # if (nrow(posts) == 0) {
  #   warning("No posts available after filtering. Returning an empty dataframe.")
  #   return(posts)
  # }
  # 
  for(i in 1:nrow(posts)){
    
    #' rename post types
    #' define platform icon for Power BI
    if(grepl('LINKEDIN_COMPANY_UPDATE', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "LinkedIn"
      posts[i, 'icon'] <- paste(1)
    } else if(grepl('FACEBOOK_POST', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Facebook"
      posts[i, 'icon'] <- paste(2)
    } else if(grepl('TWEET', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Twitter"
      posts[i, 'icon'] <- paste(3)
    } else if(grepl('INSTAGRAM_MEDIA', posts[i, 'post_type'])){
      posts[i, 'post_type'] <- "Instagram"
      posts[i, 'icon'] <- paste(4)
    }
    
    #' identify accounts using post URLs
    #' explanation: the call doesn't return the account/profileID that made the post so use post links - perma_link field - to identify the account
    #' note: this works for all platforms except LinkedIn, which doesn't include account info in post URLs
    link <- posts[i, 'perma_link']
    
    if(grepl('twitter.com/RockyMtnInst|www.facebook.com/344520634375161|www.instagram.com|linkedin.com|344046974422527', link) & linkedin == 'FALSE'){
      posts[i, 'account'] <- 'RMI Brand'
    } else if(grepl('twitter.com/RMI_Industries', link)){
      posts[i, 'account'] <- 'RMI Industries'
    } else if(grepl('twitter.com/RMIPolicy', link)){
      posts[i, 'account'] <- 'RMI Policy'
    } else if(grepl('twitter.com/RMIBuildings', link)){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(grepl('twitter.com/RMICaribbean|www.facebook.com/101650939303293', link)){
      posts[i, 'account'] <- 'RMI Caribbean'
    } else if(grepl('twitter.com/RMIEmissions', link)){
      posts[i, 'account'] <- 'RMI Emissions'
    } else if(grepl('twitter.com/RMIAfrica', link)){
      posts[i, 'account'] <- 'RMI Africa'
    } else if(grepl('twitter.com/RMIElectricity', link)){
      posts[i, 'account'] <- 'RMI Electricity'
    } else if(grepl('twitter.com/Third_Deriv', link)){
      posts[i, 'account'] <- 'RMI Third Derivative'
    } else if(grepl('twitter.com/RMICities', link)){
      posts[i, 'account'] <- 'RMI Cities'
    } else if(grepl('twitter.com/ClimateAlignmnt', link)){
      posts[i, 'account'] <- 'CCAF'
    } else if(grepl('https://twitter.com/CFANadvisors', link)){
      posts[i, 'account'] <- 'CFAN'
    } else if(grepl('https://twitter.com/AmoryLovins', link)){
      posts[i, 'account'] <- 'Amory Lovins'
    } else if(grepl('https://twitter.com/KingsmillBond', link)){
      posts[i, 'account'] <- 'Kingsmill Bond'
    } else if(grepl('https://twitter.com/Jon_Creyts', link)){
      posts[i, 'account'] <- 'Jon Creyts'
    } else if(grepl('facebook.com/photo.php', link)){
      posts[i, 'account'] <- 'RMI Caribbean'
    }
    
    #' identify LinkedIn program accounts after making separate calls for each program LinkedIn account
    #' after making each call, provide the account name as an argument using the 'linkedin' parameter
    if(linkedin == 'Buildings'){
      posts[i, 'account'] <- 'RMI Buildings'
    } else if(linkedin == 'Transportation'){
      posts[i, 'account'] <- 'RMI Transportation'
    } else if(linkedin == 'CFAN'){
      posts[i, 'account'] <- 'CFAN'
    } else if(linkedin == 'CCAF'){
      posts[i, 'account'] <- 'CCAF'
    } else if(linkedin == 'RMI'){  # ✅ Add this
      posts[i, 'account'] <- 'RMI Brand'
    }
  }
  
  if(type == 'tagged'){
    posts <- posts %>% 
      left_join(select(tags, c(id = tag_id, tag_name = text)), by = 'id') %>% 
      select(created_time, account, post_type, icon, tag_id = id, tag_name, text, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } else {
    posts <- posts %>% 
      select(created_time, account, post_type, icon, impressions = lifetime.impressions, 
             engagements = lifetime.engagements, engagementRate, postClicks = lifetime.post_content_clicks,
             shares = lifetime.shares_count, perma_link, text)
  } 
  
  posts <- posts %>% 
    mutate(icon = as.numeric(icon)) %>% 
    filter(grepl('Facebook|LinkedIn|Twitter|Instagram', post_type))
  
  return(posts)
}


getPosts <- function(ids, type){
  
  AP <- data.frame(created_time = character(), post_type = character(),
                   text = character(), perma_link = character(),
                   lifetime.impressions = numeric(), lifetime.post_content_clicks = numeric(),
                   lifetime.engagements = numeric(), lifetime.shares_count = numeric())
  
  APT <- data.frame(created_time = character(), post_type = character(),
                    text = character(), perma_link = character(),
                    lifetime.impressions = numeric(), lifetime.post_content_clicks = numeric(),
                    lifetime.engagements = numeric(), lifetime.shares_count = numeric(),
                    id = integer())
  
  for(i in 1:100){
    if(type == 'tagged'){
      postStats <- sproutPostRequest(
        i,
        paste0("created_time.in(2023-04-01T00:00:00..", currentDate, "T23:59:59)"),
        profileIDs = ids,
        tagged = TRUE
      )
      
      if (is.null(postStats)) break
      
      # Safely remove "metrics" and "internal" if they exist
      cols_to_remove <- intersect(c("metrics", "internal"), names(postStats))
      if (length(cols_to_remove) > 0) {
        postStats <- postStats %>% select(-all_of(cols_to_remove))
      }
      
      # Fix: handle nested tags
      if ("tags" %in% names(postStats)) {
        postStats <- postStats %>%
          tidyr::unnest_longer(tags) %>%
          tidyr::unnest_wider(tags)
      }
      
      APT <- APT %>% bind_rows(postStats)
      
    } else if (type == 'all') {
      postStats <- sproutPostRequest(
        i,
        dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"),
        profileIDs = ids,
        tagged = FALSE
      )
      
      if (is.null(postStats)) break
      
      cols_to_remove <- intersect(c("metrics", "internal"), names(postStats))
      if (length(cols_to_remove) > 0) {
        postStats <- postStats %>% select(-all_of(cols_to_remove))
      }
      
      AP <- AP %>% bind_rows(postStats)
    }
  }
  
  if(type == 'tagged') return(APT) else return(AP)
}


getPostAverages <- function(){
  posts1YR <- data.frame(created_time = '', post_type = '', text = '', perma_link = '',
                         lifetime.impressions = '', lifetime.post_content_clicks = '',
                         lifetime.engagements = '', lifetime.shares_count = '')[0, ]
  # get current customer profile IDs
  metadeta <- getMetadata(url = 'metadata/customer')
  customers <- metadeta[["data"]]
  
  profileIDs <- customers$customer_profile_id
  profileIDs <- paste0('(', paste0(profileIDs, collapse = ', '), ')')
  #' get all posts from social channels made over the past year
  for(i in 1:300){
    postStats <- sproutPostRequest(i, 
                                   dateRange = paste0("created_time.in(", sixMonthsAgo, "T00:00:00..", currentDate, "T23:59:59)"),
                                   profileIDs = profileIDs,
                                   tagged = FALSE)
    if(is.null(postStats)){ break }
    posts1YR <- posts1YR %>% rbind(postStats)
  }
  linkedInAll <- getLIProgramPosts('all')
  allPosts1YR <- cleanPostDF(posts1YR, 'all') %>%
    rbind(linkedInAll) %>% 
    #' filter out reposts
    filter(impressions > 0,
           !grepl('ugcPost', perma_link),
           account != '')
  posts1YRaverages <- allPosts1YR %>%
    group_by(post_type, account) %>%
    summarize(
      numPosts = n(),
      impressionsMedian = round(median(impressions, na.rm = TRUE), 1),
      impressionsMean = round(mean(impressions, na.rm = TRUE), 1),
      engagementsMedian = round(median(engagements), 1),
      engagementsMean = round(mean(engagements), 1),
      engrtMedian = round(median(engagementRate), 3),
      engrtMean = round(mean(engagementRate), 3)
    ) 
  return(posts1YRaverages)
}





