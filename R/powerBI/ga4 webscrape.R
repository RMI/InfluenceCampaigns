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

entireWebsiteUrls <- function(propertyID) {
  # Fetch website data from GA4
  all_data <- ga_data(
    propertyID,
    date_range = dateRangeGA,
    dimensions = c("pagePath"),
    metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
    limit = -1
  )
  
  # Convert to a dataframe
  url_data <- as.data.frame(all_data)
  
  # Base domain for constructing full URLs
  base_domain <- "https://rmi.org"
  
  # Create a new dataframe to store the full URLs
  all_urls <- data.frame(pagePath = character(), stringsAsFactors = FALSE)
  
  for (url in url_data$pagePath) {
    full_url <- paste0(base_domain, url)
    
    # Add the full URL to the dataframe
    all_urls <- rbind(all_urls, data.frame(pagePath = full_url, stringsAsFactors = FALSE))
  }
  
  return(all_urls)
}

getentireWebsiteUrls <- entireWebsiteUrls(rmiPropertyID)

# View the results
print(getentireWebsiteUrls)

# getentireWebsiteUrls <- getentireWebsiteUrls %>%
#   mutate(site = "rmi.org")

scrape_metadata <- function(url) {
  tryCatch({
    # Fetch the content of the webpage
    page_content <- read_html(url)
    
    # Extract all <script> tags that contain JSON-LD metadata
    json_ld_list <- page_content %>% 
      html_nodes('script[type="application/ld+json"]') %>% 
      html_text()
    
    # If no JSON-LD scripts found, return NA
    if (length(json_ld_list) == 0) {
      return(list(keywords = NA, datePublished = NA))
    }
    
    # Initialize placeholders for keywords and datePublished
    keywords <- NA
    date_published <- NA
    
    # Iterate over each JSON-LD script and attempt to extract both keywords and datePublished
    for (json_ld in json_ld_list) {
      metadata <- fromJSON(json_ld, flatten = TRUE)
      
      # Extract keywords if they exist
      if (!is.null(metadata$keywords)) {
        keywords <- metadata$keywords
      }
      
      # Extract datePublished if it exists
      if (!is.null(metadata$datePublished)) {
        date_published <- metadata$datePublished
      }
    }
    
    # Return a list with both keywords and datePublished
    return(list(keywords = keywords, datePublished = date_published))
    
  }, error = function(e) {
    return(list(keywords = NA, datePublished = NA)) # Return NA on error
  })
}
# Example of running the function on a single URL
test_url <- "https://rmi.org/insight/unlocking-new-opportunities-for-carbon-neutrality-in-chinas-building-sector/"
metadata <- scrape_metadata(test_url)
print(metadata)



getentireWebsiteUrls2 <- getentireWebsiteUrls %>%
  rowwise() %>%
  mutate(keywords = list(scrape_metadata(pagePath)))

# View the results
print(getentireWebsiteUrls2)




getentireWebsiteUrls$keywords <- NA
getentireWebsiteUrls$datePublished <- NA

for (i in 1:nrow(getentireWebsiteUrls)) {
  url <- getentireWebsiteUrls$pagePath[i]
  
  # Call the modified scrape_metadata function
  metadata <- scrape_metadata(url)
  
  # Handle cases where keywords or datePublished might be missing or empty
  if (length(metadata$keywords) == 0 || is.null(metadata$keywords)) {
    getentireWebsiteUrls$keywords[i] <- NA
  } else {
    getentireWebsiteUrls$keywords[i] <- metadata$keywords
  }
  
  if (length(metadata$datePublished) == 0 || is.null(metadata$datePublished)) {
    getentireWebsiteUrls$datePublished[i] <- NA
  } else {
    getentireWebsiteUrls$datePublished[i] <- metadata$datePublished
  }
  
  # Print progress
  print(paste("Processed", i, "out of", nrow(getentireWebsiteUrls), "URLs"))
}






pivot_table <- getentireWebsiteUrls %>%
  mutate(published_date = as.Date(datePublished)) %>%
  mutate(
    date_category = case_when(
      # April to June 2024
      published_date >= as.Date("2024-04-01") & published_date <= as.Date("2024-06-30") ~ "2024 Q4 (Apr-Jun)",
      
      # July to September 2024
      published_date >= as.Date("2024-07-01") & published_date <= as.Date("2024-09-30") ~ "2025 Q1 (Jul-Sep)",
      
      # July to September 2023
      published_date >= as.Date("2023-07-01") & published_date <= as.Date("2023-09-30") ~ "2024 Q1 (Jul-Sep)",
      
      # Default for any other dates
      TRUE ~ "Other"
    )
  ) 

pivot_table2 <- pivot_table %>%
  filter(date_category != "Other") 


pivot_table3 <- pivot_table2 %>%
  mutate(
    pagePath = str_remove(pagePath, "/$"),  # Remove trailing slash
    content_type = case_when(
      str_detect(pagePath, "reality-check") ~ "reality check",
      str_detect(pagePath, "insight") ~ "report",
      str_detect(pagePath, "press-release") ~ "news",
      str_detect(pagePath, "101") ~ "101",
      str_detect(pagePath, "event") ~ "event",
      TRUE ~ "article"  # Default case
    )
  ) %>%
  distinct(pagePath, .keep_all = TRUE)  # Keep all columns, but only unique pagePath



pivot_table_count <- pivot_table3 %>%
  group_by(date_category, content_type) %>%
  summarize(count = n(), .groups = 'drop')






# 
# 
# 
# getEntireWebsiteData <- function(url){
#   page <- tryCatch({
#     read_html(url)
#   }, error = function(e) return(NULL))  # Return NULL if the page fails to load
#   
#   if (is.null(page)) return(data.frame(url = url, content_type = NA, published_date = NA))
#   
#   # Extract content type from a meta tag (adjust the selector if needed)
#   content_type <- page %>%
#     html_nodes("meta[property='og:type']") %>%
#     html_attr("content") %>%
#     .[1] %>%
#     ifelse(is.na(.), "Unknown", .)
#   
#   # Extract published date (commonly found in 'article:published_time' or 'datePublished' tags)
#   published_date <- page %>%
#     html_nodes("meta[property='article:published_time'], meta[name='datePublished']") %>%
#     html_attr("content") %>%
#     .[1] %>%
#     ifelse(is.na(.), "Unknown", .)
#   
#   return(data.frame(url = url, content_type = content_type, published_date = published_date, stringsAsFactors = FALSE))
# }
# 
# scrape_metadata_from_urls <- function(url_list) {
#   # Initialize an empty dataframe to store results
#   metadata_df <- data.frame(url = character(), content_type = character(), published_date = character(), stringsAsFactors = FALSE)
#   
#   # Loop through each URL and extract metadata
#   for (url in getentireWebsiteUrls$pagePath) {
#     # Extract metadata for the current URL
#     metadata <- getEntireWebsiteData(url)
#     
#     # Append the extracted metadata to the dataframe
#     metadata_df <- rbind(metadata_df, metadata)
#   }
#   
#   return(metadata_df)
# }
# 
# website_metadata <- scrape_metadata_from_urls(getentireWebsiteUrls)
# 
# print(website_metadata)
# 
# 
# #new attempt 
# 
# 
# scrape_metadata <- function(urls) {
#   # Create an empty list to store the results
#   results <- list()
#   
#   # Loop through each URL in the dataframe
#   for (url in urls) {
#     try({
#       # Read the page content
#       page <- read_html(url)
#       
#       # Extract the JSON-LD metadata
#       json_ld <- page %>% 
#         html_nodes('script[type="application/ld+json"]') %>% 
#         html_text() %>% 
#         fromJSON(flatten = TRUE)
#       
#       # Check if 'keywords' field exists and extract it
#       if ("keywords" %in% names(json_ld)) {
#         keywords <- json_ld$keywords
#         content_type <- keywords # You can modify this to return specific tags if needed
#       } else {
#         content_type <- NA  # If no keywords are found, return NA
#       }
#       
#       # Store the result in a list
#       results[[url]] <- list(url = url, content_type = content_type)
#       
#     }, silent = TRUE)
#   }
#   
#   # Convert the list of results to a dataframe
#   return(bind_rows(lapply(results, as.data.frame)))
# }
# 
# 
# scraped_data <- scrape_metadata(getentireWebsiteUrls$pagePath)
# 
# print(scraped_data)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 






  