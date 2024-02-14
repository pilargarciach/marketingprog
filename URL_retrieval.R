# Load the required libraries
library(httr)
library(jsonlite)
library(utils)

# Set up your API key and custom search engine ID
api_key <- "AIzaSyD0Yn8aFxxW16ehNGqAiyu0lOVCLtqlZgk"
cx <- "03f0733f66c0d42a8"

# Define the base URL for the API
base_url <- "https://www.googleapis.com/customsearch/v1"

# Function to get the URL for a given university
get_university_url <- function(university_name) {
  # Encode the university name to handle special characters
  encoded_university_name <- URLencode(university_name)
  
  # Construct the full URL for the API request
  query_url <- paste0(base_url, "?key=", api_key, "&cx=", cx, "&q=", encoded_university_name)
  
  # Check if the URL length is acceptable
  if (nchar(query_url) >   2000) {
    warning(paste("URL too long for", university_name))
    return(NA)
  }
  
  # Send the GET request
  response <- httr::GET(query_url)
  
  # Check if the request was successful
  if (http_status(response)$category == "Success") {
    json_content <- content(response, "parsed")
    if (!is.null(json_content$items)) {
      return(json_content$items[[1]]$link)
    } else {
      warning(paste("No URL found for", university_name))
      return(NA)
    }
  } else {
    warning(paste("Error making the API request for", university_name))
    return(NA)
  }
}

# Read the CSV file into a data frame
AU <- read_csv("AccreditedUniversities.csv")

# Add a new column to the data frame to store the URLs
AU$URL <- NA

# Apply the function to each university in the data frame to get their URLs
AU$URL <- sapply(AU$Institution, get_university_url)

# Print the updated data frame
print(AU)
