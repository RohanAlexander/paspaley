#### Preamble ####
# Purpose: 
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Date: 26 December 2017
# Prerequisites: 
# Issues: 
# To do:

#### Workspace set-up ####
library(XML)
library(xml2)
library(tidyverse)
library(purrr)
library(rvest)
library(stringr)

#### Get a dataframe of all the products and their information available on the general page ####
# Create a list of the html files then create another list just with the file names, finally iterate over each html file and read it in and name it
file_names <-
  list.files(
    path = "data/lists",
    pattern = "*.htm",
    recursive = TRUE,
    full.names = TRUE
  )

# Parse the HTML
all_categories_products <-
  tibble(product = character())
# Loop into each file
system.time(for (i in 1:length(file_names)) {
  # Read in file i
  this_category <- read_html(file_names[i])
  # Find all the products in this file
  this_categories_products <- xml_find_all(this_category, ".//article[contains(@class, 'product-list-item')]") %>%
    # This imposes a function on a list - the list is each of the children nodesets
    map_df(function(x) {
      product <-
        xml_find_first(x, ".//h2[contains(@class, 'product-list-item-title')]") %>% xml_text() %>% as.character()
      data_product_id <-
        xml_attr(x, "data-product-id") %>% as.character()
      price <-
        xml_find_first(x, ".//span[contains(@class, 'money')]") %>% xml_text() %>% as.character()
      url <- 
        xml_find_first(x, ".//figure") %>% xml_attr("data-url") %>% as.character()
      name_of_file <- file_names[i]
      if (length(product) == 0)
        product <- NA
      if (length(data_product_id) == 0)
        data_product_id <- NA
      if (length(price) == 0)
        price <- NA
      if (length(url) == 0)
        url <- NA
      # Pull it all together into a dataframe - if you add another type of field then make sure you add it to the list here
      data_frame(
        product,
        data_product_id,
        price,
        url, 
        name_of_file
      )
    })
  # Add the date and other session details
  category_type <-
    xml_text(xml_find_first(this_category, ".//title"))
  this_categories_products$category_type <-
    rep(category_type, nrow(this_categories_products))
  # Add this metal's products to all of them
  all_categories_products <- rbind(all_categories_products, this_categories_products)
  print(paste(
    "Done with file number",
    i,
    "of",
    length(file_names),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
})

# Clean up
# Change the name
all_products <- all_categories_products %>% drop_na()
# Clear the files
rm(all_categories_products, i, file_names, category_type, this_category, this_categories_products)
# Change the prices to numeric
all_products$price <- sub(",", "", all_products$price)
all_products$price <- sub("\\$", "", all_products$price)
all_products$price <- as.numeric(all_products$price)
# 
all_products$name_of_file <- sub("data/lists/", "", all_products$name_of_file)
all_products <- cbind(all_products, str_split_fixed(all_products$name_of_file, "/", 2))
all_products <- rename(all_products, "grouping" = "1", "grouping_value" = "2")

all_products$category_type <- NULL
all_products$name_of_file <- NULL

unique(all_products$grouping)

lists_by_categories <- all_products %>%
  filter(grouping == "lists_by_categories")
lists_by_collections <- all_products %>%
  filter(grouping == "lists_by_collections")
lists_by_metals <- all_products %>%
  filter(grouping == "lists_by_metals")
lists_by_price <- all_products %>%
  filter(grouping == "lists_by_price")


test <- left_join(lists_by_price, lists_by_categories, by = c("product", "data_product_id", "price"))
test <- left_join(test, lists_by_collections, by = c("product", "data_product_id", "price"))
test <- left_join(test, lists_by_metals, by = c("product", "data_product_id", "price"))

# Correct for the mistake
test <- test[-53,]

names(test)

test <- test %>%
  select(product:url.x, grouping_value.x, grouping_value.y, grouping_value.x.x, grouping_value.y.y)








all_products <- full_join(lists_by_price, lists_by_metals,  by = c("data_product_id")) %>%
  select(product.x, data_product_id, price.x, url.x, grouping_value.x, grouping_value.y) %>%
  rename("product" = "product.x", "price" = "price.x", "url" = "url.x", "lists_by_price" = "grouping_value.x", "lists_by_metals" = "grouping_value.y")


all_products <- full_join(lists_by_price, lists_by_metals,  by = c("data_product_id")) %>%
  select(product.x, data_product_id, price.x, url.x, grouping_value.x, grouping_value.y) %>%
  rename("product" = "product.x", "price" = "price.x", "url" = "url.x", "lists_by_price" = "grouping_value.x", "lists_by_metals" = "grouping_value.y")

all_products <- full_join(all_products, lists_by_collections, by = c("data_product_id")) %>%
  select(product.x, data_product_id, price.x, url.x, lists_by_price, lists_by_metals, grouping_value) %>%
  rename("product" = "product.x", "price" = "price.x", "url" = "url.x", "lists_by_collections" = "grouping_value")

all_products <- full_join(all_products, lists_by_categories, by = c("data_product_id")) %>%
  select(product.x, data_product_id, price.x, url.x, lists_by_price, lists_by_metals, lists_by_collections, grouping_value) %>%
  rename("product" = "product.x", "price" = "price.x", "url" = "url.x", "lists_by_categories" = "grouping_value")

rm(lists_by_categories, lists_by_collections, lists_by_metals, lists_by_price)

all_products$url <- sub("/collections/up-to-au-1500/products/", "", all_products$url)
all_products$url <- sub("/collections/au-1500-au-5000/products/", "", all_products$url)
all_products$url <- sub("/collections/au-5-000-au-10-000/products/", "", all_products$url)
all_products$url <- sub("/collections/over-au-10-000/products/", "", all_products$url)
all_products$lists_by_metals <- sub(".html", "", all_products$lists_by_metals)
all_products$lists_by_price <- sub(".htm", "", all_products$lists_by_price)
all_products$lists_by_price <- sub("au5000to_10,000 – Paspaley", "au5000_to_10,000", all_products$lists_by_price)
all_products$lists_by_collections <- sub(".htm", "", all_products$lists_by_collections)
all_products$lists_by_categories <- sub(".htm", "", all_products$lists_by_categories)













#### Get the descriptions for each product and add to the dataframe ####
# Get all the URLs
product_urls <- paste0("https://www.paspaley.com", all_products$url) 
# Download the website html and then save it
for (i in 1:length(product_urls)) {
  product_data <- read_html(product_urls[i]) 
  product_name <- sub("https://www.paspaley.com/products/", "", product_urls[i])
  name_of_file <- paste0("data/products/", product_name, ".html") 
  write_html(product_data, file=name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(product_urls),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(15)
}
# Clean up
rm(i, name_of_file, product_data, product_name, product_urls)








# Add the descriptions that were scraped into the data
all_products$file_name <- paste0("data/all_products/", all_products$url, ".html")

for (i in 1:nrow(all_products)) {
  # Read in file i
  if (file.exists(all_products$file_name[i])) {
  all_products$description[i] <- xml_find_first(read_html(all_products$file_name[i]), ".//div[contains(@class, 'product-description rte')]") %>%
    xml_text() %>% 
    as.character() }
  else {all_products$description[i] <- NA}
}

rm(i)















#### Get the missing descriptions for each product and add to the dataframe ####
# Get all the URLs
product_urls <- ifelse(is.na(all_products$description), paste0("https://www.paspaley.com/products/", all_products$url), NA)
product_urls <- product_urls[!is.na(product_urls)]
product_urls <- unique(product_urls)

# Download the website html and then save it
for (i in 1:length(product_urls)) {
  product_data <- read_html(product_urls[i]) 
  product_name <- sub("https://www.paspaley.com/products/", "", product_urls[i])
  name_of_file <- paste0("data/all_products/", product_name, ".html") 
  write_html(product_data, file=name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(product_urls),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(15)
}
# Clean up
rm(i, name_of_file, product_data, product_name, product_urls)


test <- spread(all_products, lists_by_collections, data_product_id)

?spread

tester <- reshape(all_products, idvar="data_product_id", timevar="lists_by_collections", direction="wide")


