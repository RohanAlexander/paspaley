library(purrr)
library(rvest)
library(tidyverse)


get_product_data <- function(file_to_look_at){
  # file_to_look_at <- c("inputs/2017/all_products/baroque-pearl-strand.html")

  content <- read_html(file_to_look_at) %>% 
    html_nodes(".product-details-wrapper") 
  
  name <- content %>%
    html_nodes(".product-title") %>% 
    html_text(trim = TRUE)
  
  description <- content %>%
    html_nodes(".product-description") %>%
    html_text(trim = TRUE) 
      
  description <- ifelse(is_empty(description), "none", description)
  # 
  # flowerydescription <- content %>%
  #   html_nodes(".description") %>% 
  #   html_text(trim = TRUE)
  # 
  # flowerydescription <- ifelse(is_empty(flowerydescription), "none", flowerydescription)
  # 
  # availability <- content %>%
  #   html_nodes(".stock") %>% 
  #   html_text(trim = TRUE)
  # 
  sku <- content %>%
    html_nodes(xpath = '//*[@id="shopify-section-product"]/section/div[2]/div/p') %>%
    html_text(trim = TRUE)
  
  price <- content %>%
    html_nodes(".product-price") %>% 
    html_text(trim = TRUE)
  
  return(tibble(
    name = name,
    description = description,
    flowerydescription = NA,
    availability = NA,
    sku = sku,
    price = price
  ))
}



products <- list.files("inputs/2017/all_products", full.names = TRUE)

names(products) <- list.files("inputs/2017/all_products") %>% 
  gsub(pattern = ".html$", replacement = "")

dataset <- purrr::map_df(products, 
                         get_product_data, 
                         .id = "product")

write_csv(dataset, "outputs/2017_dataset.csv")
