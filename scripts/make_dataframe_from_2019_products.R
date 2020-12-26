library(purrr)
library(rvest)
library(tidyverse)


get_product_data <- function(file_to_look_at){
  # file_to_look_at <- c("inputs/2018/products/pearl-earrings-diamond-touchstone-earrings-12mm-oval-yellow-gold.html")
  
  content <- read_html(file_to_look_at) %>% 
    html_nodes(".product-info-main") 
  
    name <- content %>%
      html_nodes(".base") %>% 
      html_text(trim = TRUE)
    
    description <- content %>%
      html_nodes(".short-description") %>% 
      html_text(trim = TRUE)
    
    description <- ifelse(is_empty(description), "none", description)
    
    flowerydescription <- content %>%
      html_nodes(".description") %>% 
      html_text(trim = TRUE)
    
    flowerydescription <- ifelse(is_empty(flowerydescription), "none", flowerydescription)
    
    availability <- content %>%
      html_nodes(".stock") %>% 
      html_text(trim = TRUE)
    
    sku <- content %>%
      html_nodes(".sku") %>% 
      html_text(trim = TRUE)
    
    price <- content %>%
      html_nodes(".price") %>% 
      html_text(trim = TRUE)
    
  return(tibble(
    name = name,
    description = description,
    flowerydescription = flowerydescription,
    availability = availability,
    sku = sku,
    price = price
    ))
}



products <- list.files("inputs/2019/products", full.names = TRUE)

names(products) <- list.files("inputs/2019/products") %>% 
  gsub(pattern = ".html$", replacement = "")

dataset <- purrr::map_df(products, 
                         get_product_data, 
                         .id = "product")

write_csv(dataset, "outputs/data/2019_dataset.csv")
