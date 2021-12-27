library(purrr)
library(rvest)
library(tidyverse)


get_links <- function(file_to_look_at){
  content <- read_html(file_to_look_at) %>% 
    html_nodes(".product-item-info") %>% 
    html_nodes(".product") %>% 
    html_nodes("a")
  
  link <- content %>%
    html_attr("href")
  
  text <- content %>%
    html_text(trim = TRUE)
  
  category <- str_replace(file_to_look_at, "inputs/2021/lists/lists_by_categories/", "") %>% 
    str_replace(".html", "")
  
  return(tibble(link = link, text = text, category = category))
}


the_files <- c(
  "inputs/2021/lists/lists_by_categories/rings.html",
  "inputs/2021/lists/lists_by_categories/bracelet.html",
  "inputs/2021/lists/lists_by_categories/clasps.html",
  "inputs/2021/lists/lists_by_categories/earrings.html",
  "inputs/2021/lists/lists_by_categories/necklace.html",
  "inputs/2021/lists/lists_by_categories/pendants.html"
)


the_data <- 
  purrr::map_df(the_files, get_links)


the_data$product <- str_replace_all(the_data$link, c("https://www.paspaley.com/jewellery/pearl-accessories/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-bracelets/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-clasps/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-earrings/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-necklaces/" = "",
                                                     "https://www.paspaley.com/jewellery/pearl-rings/" = ""))

the_data$year = 2021

write_csv(the_data, "outputs/misc/2021_links_from_categories.csv")
