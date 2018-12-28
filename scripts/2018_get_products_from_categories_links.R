library(rvest)
library(tidyverse)


the_data <- read_csv("outputs/2018_links_from_categories.csv")

the_data <- the_data[142:170,]


for (i in 1:length(the_data$link)) {
  product_data <- read_html(the_data$link[i]) 
  product_name <- str_replace_all(the_data$link[i], "https://www.paspaley.com/jewellery/", "")
  product_name <- str_replace_all(product_name, "/", "-")
  name_of_file <- paste0("inputs/2018/products/", product_name, ".html") 
  write_html(product_data, file = name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(the_data$link),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(30)
}


# Clean up
rm(i, name_of_file, product_data, product_name)


# Throws an error:
# https://www.paspaley.com/jewellery/pearl-necklaces/oval-bella-petite-pendant-white-gold


# Based on 2018_compare_xml_list_with_categories_list.R we also need to go and get these ones:
# baroque-strand
# gold-keshi-rhapsody
# keshi-rope-strand-of-the-harvest
# monsoon-earring-enhancer-l-r-white-gold
# monsoon-earring-enhancer-l-r-yellow-gold
# monsoon-earring-enhancer-left-white-gold
# monsoon-earring-enhancer-left-yellow-gold
# monsoon-earring-enhancer-right-white-gold
# monsoon-earring-enhancer-right-yellow-gold
# petite-circle-strand

also_get <- 
  c(
    "https://www.paspaley.com/jewellery/baroque-strand",
    "https://www.paspaley.com/jewellery/gold-keshi-rhapsody",
    "https://www.paspaley.com/jewellery/keshi-rope-strand-of-the-harvest",
    "https://www.paspaley.com/monsoon-earring-enhancer-l-r-white-gold",
    "https://www.paspaley.com/monsoon-earring-enhancer-l-r-yellow-gold",
    "https://www.paspaley.com/monsoon-earring-enhancer-left-white-gold",
    "https://www.paspaley.com/monsoon-earring-enhancer-left-yellow-gold",
    "https://www.paspaley.com/monsoon-earring-enhancer-right-white-gold",
    "https://www.paspaley.com/monsoon-earring-enhancer-right-yellow-gold",
    "https://www.paspaley.com/jewellery/petite-circle-strand"
  )
also_get <- also_get[4:10]


# Yes, I should have made the above into a function and done this properly, but I didn't. I'll do better next time.
for (i in 1:length(also_get)) {
  product_data <- read_html(also_get[i]) 
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/jewellery/", "")
  product_name <- str_replace_all(product_name, "/", "-")
  name_of_file <- paste0("inputs/2018/products/", product_name, ".html") 
  write_html(product_data, file = name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(also_get),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(30)
}
