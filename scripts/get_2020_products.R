library(rvest)
library(tidyverse)


the_categories_data <- read_csv("outputs/misc/2020_links_from_categories.csv") %>% 
  select(link, text)
the_collections_data <- read_csv("outputs/misc/2020_links_from_collections.csv") %>% 
  select(link, text)
the_xml_data <- read_csv("outputs/misc/2020_links_from_xml.csv") %>% 
  rename(text = product_name,
         link = product_link)

all <- rbind(the_categories_data, the_xml_data)

all <- 
  all %>% 
  arrange(text) %>% 
  distinct(text)


the_categories_data <- the_categories_data[1:2,]

for (i in 1:length(the_data$link)) {
  product_data <- read_html(the_data$link[i]) 
  product_name <- str_replace_all(the_data$link[i], "https://www.paspaley.com/jewellery/", "")
  product_name <- str_replace_all(product_name, "/", "-")
  name_of_file <- paste0("inputs/2020/products/", product_name, ".html") 
  write_html(product_data, file = name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(the_data$link),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(15)
}


# Clean up
rm(i, name_of_file, product_data, product_name)


# Throws an error:
# https://www.paspaley.com/jewellery/pearl-necklaces/oval-bella-petite-pendant-white-gold


# Based on compare_2019_xml_list_with_categories_list.R we also need to go and get these ones:
# baroque-strand
# gold-keshi-rhapsody
# keshi-rope-strand-of-the-harvest
# petite-circle-pearl-strand
# rhapsody-keshi-and-circle-strand-necklace
# rhapsody-strand-bracelet
# round-pearl-strand-bracelet
# round-pearl-strand-necklace

also_get <- 
  c(
    "https://www.paspaley.com/jewellery/touchstone-ruby-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/touchstone-black-diamond-clasp-white-gold",
      "https://www.paspaley.com/jewellery/touchstone-diamond-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/maxima-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/maxima-clasp-white-gold",
      "https://www.paspaley.com/jewellery/rockpool-pave-clasp-white-gold",
      "https://www.paspaley.com/jewellery/rockpool-pave-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/rockpool-mother-of-pearl-clasp-white-gold",
      "https://www.paspaley.com/jewellery/rockpool-mother-of-pearl-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/lavalier-circlé-white-gold",
      "https://www.paspaley.com/jewellery/lavalier-circlé-yellow-gold",
      "https://www.paspaley.com/jewellery/lavalier-earrings-circlé-white-gold",
      "https://www.paspaley.com/jewellery/lavalier-earrings-circlé-yellow-gold",
      "https://www.paspaley.com/jewellery/lavalier-petite-circlé-white-gold",
      "https://www.paspaley.com/jewellery/lavalier-petite-circlé-yellow-gold",
      "https://www.paspaley.com/jewellery/lavalier-circlé-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-pearls-my-way-–-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-studs-–-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-chain-necklace-–-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-necklet-–-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-rope-necklace-–-rose-gold",
      "https://www.paspaley.com/pearl-collections/pavé-cherish-ring-white-gold",
      "https://www.paspaley.com/jewellery/maxima-ring-–-rose-gold",
      "https://www.paspaley.com/jewellery/maxima-snake-ring-–-rose-gold",
      "https://www.paspaley.com/pearl-collections/monsoon-budding-flower-clasp-white-gold",
      "https://www.paspaley.com/pearl-collections/monsoon-budding-flower-clasp-yellow-gold",
      "https://www.paspaley.com/jewellery/touchstone-sapphire-clasp-white-gold",
      "https://www.paspaley.com/jewellery/touchstone-sapphire-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/megisti-lapis-clasp",
      "https://www.paspaley.com/pearl-collections/strand-bracelet-circlé",
      "https://www.paspaley.com/pearl-collections/circlé-strand-bracelet",
      "https://www.paspaley.com/pearl-collections/kimberley-dark-bracelet-–-circle-pearl",
      "https://www.paspaley.com/pearl-collections/kimberley-dark-bracelet-–-double-circle-pearl",
      "https://www.paspaley.com/pearl-collections/maxima/maxima-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/maxima/maxima-clasp-white-gold",
      "https://www.paspaley.com/pearl-collections/megisti/megisti-lapis-clasp",
      "https://www.paspaley.com/pearl-collections/rockpool/rockpool-pave-clasp-white-gold",
      "https://www.paspaley.com/pearl-collections/rockpool/rockpool-pave-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/rockpool/rockpool-mother-of-pearl-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/rockpool/rockpool-mother-of-pearl-clasp-white-gold",
      "https://www.paspaley.com/catalog/product/view/id/4004/s/rhapsody-keshi-and-circle-strand-necklace/category/7/",
      "https://www.paspaley.com/catalog/product/view/id/3336/s/rhapsody-strand-bracelet/category/7/",
      "https://www.paspaley.com/catalog/product/view/id/3116/s/gold-keshi-rhapsody/category/7/",
      "https://www.paspaley.com/pearl-collections/touchstone/touchstone-sapphire-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/touchstone/touchstone-sapphire-clasp-white-gold",
      "https://www.paspaley.com/pearl-collections/touchstone/touchstone-black-diamond-clasp-white-gold",
      "https://www.paspaley.com/pearl-collections/touchstone/touchstone-diamond-clasp-yellow-gold",
      "https://www.paspaley.com/pearl-collections/touchstone/touchstone-ruby-clasp-yellow-gold"
      )



# Yes, I should have made the above into a function and done this properly, but I didn't. I'll do better next time.
for (i in 1:length(also_get)) {
  product_data <- read_html(also_get[i]) 
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/jewellery/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/pearl-collections/touchstone/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/pearl-collections/megisti/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/pearl-collections/rockpool/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/pearl-collections/maxima/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/catalog/product/view/id/4004/s/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/catalog/product/view/id/3336/s/", "")
  product_name <- str_replace_all(also_get[i], "https://www.paspaley.com/catalog/product/view/id/3116/s/", "")
  product_name <- str_replace_all(product_name, "/", "-")
  name_of_file <- paste0("inputs/2020/products/", product_name, ".html") 
  write_html(product_data, file = name_of_file)
  
  print(paste(
    "Done with file number",
    i,
    "of",
    length(also_get),
    "at",
    Sys.time()
  ))  # Helpful updating for when running it on all the records
  Sys.sleep(10)
}
