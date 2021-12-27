library(tidyverse)

# Read in the data
the_data <- read_csv("outputs/data/cleaned_dataset.csv")


# Number per year
the_data %>% group_by(year) %>% count()

the_data %>% 
  group_by(year, collection) %>% 
  count() %>% 
  spread(year, n)

product_in_all_years <- 
  the_data %>% 
  pivot_wider(id_cols = c("year", "sku"),
              names_from = year, 
              values_from = price) %>% 
  rename(price_in_2017 = `2017`, 
         price_in_2018 = `2018`,
         price_in_2019 = `2019`,
         price_in_2020 = `2020`,
         price_in_2021 = `2021`) %>% 
  filter(!is.na(price_in_2021),
    !is.na(price_in_2020),
         !is.na(price_in_2019),
         !is.na(price_in_2018),
         !is.na(price_in_2017)) %>% 
  left_join(the_data[the_data$year == 2021,]) %>% 
  select(-price, 
         -year,
         -name_for_matching) %>% 
  ungroup()

product_in_all_years %>% 
  count(category) 

product_in_all_years %>% 
  count(collection) 

product_in_all_years$change <- product_in_all_years$price_in_2021/product_in_all_years$price_in_2017



# Try some plots
ggplot(data = the_data, aes(x = price)) +
  geom_histogram(binwidth = 1000) +
  facet_wrap(vars(year), nrow = 2) +
  theme_classic()

ggplot(data = the_data, aes(x = log(price))) +
  geom_histogram() +
  facet_wrap(vars(year), nrow = 2) +
  theme_classic()


ggplot(data = the_data, aes(x = price, y = year, colour = collection)) +
  geom_point()

ggplot(data = the_data, aes(x = price)) +
  geom_density(aes(fill=factor(collection)), alpha=0.8)

ggplot(data = the_data, aes(x = log(price))) +
  geom_density(aes(fill=factor(year)), alpha=0.8) +
  facet_wrap(vars(collection), nrow = 10, scales = "free") +
  scale_fill_brewer(palette = "Set1") 

ggplot(data = the_data, aes(collection, log(price))) +
  geom_boxplot() +
  facet_wrap(vars(year), nrow = 2)

# Try some summary statistics
# Counts and average price by collection by year
the_data %>% 
  group_by(year, collection) %>% 
  summarise(average = mean(price) %>% as.integer(),
            maximum = max(price) %>% as.integer(),
            minimum = min(price) %>% as.integer(),
            number = n()
            ) %>% 
  filter(collection %in% c("Dive", "Kimberley", "Lavalier", "Maxima", "Monsoon", "Rockpool", "Touchstone")) 

the_data %>% 
  group_by(year, category) %>% 
  summarise(average = mean(price) %>% as.integer(),
            maximum = max(price) %>% as.integer(),
            minimum = min(price) %>% as.integer(),
            number = n())

# Try comparing the prices for the products that are the same
price_comparison <- the_data %>% 
  select(sku, collection , category, year, price) %>% 
  group_by(sku) %>% 
  spread(key = year, value = price) %>% 
  rename(price_in_2017 = `2017`, price_in_2018 = `2018`)

price_comparison <- price_comparison[complete.cases(price_comparison), ] %>% 
  mutate(percent_change = ((price_in_2018 - price_in_2017) / price_in_2017) * 100,
         percent_change = round(percent_change, digits = 2))

the_2018_names <- the_data %>% 
  filter(year == 2018) %>% 
  select(product, sku)

price_comparison <- price_comparison %>% 
  left_join(the_2018_names, by = "sku")

rm(the_2018_names)

price_comparison %>% 
  arrange(percent_change) %>% 
  ggplot(aes(y = product, x = percent_change)) +
  geom_point()

write_csv(price_comparison, "outputs/data/price_comparison.csv")

price_comparison %>% 
  arrange(percent_change) %>% 
  ggplot(aes(y = 1, x = percent_change)) +
  geom_jitter() +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



# Try comparing the prices by category, by collection
summary <- the_data %>% 
  group_by(year, category, collection) %>% 
  summarise(average = mean(price) %>% as.integer(),
            maximum = max(price) %>% as.integer(),
            minimum = min(price) %>% as.integer(),
            number = n())



install.packages("brms")
library(brms)

fit1 <- brm(formula = price ~ year + category + collection,
            data = the_data, 
            family = lognormal(),
            # prior = c(set_prior("normal(0,5)", class = "b"),
            #           set_prior("cauchy(0,2)", class = "sd"),
            #           set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))

install.packages("tidybayes")
library(tidybayes)
get_variables(fit1)

plot(fit1)

the_data %>%
  modelr::data_grid(condition) %>%
  add_fitted_draws(fit1) %>%
  ggplot(aes(x = .value, y = condition)) +
  stat_pointintervalh(.width = c(.66, .95))

the_data %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointrangeh()

fit1 %>%
  spread_draws(b_Intercept, b_year, b_categoryBracelets, b_categoryClasps, b_categoryEarrings, b_categoryNecklacesAndPendants, b_categoryOther, b_categoryRings, b_categoryStrands, b_collectionLavalier, b_collectionMaxima, b_collectionMonsoon, b_collectionOther, b_collectionPearlsMyWay, b_collectionRockpool, b_collectionStrand, b_collectionTouchstone) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  geom_halfeyeh() 

stanplot(fit1, type = "hist")