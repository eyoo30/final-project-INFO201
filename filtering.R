#rm(list=ls())

data <- read_csv("hfi_cc_2020.csv")

religious_vs_hf <- data %>% 
  group_by(year) %>% 
  select(hf_rank, pf_religion_restrictions)

legal_restriction_yrs <- data %>% 
  group_by(year) %>% 
  select(ef_legal)

legal_restriction_rgn <- data %>% 
  group_by(region) %>% 
  select(ef_legal)

top_money_growth %>% data %>% 
  arrange(desc(ef_money_growth)) %>% 
  select(countries) %>% 
  head(5)

top_money_growth %>% data %>% 
  arrange(desc(ef_money_growth)) %>% 
  select(countries) %>% 
  head(5)

country_vs_hf <- data %>% 
  group_by(countries) %>% 
  select(hf_rank, countries)