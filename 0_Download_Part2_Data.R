
# install.packages('WDI')

library(tidyverse)
library(WDI)



clean_df <- function(data, name_col) {
  
  data_country_list <- data %>% 
    rename( {{ name_col }} := 3) %>%  # 3 because it is always the third column
    drop_na( {{ name_col }} ) %>% # The {{ }} are needed because of the way the tidyverse works (see : https://dplyr.tidyverse.org/articles/programming.html)
    count(country) %>% 
    filter(n == max(n)) %>% 
    distinct(country) %>% 
    pull()
  
  data %>% 
    rename( {{ name_col }} := 3) %>% # same for := (it's because of what they call 'tidyevaluation')
    filter(country %in% data_country_list) %>% 
    drop_na()
  
}

# -------------------------------------------------------------------------


# Corruption Index --------------------------------------------------------


WDIsearch('corruption') %>% tibble()

# https://databank.worldbank.org/metadataglossary/1181/series/CC.EST
corruption <- WDI(indicator='CC.EST',  start=1960, end=2020) %>% tibble()


corruption %>% 
  clean_df(name_col = 'corruption') %>% 
  write_rds('Data/Corruption_index.RDS')


# -------------------------------------------------------------------------

# GDP Per Capita ----------------------------------------------------------


WDIsearch('gdp.*capita') %>% tibble() 

# GDP Per capita (Constant 2015 US Dollar) 
# https://data.worldbank.org/indicator/NY.GDP.PCAP.KD
gdp_per_cap <- WDI(indicator='NY.GDP.PCAP.KD',  start=1990, end=2020) %>% tibble()

gdp_per_cap %>% 
  clean_df(name_col = 'gdp_per_cap') %>% 
  write_rds('Data/GDP_per_cap.RDS')



# -------------------------------------------------------------------------

# GDP / Head PPP ----------------------------------------------------------


WDIsearch('gdp.*capita') %>% tibble() 

# GDP per capita, PPP (constant 2017 international $)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp_ppp = WDI(indicator='NY.GDP.PCAP.PP.KD',  start=1990, end=2020) %>% tibble()

gdp_ppp %>% 
  clean_df(name_col = 'gdp_ppp') %>% 
  write_rds('Data/GDP_per_cap_PPP.RDS')


# -------------------------------------------------------------------------

# Population --------------------------------------------------------------

WDIsearch('total.*population') %>% tibble() 

# Population, total
# Ghttps://data.worldbank.org/indicator/SP.POP.TOTL

population <- WDI(indicator='SP.POP.TOTL',  start=1990, end=2020) %>% tibble() 


population %>% 
  clean_df(name_col = 'population') %>% 
  write_rds('Data/Population.RDS')


# -------------------------------------------------------------------------

# Foreign Aid / GDP -------------------------------------------------------


#WDIsearch('ODA') %>%  view()

# Net ODA received (% of GNI), Take GNI because the series for GDP stop in 2012
# https://data.worldbank.org/indicator/DT.ODA.ODAT.GN.ZS
oda_gni = WDI(indicator='DT.ODA.ODAT.GN.ZS',  start=1990, end=2020) %>% tibble()

oda_gni %>% 
  tibble() %>% 
  write_rds('Data/ODA_GNI.RDS')

oda_gni %>% 
  clean_df(name_col = 'oda_gni') %>% 
  write_rds('Data/ODA_GNI.RDS')




# Net ODA received per capita (current US$)
# https://data.worldbank.org/indicator/DT.ODA.ODAT.PC.ZS
oda_net = WDI(indicator='DT.ODA.ODAT.PC.ZS',  start=1990, end=2020) %>% tibble()

oda_net %>% 
  clean_df(name_col = 'oda_net') %>% 
  write_rds('Data/ODA_NET.RDS')




