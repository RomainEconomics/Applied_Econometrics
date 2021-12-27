



install.packages('WDI')

library(tidyverse)
library(WDI)


# -------------------------------------------------------------------------


# Corruption Index --------------------------------------------------------


WDIsearch('corruption') %>% 
  tibble()

# https://databank.worldbank.org/metadataglossary/1181/series/CC.EST
corruption = WDI(indicator='CC.EST',  start=1960, end=2020)

dat %>% 
  tibble()


dat %>% 
  tibble() %>% #view()
  filter(!is.na(CC.EST)) %>% 
  count(iso2c, sort = T) %>% 
  filter(n > 20)



# -------------------------------------------------------------------------

# GDP Per Capita ----------------------------------------------------------

WDIsearch('gdp.*capita') %>% view()
tibble() 

# GDP Per capita (Current LCU) (Local Currency Unit)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CN
gdp_per_cap = WDI(indicator='NY.GDP.PCAP.CN',  start=1990, end=2020)

gdp_per_cap %>% 
  tibble() %>% 
  drop_na() %>% 
  count(country)

# -------------------------------------------------------------------------

# GDP / Head PPP ----------------------------------------------------------


WDIsearch('gdp.*capita') %>% view()
  tibble() 

# GDP per capita, PPP (constant 2017 international $)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp_ppp = WDI(indicator='NY.GDP.PCAP.PP.KD',  start=1990, end=2020)

gdp_ppp %>% 
  tibble() %>% 
  count(country)



# -------------------------------------------------------------------------

# Population --------------------------------------------------------------

WDIsearch('total.*population') %>% view()
tibble() 

# Population, total
# Ghttps://data.worldbank.org/indicator/SP.POP.TOTL

population = WDI(indicator='SP.POP.TOTL',  start=1990, end=2020)

population %>% 
  tibble() %>% 
  drop_na() %>% 
  count(country)



# -------------------------------------------------------------------------

# Foreign Aid / GDP -------------------------------------------------------


WDIsearch('ODA') %>% view()
tibble() 

# Net ODA received (% of GNI), Take GNI because the series for GDP stop in 2012
# https://data.worldbank.org/indicator/DT.ODA.ODAT.GN.ZS
oda = WDI(indicator='DT.ODA.ODAT.GN.ZS',  start=1990, end=2020)

oda %>% 
  tibble() %>% 
  drop_na() %>% 
  count(country)



