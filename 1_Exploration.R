library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(janitor)
library(tidyverse)
library(sf)
library(usmap)
library(DBI)


con <- dbConnect(RSQLite::SQLite(), "Data/DB.sqlite")
DB = tbl(con, "DB")



# What types of Banks ? ---------------------------------------------------

DB %>% 
  count(Lender_Type)

# 2 112 511 Banks
# 2 755 819 Shadow Banks

DB %>% 
  filter(Lender_Type == 0) %>% 
  count(Fintech)

# 715 681 FinTech Shadow banks
# 2 040 138 Non FinTech Shadow banks


DB %>% 
  mutate(Type = case_when(
           Lender_Type == 1 ~ 'Bank',
           Lender_Type == 0 & Fintech == 0 ~ 'Shadow Bank',
           Lender_Type == 0 & Fintech == 1 ~ 'Fintech'
         )) %>% 
  group_by(Type) %>% 
  summarise(Current_Interest_Rate = mean(Current_Interest_Rate),
            Credit_score = mean(Credit_score, na.rm = T))
  
DB %>% 
  mutate(Type = case_when(
    Lender_Type == 1 ~ 'Bank',
    Lender_Type == 0 & Fintech == 0 ~ 'Shadow Bank',
    Lender_Type == 0 & Fintech == 1 ~ 'Fintech'
  )) %>% 
  #mutate(Type = as.factor(Type)) %>% 
  group_by(Type) %>% 
  distinct(Loan_Seq_Number, .keep_all = F) %>% 
  summarise(Count = n()) %>% 
  collect() %>% 
  mutate(Type = reorder(Type, Count)) %>% 
  ggplot(aes(x = Count, y = Type, fill = Type)) +
  geom_col() +
  labs(x = '', y = 'Number of unique loans') +
  theme(legend.position = 'none') +                                   
  scale_x_continuous(labels = scales::comma)




DB %>% 
  add_count(Seller_Name) %>% 
  select(Seller_Name, n, Lender_Type, Fintech) %>% 
  collect() %>% 
  distinct(Seller_Name, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(Seller_Name = reorder(Seller_Name, n),
         Type = case_when(
           Lender_Type == 1 ~ 'Bank',
           Lender_Type == 0 & Fintech == 0 ~ 'Shadow Bank',
           Lender_Type == 0 & Fintech == 1 ~ 'Fintech'
         )) %>% 
  ggplot(aes(x = n, y = Seller_Name, fill = Type)) +
  geom_col() +
  labs(x = '', y = '') +                                   
  scale_x_continuous(labels = scales::comma)

# Pas de NA
DB %>% count(FstTime_HB_Flag, sort = TRUE)

# 327932 NA
DB %>% count(MSA, sort = TRUE)

DB %>% count(Lender_Type, Fintech, sort = TRUE)

DB %>% count(Loan_Purpose, sort = TRUE)

DB %>% count(Credit_score, sort = TRUE) 

DB %>% count(O_LoanToValue, sort = TRUE)

DB %>% count(Postal_Code, sort = TRUE)





DB %>% count(Loan_Purpose, sort = TRUE)

DB %>% count(Credit_score, sort = TRUE)

DB %>% count(O_LoanToValue, sort = TRUE)

DB %>% count(Postal_Code, sort = TRUE)

DB





# Data Explorer -----------------------------------------------------------

install.packages("DataExplorer")

library(DataExplorer)
create_report(DB)


