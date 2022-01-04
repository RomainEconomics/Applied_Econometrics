library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(janitor)
library(tidyverse)
library(sf)
library(usmap)
library(DBI)
library(RSQLite)

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
  group_by(Type) %>% 
  distinct(Loan_Seq_Number, .keep_all = F) %>% 
  summarise(Count = n()) %>% 
  collect() %>% 
  mutate(Type = reorder(Type, Count)) %>% 
  ggplot(aes(x = Count, y = Type, fill = Type)) +
  geom_col() +
  labs(x = '', y = '', title = 'Number of unique loans by type of financial institutions') +
  theme(legend.position = 'none') +                                   
  scale_x_continuous(labels = scales::comma)

ggsave('Graphs/num_loans_by_bank_types.png')




DB %>% 
  select(Loan_Seq_Number, Seller_Name, Lender_Type, Fintech) %>% 
  collect() %>% 
  distinct(Loan_Seq_Number, .keep_all = TRUE) %>% 
  add_count(Seller_Name) %>% 
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
  labs(x = '', y = '', title = 'Number of unique loans by Seller') +                                   
  scale_x_continuous(labels = scales::comma) +
  theme(legend.position = 'bottom') 

ggsave('Graphs/num_loans_by_seller.png', width = 14, height = 10, units = "cm")



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

plot_missing(DB)

dbListFields(con, "DB")

# Reformatting the "Zero Balance Code" variable

DB$Zero_Balance_Code[DB$Zero_Balance_Code %in% c(02, 03, 96, 09, 15)] <- 0
DB
replace_na(data, replace, ...)


DB[is.na(DB)] <- 0
DB %>% count(Zero_Balance_Code, sort = TRUE)

