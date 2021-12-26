library(data.table)
library(dtplyr)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(readxl)
library(janitor) # Pour la fonction clean_names(), ca permet de supprimer les espaces et de mettre les noms de colonnes propores
library(COVID19)
library(tsibble)

path <- 'Panel\ data_Project/Databases/'

# 1 -----------------------------------------------------------------------


FREDDIE_MAC_Origination <- fread(paste0(path, "historical_data_2020Q1.txt"))

colnames(FREDDIE_MAC_Origination) <- c("Credit_score","First_Payment_Date","FstTime_HB_Flag",
                                       "Maturity_Date","MSA","Mortgage_Insurance_pct","Number_of_Units",
                                       "Occupancy_Status","O_CombinedLoanToValue","O_DebtToIncome",
                                       "O_UnpaidPrincipalBalance","O_LoanToValue","O_InterestRate",
                                       "Channel","PrepaymentPenalty_Flag","Amort_Type","Ppty_State",
                                       "Ppty_Type","Postal_Code","Loan_Seq_Number","Loan_Purpose",
                                       "Original_Loan_Term","No_of_Borrowers","Seller_Name","Servicer_Name",
                                       "Super_Conforming_Flag","V27","Program_Indicator","HARP_Indicator","V30",
                                       "InterestOnly_Indicator")


# 2 -----------------------------------------------------------------------


FREDDIE_MAC_Performance <-  fread(paste0(path, "historical_data_time_2020Q1.txt"))

colnames(FREDDIE_MAC_Performance) <- c("Loan_Seq_Number","Reporting_Period","Current_UPB","Delinquency_Status",
                                       "Loan_Age","Time_to_Maturity","Defect_Settlement_Date","Modification_Flag",
                                       "Zero_Balance_Code","Zero_Balance_EffectiveDate","Current_Interest_Rate",
                                       "Current_Deferred_UPB","DueDateOfLastPaidInstallment","MI_Recoveries",
                                       "Net_Sales_Proceeds","NonMI_Recoveries","Expenses","Legal_Costs",
                                       "Maintenance_Preservation_Costs","Taxes_and_Insurance","Misc_Expenses",
                                       "Loss_Calculation","Modification_Cost","StepModificationFlag",
                                       "Deferred_Payment_Plan","E_LoanToValue","ZeroBalance_Removal_UPB",
                                       "Delinquent_Accrued_Interest","Delinquency_Due_To_Disaster",
                                       "Borrower_Assistance_Status_Code","Current_Month_Modification_Cost",
                                       "Interest_Bearing_UPB")



# Constructing Panel DB ---------------------------------------------------


# STEP 1 - PERIMETER: removing loans issued by "small sellers" (<1% of total UPB at Q1 2020)


FREDDIE_MAC_Performance2 <- lazy_dt(FREDDIE_MAC_Performance)
FREDDIE_MAC_Origination2 <- lazy_dt(FREDDIE_MAC_Origination)

FREDDIE_MAC_Origination_Reduced <- FREDDIE_MAC_Origination2 %>% 
  filter(Seller_Name!="Other sellers")


FREDDIE_MAC_DB <- FREDDIE_MAC_Performance2 %>% 
  inner_join(FREDDIE_MAC_Origination_Reduced, by = 'Loan_Seq_Number') %>% 
  drop_na(Seller_Name) 



# STEP 2 - LENDERS: sorting lenders between commercial banks and shadow banks (incl. Fintechs)


### Lenders are classified according to whether they are a depository institution or not. 

Bank <- c("U.S. BANK N.A.","WELLS FARGO BANK, N.A.","TRUIST BANK", "CITIZENS BANK, NA","TEXAS CAPITAL BANK, N.A.", 
          "JPMORGAN CHASE BANK, NATIONAL ASSOCIATION","FLAGSTAR BANK, FSB","FIFTH THIRD BANK, NATIONAL ASSOCIATION")

ShadowBank <- c("CMG MORTGAGE, INC.","QUICKEN LOANS INC.","CALIBER HOME LOANS, INC.",
                "FAIRWAY INDEPENDENT MORTGAGE CORPORATION","NATIONSTAR MORTGAGE LLC DBA MR. COOPER",
                "PRIMELENDING A PLAINS CAPITAL CO","LOANDEPOT.COM, LLC","PROVIDENT FUNDING ASSOCIATES, L.P.",
                "UNITED SHORE FINANCIAL SERVICES, LLC","GUARANTEED RATE, INC.","FINANCE OF AMERICA MORTGAGE LLC",
                "AMERIHOME MORTGAGE COMPANY, LLC","NEWREZ LLC")

Fintech <- c("QUICKEN LOANS INC.","GUARANTEED RATE, INC.")

FREDDIE_MAC_DB <- FREDDIE_MAC_DB %>% 
  mutate(Lender_Type = if_else(Seller_Name %in% Bank, 1, 0),        # Changed to Lender_Type to make it clearer
         Fintech = if_else(Seller_Name %in% Fintech, 1, 0))



# STEP 3 - UNUSED CHARACTERISTICS: dropping unused variables


To_drop <- c("Defect_Settlement_Date","Modification_Flag","Current_Deferred_UPB","DueDateOfLastPaidInstallment",
             "Net_Sales_Proceeds","NonMI_Recoveries","Expenses","Legal_Costs","Maintenance_Preservation_Costs",
             "Taxes_and_Insurance","Misc_Expenses","Modification_Cost","StepModificationFlag","Deferred_Payment_Plan",
             "ZeroBalance_Removal_UPB","Current_Month_Modification_Cost","Zero_Balance_EffectiveDate","Delinquent_Accrued_Interest",
             "Loss_Calculation","MI_Recoveries","HARP_Indicator","V27", "V30")


DB <- FREDDIE_MAC_DB %>% 
  select(- To_drop) %>% 
  as_tibble() 



# STEP 5 - TIME-VARYING CHARACTERISTICS: adding COVID-19 and unemployment data at the zip code level

# Cleaning MSA ------------------------------------------------------------

MSA_2020 <- read_excel("Data/MSA_Large.xls", skip = 2) %>% 
  clean_names() %>% 
  mutate(Concatenated = paste0(fips_state_code, fips_county_code))


MSA_OK <- DB %>% 
  count(MSA) %>% # 405 MSA
  drop_na(MSA) %>% 
  mutate(MSA = as.character(MSA)) %>% 
  inner_join(MSA_2020, by = c('MSA' = 'cbsa_code')) %>% 
  distinct(MSA, .keep_all = T) %>% 
  mutate(cbsa_code = MSA) %>% 
  select(MSA, cbsa_code)

MSAtoChange <- DB %>% 
  count(MSA) %>% # 405 MSA
  drop_na(MSA) %>% # 404
  mutate(MSA = as.character(MSA)) %>% 
  inner_join(MSA_2020, by = c('MSA' = 'metropolitan_division_code')) %>% 
  distinct(MSA, .keep_all = T) %>% 
  select(MSA, cbsa_code)


# Adding COVID data ------------------------------------------------------------


### On peut ne garder que la premi?re variable (la variable "Confirmed"?)
COVID_data <- covid19(country = c( "US"), level = 3, start = "2020-03-01", end = "2021-11-01")


### Converting FIPS into MSA and then sum for each county the number of confirmed cases

add_COVID <- COVID_data %>% 
  tibble() %>% 
  select(id, date, key_local, confirmed) %>% 
  inner_join(MSA_2020 %>% 
               select(Concatenated, cbsa_code),
             by = c('key_local' = 'Concatenated')) %>% 
  as_tsibble( key = id, index = date) %>% # Tibble for time series, allow to calculate sum for TS data
  group_by(cbsa_code) %>% # In order to sum by MSA
  index_by(Reporting_Period = ~ yearmonth(.)) %>% # and by month
  summarise(confirmed = sum(confirmed)) %>% 
  ungroup()



# Save the DB
DB %>% 
  left_join(bind_rows(MSA_OK, MSAtoChange) %>% mutate(MSA = as.integer(MSA)), by = 'MSA') %>% 
  mutate(Reporting_Period = sub('(.{4})(.*)', '\\1-\\2', Reporting_Period),
         Reporting_Period = yearmonth(Reporting_Period)) %>% 
  left_join(add_COVID, by = c('cbsa_code', 'Reporting_Period')) %>% 
  mutate(Credit_score = na_if(Credit_score, 9999)) %>% # 9999 is for NaN
  saveRDS("Data/DB.rds")

#DB <- readRDS("DB.rds")