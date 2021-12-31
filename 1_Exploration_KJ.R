library(data.table)
library(dtplyr)
library(tidyverse)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(janitor) # Pour la fonction clean_names(), ca permet de supprimer les espaces et de mettre les noms de colonnes propores
library(COVID19)
library(tsibble)
library(DBI) # To connect to the database
library(plm)
library(DataExplorer)

# Reformatting the "Zero Balance Code" variable: 1 if prepaid, 0 otherwise

Final_DB$Zero_Balance_Code[Final_DB$Zero_Balance_Code %in% c(02, 03, 96, 09, 15)] <- 0
replace_na(Final_DB$Zero_Balance_Code,0)

Final_DB %>% count(Zero_Balance_Code, sort = TRUE)

Zero_Balance_Code <- as.numeric(Final_DB$Zero_Balance_Code)

# Reformatting the "Delinquency status" variable: counting "REO acquisition as "NA"

Final_DB$Delinquency_Status[Final_DB$Delinquency_Status=="RA"] <- ""

Final_DB %>% count(Delinquency_Status, sort = TRUE)

Delinquency_Status <- as.numeric(Final_DB$Delinquency_Status)

# Reformatting the "Estimated Loan-to-Value" variable: counting "999" as "NA"

Final_DB$E_LoanToValue[Final_DB$E_LoanToValue==999] <- ""

E_LoanToValue <- as.numeric(Final_DB$E_LoanToValue)

# Reformatting the "Original Debt-to-Income" variable: counting "999" as "NA"

Final_DB$O_DebtToIncome[Final_DB$O_DebtToIncome==999] <- ""

O_Debt_to_Income <- as.numeric(Final_DB$O_DebtToIncome)

#Formatting the remaining variables of interest as numeric variables

COVID <- as.numeric(Final_DB$confirmed)
Current_Interest_Rate <- as.numeric(Final_DB$Current_Interest_Rate)
Current_UPB <- as.numeric(Final_DB$Current_UPB)
Credit_score <- as.numeric(Final_DB$Credit_score)
Time_to_Maturity <- as.numeric(Final_DB$Time_to_Maturity)

#Creating a finalized working data frame with only numeric variables

DB_KJ <- data.frame(Final_DB$Loan_Seq_Number, Final_DB$Reporting_Period, Delinquency_Status,
                    Zero_Balance_Code,COVID,Credit_score,Current_Interest_Rate,Current_UPB,
                    E_LoanToValue, O_Debt_to_Income, Time_to_Maturity, Final_DB$Lender_Type,
                    Final_DB$Fintech, Final_DB$MSA, Final_DB$Seller_Name)

saveRDS(DB_KJ,"DB_KJ.rds")

# Checking presence of NAs in the whole Final_DB

NAindex <- is.na(Final_DB)
Final_DB[NAindex] <- ""

#Save database as RDS (KJ)

saveRDS(Final_DB,"Final_DB.rds")

# Data Explorer

create_report(Final_DB)

plot_missing(Final_DB)

# Identifying which loans are not related to an MSA (22,351 / 346,724)

Loans_by_MSA <- 
  Final_DB %>%                             
  group_by(MSA) %>%
  summarise(LoansPerMSA = n_distinct(Loan_Seq_Number))

sum(Loans_by_MSA$LoansPerMSA)

# Grouping loans by number of consecutive observations (only 0.3% have 1 period)

Final_DB %>%
  group_by(Loan_Seq_Number) %>%
  tally() %>% 
  count(n) %>%
  summarise(Number_Periods_available = n,
            Number_of_loans = nn)

# Checking panel data structure

Final_DB %>% 
  is.pbalanced()

# Computing within and between transformed variables for all variables

Panel_DB_KJ <- pdata.frame(DB_KJ)

#Between variables

Delinquency_Status <- Panel_DB_KJ$Delinquency_Status
Current_Interest_Rate <- Panel_DB_KJ$Current_Interest_Rate
E_LoanToValue <- Panel_DB_KJ$E_LoanToValue
Credit_score <- Panel_DB_KJ$Credit_score
O_Debt_to_Income <- Panel_DB_KJ$O_Debt_to_Income
Current_UPB <- Panel_DB_KJ$Current_UPB
Time_to_Maturity <- Panel_DB_KJ$Time_to_Maturity
COVID <- Panel_DB_KJ$COVID

as.matrix(Delinquency_Status)
as.matrix(Current_Interest_Rate)
as.matrix(E_LoanToValue)
as.matrix(Credit_score)
as.matrix(O_Debt_to_Income)
as.matrix(Current_UPB)
as.matrix(Time_to_Maturity)
as.matrix(COVID)

between(Delinquency_Status, effect = c("individual", "time", "group"))
between(Current_Interest_Rate, effect = c("individual", "time", "group"))
between(E_LoanToValue, effect = c("individual", "time", "group"))
between(Credit_score, effect = c("individual", "time", "group"))
between(O_Debt_to_Income, effect = c("individual", "time", "group"))
between(Current_UPB, effect = c("individual", "time", "group"))
between(Time_to_Maturity, effect = c("individual", "time", "group"))
between(COVID, effect = c("individual", "time", "group"))

Between(Delinquency_Status, effect = c("individual", "time", "group"))
Between(Current_Interest_Rate, effect = c("individual", "time", "group"))
Between(E_LoanToValue, effect = c("individual", "time", "group"))
Between(Credit_score, effect = c("individual", "time", "group"))
Between(O_Debt_to_Income, effect = c("individual", "time", "group"))
Between(Current_UPB, effect = c("individual", "time", "group"))
Between(Time_to_Maturity, effect = c("individual", "time", "group"))
Between(COVID, effect = c("individual", "time", "group"))

Within(Delinquency_Status,effect = c("individual", "time", "group"))
Within(Current_Interest_Rate,effect = c("individual", "time", "group"))
Within(E_LoanToValue,effect = c("individual", "time", "group"))
Within(Credit_score,effect = c("individual", "time", "group"))
Within(O_Debt_to_Income,effect = c("individual", "time", "group"))
Within(Current_UPB,effect = c("individual", "time", "group"))
Within(Time_to_Maturity,effect = c("individual", "time", "group"))
Within(COVID,effect = c("individual", "time", "group"))

Sum(Delinquency_Status,effect = c("individual", "time", "group"))
Sum(Current_Interest_Rate,effect = c("individual", "time", "group"))
Sum(E_LoanToValue,effect = c("individual", "time", "group"))
Sum(Credit_score,effect = c("individual", "time", "group"))
Sum(O_Debt_to_Income,effect = c("individual", "time", "group"))
Sum(Current_UPB,effect = c("individual", "time", "group"))
Sum(Time_to_Maturity,effect = c("individual", "time", "group"))
Sum(COVID,effect = c("individual", "time", "group"))

Variance <- Panel_DB_KJ %>%
  dplyr::select(Final_DB.Loan_Seq_Number, Final_DB.Reporting_Period, Delinquency_Status,
                Current_Interest_Rate, E_LoanToValue, Credit_score, O_Debt_to_Income,
                Current_UPB, Time_to_Maturity, COVID) %>%
  gather(variable, value, -Final_DB.Loan_Seq_Number, -Final_DB.Reporting_Period) %>%
  group_by(variable) %>%
  summarize(var = var(value))

Within.Variance <-c('W.Delinquency', 'W.Interest', 'W.ELTV', 'W.Score', 'W.ODTI',
                    'W.CUPB', 'W.TTM', 'W.COVID')
Between.Variance <- c('M.Delinquency', 'M.Interest', 'M.ELTV', 'M.Score', 'M.ODTI',
                      'M.CUPB','M.TTM','M.COVID')

Within.Variance.values  <- Panel_DB_KJ %>%
  dplyr::select(Final_DB.Loan_Seq_Number, Final_DB.Reporting_Period, Within.Variance) %>%
  gather(variable, value, -Final_DB.Loan_Seq_Number, -Final_DB.Reporting_Period) %>%
  group_by(variable) %>%
  summarize(var = var(value))

Between.Variance.values  <- Panel_DB_KJ %>%
  dplyr::select(Final_DB.Loan_Seq_Number, Final_DB.Reporting_Period, Between.Variance) %>%
  gather(variable, value, -Final_DB.Loan_Seq_Number, -Final_DB.Reporting_Period) %>%
  group_by(variable) %>%
  summarize(var = var(value))

Three.variances <- Variance %>% bind_cols(Within.Variance.values, Between.Variance.values) %>%
  dplyr::select(-variable1, -variable2) %>%
  rename(Variable = variable, Overall = var, Within = var1, Between = var2) %>%
  mutate(`Within Variance Share` = (Within/Overall)*100,
         `Between Variance Share` = (Between/Overall)*100) %>%
  na.omit()
Three.variances


