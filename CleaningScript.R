# ----------------------------------------------------------------------------------- #
#     
#     Departamento de Análise de Riscos e Solvência  
#     ASF - Autoridade de Supervisão de Seguros e Fundos de Pensões
#
#     Diogo Salvador Zuzarte
#     Julho 2022
#
#     Código com o objetivo de identificar os ativos que são comprados
#     e vendidos entre períodos de análise e de fazer a sua respetiva caracterização.
#     
#------------------------------------------------------------------------------------ #


# 1.Sample creation and data cleaning

# Screen cleaning
rm(list=ls(all=TRUE))
graphics.off()
close.screen(all = TRUE)
erase.screen()

# Set working directory
setwd("~/Documents/ASF/R Model")

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(writexl)
library(stringr)
library(pastecs)
library(stargazer)
library(car)
library(lmtest)
library(skedastic)
library(sandwich)
library(epiDisplay)
library(formattable)
library(waterfalls)

# Import data
report_2021q4 <- read_excel("report_2021q4.xlsx", sheet = "S.06.02")
report_2022q1 <- read_excel("report_2022q1.xlsx", sheet = "S.06.02")

# Clean data

# a) rows that are not used (shareholders' funds)
report_2021q4 <- report_2021q4[!(report_2021q4$Portfolio == "Shareholders' funds"),]
# b) columns that are not used
report_2021q4 <- report_2021q4[, -which(names(report_2021q4) %in% c("Data", "Line_pk", "Match_portfolio", "Collateral", "Country_custody", "Custodian", "Writes", "Val_method", "Acquis_value", "Accr_interest", "Deposits_art162", "Confirmation_no_setoff", "Issuer", "Issuer_ID", "Issuer_Sector_ESA", "Issuer_Group", "Issuer_Group_ID", "Country_residence", "SCR_CIU", "Instrument_ESA", "Infrastructure", "Holdings", "Rating_External", "ECAI", "Rating_Internal", "Issue_date"))]
# c) EIOPA criteria
# rows for which SII amount is not reported
report_2021q4 <- report_2021q4[!(is.na(report_2021q4$SII_amount)),]
# control for investment value lost
original_sum1 <- sum(report_2021q4$SII_amount)
# i) rows for which SII values and quantity/ par amount are not reported
report_2021q4 <- report_2021q4[!(is.na(report_2021q4$Quantity) & is.na(report_2021q4$Par_amount)),]
report_2021q4 <- report_2021q4[!(is.na(report_2021q4$Unit_SII_Price) & is.na(report_2021q4$Unit_SII_Percentage)),]
# ii) rows for which SII amount is negative (error)
report_2021q4 <- report_2021q4[!(report_2021q4$SII_amount<0),]

# a) rows that are not used (shareholders' funds)
report_2022q1 <- report_2022q1[!(report_2022q1$Portfolio == "Shareholders' funds"),]
# b) columns that are not used
report_2022q1 <- report_2022q1[, -which(names(report_2022q1) %in% c("Data", "Line_pk", "Match_portfolio", "Collateral", "Country_custody", "Custodian", "Writes", "Val_method", "Acquis_value", "Accr_interest", "Deposits_art162", "Confirmation_no_setoff", "Issuer", "Issuer_ID", "Issuer_Sector_ESA", "Issuer_Group", "Issuer_Group_ID", "Country_residence", "SCR_CIU", "Instrument_ESA", "Infrastructure", "Holdings", "Rating_External", "ECAI", "Rating_Internal", "Issue_date"))]
# c) EIOPA criteria
# rows for which SII amount is not reported
report_2022q1 <- report_2022q1[!(is.na(report_2022q1$SII_amount)),]
# control for investment value lost
original_sum2 <- sum(report_2022q1$SII_amount)
# i) rows for which SII values and quantity/ par amount are not reported 
report_2022q1 <- report_2022q1[!(is.na(report_2022q1$Quantity) & is.na(report_2022q1$Par_amount)),]
report_2022q1 <- report_2022q1[!(is.na(report_2022q1$Unit_SII_Price) & is.na(report_2022q1$Unit_SII_Percentage)),]
# ii) rows for which SII amount is negative (error)
report_2022q1 <- report_2022q1[!(report_2022q1$SII_amount<0),]

# control for investment value lost
final_sum1 <- sum(report_2021q4$SII_amount)
final_sum2 <- sum(report_2022q1$SII_amount)
percentage_original_investment1 <- (final_sum1 * 100) / original_sum1
percentage_original_investment2 <- (final_sum2 * 100) / original_sum2

# d) separate CIC codes
report_2021q4[c('Country_Code_0', 'Asset_Sub_Category')] <- str_split_fixed(report_2021q4$CIC, '', 2)
report_2021q4[c('Country_Code_1', 'Asset_Sub_Category')] <- str_split_fixed(report_2021q4$Asset_Sub_Category, '', 2)
report_2021q4[c('Asset_Category', 'Asset_Sub_Category_0')] <- str_split_fixed(report_2021q4$Asset_Sub_Category, '', 2)
report_2021q4$Country_Code <- gsub(" ", "", paste(report_2021q4$Country_Code_0,report_2021q4$Country_Code_1))
report_2021q4 <- report_2021q4[-c(13, 19, 21, 23)]
report_2021q4 <- report_2021q4[, c(1:12, 20, 19, 18, 13:17)]
report_2022q1[c('Country_Code_0', 'Asset_Sub_Category')] <- str_split_fixed(report_2022q1$CIC, '', 2)
report_2022q1[c('Country_Code_1', 'Asset_Sub_Category')] <- str_split_fixed(report_2022q1$Asset_Sub_Category, '', 2)
report_2022q1[c('Asset_Category', 'Asset_Sub_Category_0')] <- str_split_fixed(report_2022q1$Asset_Sub_Category, '', 2)
report_2022q1$Country_Code <- gsub(" ", "", paste(report_2022q1$Country_Code_0,report_2022q1$Country_Code_1))
report_2022q1 <- report_2022q1[-c(13, 19, 21, 23)]
report_2022q1 <- report_2022q1[, c(1:12, 20, 19, 18, 13:17)]

# e) separate Issuer_Sector
report_2021q4[c('Issuer_Sector_Code', 'Issuer_Sector_Description')] <- str_split_fixed(report_2021q4$Issuer_Sector, ' ', 2)
report_2021q4 <- report_2021q4[-c(10, 22)]
report_2021q4$Issuer_Sector_K <- ifelse(substr(report_2021q4$Issuer_Sector_Code, 1, 1) == 'K', report_2021q4$Issuer_Sector_Code, '')
report_2021q4$Issuer_Sector_Code <- ifelse(substr(report_2021q4$Issuer_Sector_Code, 1, 1) == 'K', '', report_2021q4$Issuer_Sector_Code)
report_2021q4$Issuer_Sector_Code <- ifelse(substr(report_2021q4$Issuer_Sector_Code, 3, 3) == '.', substr(report_2021q4$Issuer_Sector_Code, 1, 2), substr(report_2021q4$Issuer_Sector_Code, 1, 3))
report_2021q4$Issuer_Sector_Code <- paste(report_2021q4$Issuer_Sector_Code, report_2021q4$Issuer_Sector_K, sep = '')
report_2021q4 <- report_2021q4[-c(21)]
report_2021q4 <- report_2021q4[, c(1:9, 20, 10:19)]
report_2022q1[c('Issuer_Sector_Code', 'Issuer_Sector_Description')] <- str_split_fixed(report_2022q1$Issuer_Sector, ' ', 2)
report_2022q1 <- report_2022q1[-c(10, 22)]
report_2022q1$Issuer_Sector_K <- ifelse(substr(report_2022q1$Issuer_Sector_Code, 1, 1) == 'K', report_2022q1$Issuer_Sector_Code, '')
report_2022q1$Issuer_Sector_Code <- ifelse(substr(report_2022q1$Issuer_Sector_Code, 1, 1) == 'K', '', report_2022q1$Issuer_Sector_Code)
report_2022q1$Issuer_Sector_Code <- ifelse(substr(report_2022q1$Issuer_Sector_Code, 3, 3) == '.', substr(report_2022q1$Issuer_Sector_Code, 1, 2), substr(report_2022q1$Issuer_Sector_Code, 1, 3))
report_2022q1$Issuer_Sector_Code <- paste(report_2022q1$Issuer_Sector_Code, report_2022q1$Issuer_Sector_K, sep = '')
report_2022q1 <- report_2022q1[-c(21)]
report_2022q1 <- report_2022q1[, c(1:9, 20, 10:19)]

# f) separate Credit Quality Step
report_2021q4[c('1', '2', '3', 'CQS')] <- str_split_fixed(report_2021q4$CQS, ' ', 4)
report_2021q4 <- report_2021q4[-c(21:23)]
report_2022q1[c('1', '2', '3', 'CQS')] <- str_split_fixed(report_2022q1$CQS, ' ', 4)
report_2022q1 <- report_2022q1[-c(21:23)]
report_2021q4$CQS <- na_if(report_2021q4$CQS, '')
report_2022q1$CQS <- na_if(report_2022q1$CQS, '')

# g) convert durations of 0 into NA
report_2021q4$Duration <- na_if(report_2021q4$Duration, 0)
report_2022q1$Duration <- na_if(report_2022q1$Duration, 0)

# Output: Two dataframes with columns (in this order): Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Quantity, Par_amount, SII_amount, Title, Issuer_Sector_code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Duration, Unit_SII_Price, Unit_SII_Percentage, Maturity_date






