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

# Analysis of Investment in Deposits

# select, merge, and clean data for deposits
# i) remove all rows not related to deposits
deposits_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '7',]
deposits_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '7',]
# remove all empty rows/ columns
deposits_2021q4 <- deposits_2021q4[rowSums(is.na(deposits_2021q4)) != ncol(deposits_2021q4),]
deposits_2021q4 <- deposits_2021q4[,colSums(is.na(deposits_2021q4)) != nrow(deposits_2021q4)]
deposits_2022q1 <- deposits_2022q1[rowSums(is.na(deposits_2022q1)) != ncol(deposits_2022q1),]
deposits_2022q1 <- deposits_2022q1[,colSums(is.na(deposits_2022q1)) != nrow(deposits_2022q1)]
# ii) name rows after Asset, Fund, Entity
# sum cells that only differ in Par_amount and SII_amount
deposits_2021q4 <- deposits_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
deposits_2022q1 <- deposits_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
deposits_2021q4$EntityAssetFundPortfolio <- paste(deposits_2021q4$Cod_Entidade, deposits_2021q4$Asset_ID_pk, deposits_2021q4$Fund, deposits_2021q4$Portfolio)
deposits_2021q4 <- deposits_2021q4[, c(18, 1:5, 16:17, 6:15)]
deposits_2022q1$EntityAssetFundPortfolio <- paste(deposits_2022q1$Cod_Entidade, deposits_2022q1$Asset_ID_pk, deposits_2022q1$Fund, deposits_2022q1$Portfolio)
deposits_2022q1 <- deposits_2022q1[, c(18, 1:5, 16:17, 6:15)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
deposits <- full_join(deposits_2021q4, deposits_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
deposits <- deposits[rowSums(is.na(deposits)) != ncol(deposits),]
deposits <- deposits[,colSums(is.na(deposits)) != nrow(deposits)]
# vi) substitute NA values 0
deposits$Par_amount.2021Q4[is.na(deposits$Par_amount.2021Q4)] <- 0
deposits$SII_amount.2021Q4[is.na(deposits$SII_amount.2021Q4)] <- 0
deposits$Unit_SII_Percentage.2021Q4[is.na(deposits$Unit_SII_Percentage.2021Q4)] <- 0
deposits$Par_amount.2022Q1[is.na(deposits$Par_amount.2022Q1)] <- 0
deposits$SII_amount.2022Q1[is.na(deposits$SII_amount.2022Q1)] <- 0
deposits$Unit_SII_Percentage.2022Q1[is.na(deposits$Unit_SII_Percentage.2022Q1)] <- 0



