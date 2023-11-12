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

# Analysis of Investment in Corporate Bonds

# select, merge, and clean data for cbonds
# i) remove all rows not related to cbonds
cbonds_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '2' | report_2021q4$Asset_Category == '5' | report_2021q4$Asset_Category == '6',]
cbonds_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '2' | report_2022q1$Asset_Category == '5' | report_2022q1$Asset_Category == '6',]
# remove all empty rows/ columns
cbonds_2021q4 <- cbonds_2021q4[rowSums(is.na(cbonds_2021q4)) != ncol(cbonds_2021q4),]
cbonds_2021q4 <- cbonds_2021q4[,colSums(is.na(cbonds_2021q4)) != nrow(cbonds_2021q4)]
cbonds_2022q1 <- cbonds_2022q1[rowSums(is.na(cbonds_2022q1)) != ncol(cbonds_2022q1),]
cbonds_2022q1 <- cbonds_2022q1[,colSums(is.na(cbonds_2022q1)) != nrow(cbonds_2022q1)]
# ii) name rows after Asset, Fund, Entity
# sum cells that only differ in Par_amount and SII_amount
cbonds_2021q4 <- cbonds_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Duration, Unit_SII_Price, Unit_SII_Percentage, Maturity_date) %>% summarise(Quantity = sum(Quantity), Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
cbonds_2022q1 <- cbonds_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Duration, Unit_SII_Price, Unit_SII_Percentage, Maturity_date) %>% summarise(Quantity = sum(Quantity), Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
cbonds_2021q4$EntityAssetFundPortfolio <- paste(cbonds_2021q4$Cod_Entidade, cbonds_2021q4$Asset_ID_pk, cbonds_2021q4$Fund, cbonds_2021q4$Portfolio)
cbonds_2021q4 <- cbonds_2021q4[, c(21, 1:5, 18:20, 6:17)]
cbonds_2022q1$EntityAssetFundPortfolio <- paste(cbonds_2022q1$Cod_Entidade, cbonds_2022q1$Asset_ID_pk, cbonds_2022q1$Fund, cbonds_2022q1$Portfolio)
cbonds_2022q1 <- cbonds_2022q1[, c(21, 1:5, 18:20, 6:17)]
# sum cells referring to the same Asset and Fund
# iii) merge data from the two data frames
cbonds <- full_join(cbonds_2021q4, cbonds_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# iv) remove all empty rows/ columns
cbonds <- cbonds[rowSums(is.na(cbonds)) != ncol(cbonds),]
cbonds <- cbonds[,colSums(is.na(cbonds)) != nrow(cbonds)]
# v) substitute NA values 0
cbonds$Quantity.2021Q4[is.na(cbonds$Quantity.2021Q4)] <- 0
cbonds$Par_amount.2021Q4[is.na(cbonds$Par_amount.2021Q4)] <- 0
cbonds$SII_amount.2021Q4[is.na(cbonds$SII_amount.2021Q4)] <- 0
cbonds$Unit_SII_Price.2021Q4[is.na(cbonds$Unit_SII_Price.2021Q4)] <- 0
cbonds$Unit_SII_Percentage.2021Q4[is.na(cbonds$Unit_SII_Percentage.2021Q4)] <- 0
cbonds$Quantity.2022Q1[is.na(cbonds$Quantity.2022Q1)] <- 0
cbonds$Par_amount.2022Q1[is.na(cbonds$Par_amount.2022Q1)] <- 0
cbonds$SII_amount.2022Q1[is.na(cbonds$SII_amount.2022Q1)] <- 0
cbonds$Unit_SII_Price.2022Q1[is.na(cbonds$Unit_SII_Price.2022Q1)] <- 0
cbonds$Unit_SII_Percentage.2022Q1[is.na(cbonds$Unit_SII_Percentage.2022Q1)] <- 0




