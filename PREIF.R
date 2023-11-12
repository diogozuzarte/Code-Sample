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

# Analysis of Investment in Property and REIF

# select, merge, and clean data for preif
# i) remove all rows not related to preif
preif_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '9' | report_2021q4$Asset_Sub_Category == '45',]
preif_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '9' | report_2022q1$Asset_Sub_Category == '45',]
# remove all empty rows/ columns
preif_2021q4 <- preif_2021q4[rowSums(is.na(preif_2021q4)) != ncol(preif_2021q4),]
preif_2021q4 <- preif_2021q4[,colSums(is.na(preif_2021q4)) != nrow(preif_2021q4)]
preif_2022q1 <- preif_2022q1[rowSums(is.na(preif_2022q1)) != ncol(preif_2022q1),]
preif_2022q1 <- preif_2022q1[,colSums(is.na(preif_2022q1)) != nrow(preif_2022q1)]
# iii) name rows after Asset, Fund, Entity
# sum cells that only differ in Par_amount and SII_amount
preif_2021q4 <- preif_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Unit_SII_Price, Unit_SII_Percentage) %>% summarise(Quantity = sum(Quantity), Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
preif_2022q1 <- preif_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Unit_SII_Price, Unit_SII_Percentage) %>% summarise(Quantity = sum(Quantity), Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
preif_2021q4$EntityAssetFundPortfolio <- paste(preif_2021q4$Cod_Entidade, preif_2021q4$Asset_ID_pk, preif_2021q4$Fund, preif_2021q4$Portfolio)
preif_2021q4 <- preif_2021q4[, c(18, 1:5, 15:17, 6:14)]
preif_2022q1$EntityAssetFundPortfolio <- paste(preif_2022q1$Cod_Entidade, preif_2022q1$Asset_ID_pk, preif_2022q1$Fund, preif_2022q1$Portfolio)
preif_2022q1 <- preif_2022q1[, c(18, 1:5, 15:17, 6:14)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
preif <- full_join(preif_2021q4, preif_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
preif <- preif[rowSums(is.na(preif)) != ncol(preif),]
preif <- preif[,colSums(is.na(preif)) != nrow(preif)]
# vi) substitute NA values 0
preif$Quantity.2021Q4[is.na(preif$Quantity.2021Q4)] <- 0
preif$Par_amount.2021Q4[is.na(preif$Par_amount.2021Q4)] <- 0
preif$SII_amount.2021Q4[is.na(preif$SII_amount.2021Q4)] <- 0
preif$Unit_SII_Price.2021Q4[is.na(preif$Unit_SII_Price.2021Q4)] <- 0
preif$Unit_SII_Percentage.2021Q4[is.na(preif$Unit_SII_Percentage.2021Q4)] <- 0
preif$Quantity.2022Q1[is.na(preif$Quantity.2022Q1)] <- 0
preif$Par_amount.2022Q1[is.na(preif$Par_amount.2022Q1)] <- 0
preif$SII_amount.2022Q1[is.na(preif$SII_amount.2022Q1)] <- 0
preif$Unit_SII_Price.2022Q1[is.na(preif$Unit_SII_Price.2022Q1)] <- 0
preif$Unit_SII_Percentage.2022Q1[is.na(preif$Unit_SII_Percentage.2022Q1)] <- 0




