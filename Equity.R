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

# Analysis of Investment in Equity

# select, merge, and clean data for equity
# i) remove all rows not related to equity
equity_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '3',]
equity_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '3',]
# remove all empty rows/ columns
equity_2021q4 <- equity_2021q4[rowSums(is.na(equity_2021q4)) != ncol(equity_2021q4),]
equity_2021q4 <- equity_2021q4[,colSums(is.na(equity_2021q4)) != nrow(equity_2021q4)]
equity_2022q1 <- equity_2022q1[rowSums(is.na(equity_2022q1)) != ncol(equity_2022q1),]
equity_2022q1 <- equity_2022q1[,colSums(is.na(equity_2022q1)) != nrow(equity_2022q1)]
# iii) name rows after Asset, Fund, Entity
# sum cells that only differ in Quantity and SII_amount
equity_2021q4 <- equity_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Unit_SII_Price) %>% summarise(Quantity = sum(Quantity), SII_amount = sum(SII_amount)) %>% ungroup()
equity_2022q1 <- equity_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Unit_SII_Price) %>% summarise(Quantity = sum(Quantity), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
equity_2021q4$EntityAssetFundPortfolio <- paste(equity_2021q4$Cod_Entidade, equity_2021q4$Asset_ID_pk, equity_2021q4$Fund, equity_2021q4$Portfolio)
equity_2021q4 <- equity_2021q4[, c(16, 1:5, 14:15, 6:13)]
equity_2022q1$EntityAssetFundPortfolio <- paste(equity_2022q1$Cod_Entidade, equity_2022q1$Asset_ID_pk, equity_2022q1$Fund, equity_2022q1$Portfolio)
equity_2022q1 <- equity_2022q1[, c(16, 1:5, 14:15, 6:13)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
equity <- full_join(equity_2021q4, equity_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
equity <- equity[rowSums(is.na(equity)) != ncol(equity),]
equity <- equity[,colSums(is.na(equity)) != nrow(equity)]
# vi) substitute NA values 0
equity$Quantity.2021Q4[is.na(equity$Quantity.2021Q4)] <- 0
equity$SII_amount.2021Q4[is.na(equity$SII_amount.2021Q4)] <- 0
equity$Unit_SII_Price.2021Q4[is.na(equity$Unit_SII_Price.2021Q4)] <- 0
equity$Quantity.2022Q1[is.na(equity$Quantity.2022Q1)] <- 0
equity$SII_amount.2022Q1[is.na(equity$SII_amount.2022Q1)] <- 0
equity$Unit_SII_Price.2022Q1[is.na(equity$Unit_SII_Price.2022Q1)] <- 0
