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

# Analysis of Investment in Government Bonds

# select, merge, and clean data for govtbonds
# i) remove all rows not related to govtbonds
govtbonds_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '1',]
govtbonds_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '1',]
# remove all empty rows/ columns
govtbonds_2021q4 <- govtbonds_2021q4[rowSums(is.na(govtbonds_2021q4)) != ncol(govtbonds_2021q4),]
govtbonds_2021q4 <- govtbonds_2021q4[,colSums(is.na(govtbonds_2021q4)) != nrow(govtbonds_2021q4)]
govtbonds_2022q1 <- govtbonds_2022q1[rowSums(is.na(govtbonds_2022q1)) != ncol(govtbonds_2022q1),]
govtbonds_2022q1 <- govtbonds_2022q1[,colSums(is.na(govtbonds_2022q1)) != nrow(govtbonds_2022q1)]
# ii) EIOPA Criteria
# a) remove rows for which Par_amount > SII_amount * 2
govtbonds_2021q4 <- govtbonds_2021q4[!(govtbonds_2021q4$Par_amount > (govtbonds_2021q4$SII_amount * 2)),]
govtbonds_2022q1 <- govtbonds_2022q1[!(govtbonds_2022q1$Par_amount > (govtbonds_2022q1$SII_amount * 2)),]
# iii) name rows after Asset, Fund, Entity
# sum cells that only differ in Par_amount and SII_amount
govtbonds_2021q4 <- govtbonds_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Duration, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
govtbonds_2022q1 <- govtbonds_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Duration, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
govtbonds_2021q4$EntityAssetFundPortfolio <- paste(govtbonds_2021q4$Cod_Entidade, govtbonds_2021q4$Asset_ID_pk, govtbonds_2021q4$Fund, govtbonds_2021q4$Portfolio)
govtbonds_2021q4 <- govtbonds_2021q4[, c(19, 1:5, 17:18, 6:16)]
govtbonds_2022q1$EntityAssetFundPortfolio <- paste(govtbonds_2022q1$Cod_Entidade, govtbonds_2022q1$Asset_ID_pk, govtbonds_2022q1$Fund, govtbonds_2022q1$Portfolio)
govtbonds_2022q1 <- govtbonds_2022q1[, c(19, 1:5, 17:18, 6:16)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
govtbonds <- full_join(govtbonds_2021q4, govtbonds_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
govtbonds <- govtbonds[rowSums(is.na(govtbonds)) != ncol(govtbonds),]
govtbonds <- govtbonds[,colSums(is.na(govtbonds)) != nrow(govtbonds)]
# vi) substitute NA values 0
govtbonds$Par_amount.2021Q4[is.na(govtbonds$Par_amount.2021Q4)] <- 0
govtbonds$SII_amount.2021Q4[is.na(govtbonds$SII_amount.2021Q4)] <- 0
govtbonds$Unit_SII_Percentage.2021Q4[is.na(govtbonds$Unit_SII_Percentage.2021Q4)] <- 0
govtbonds$Par_amount.2022Q1[is.na(govtbonds$Par_amount.2022Q1)] <- 0
govtbonds$SII_amount.2022Q1[is.na(govtbonds$SII_amount.2022Q1)] <- 0
govtbonds$Unit_SII_Percentage.2022Q1[is.na(govtbonds$Unit_SII_Percentage.2022Q1)] <- 0




