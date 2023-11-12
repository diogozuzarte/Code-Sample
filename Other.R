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

# Analysis of Investment in Other

# select, merge, and clean data for other
# i) remove all rows not related to other
other_2021q4 <- report_2021q4[report_2021q4$Asset_Category == 'A' | report_2021q4$Asset_Category == 'B' | report_2021q4$Asset_Category == 'C' | report_2021q4$Asset_Category == 'D' | report_2021q4$Asset_Category == 'E' | report_2021q4$Asset_Category == 'F' | report_2021q4$Asset_Category == '8',]
other_2022q1 <- report_2022q1[report_2022q1$Asset_Category == 'A' | report_2022q1$Asset_Category == 'B' | report_2022q1$Asset_Category == 'C' | report_2022q1$Asset_Category == 'D' | report_2022q1$Asset_Category == 'E' | report_2022q1$Asset_Category == 'F' | report_2022q1$Asset_Category == '8',]
# remove all empty rows/ columns
other_2021q4 <- other_2021q4[rowSums(is.na(other_2021q4)) != ncol(other_2021q4),]
other_2021q4 <- other_2021q4[,colSums(is.na(other_2021q4)) != nrow(other_2021q4)]
other_2022q1 <- other_2022q1[rowSums(is.na(other_2022q1)) != ncol(other_2022q1),]
other_2022q1 <- other_2022q1[,colSums(is.na(other_2022q1)) != nrow(other_2022q1)]
# ii) name rows after Asset, Fund, Entity
# sum cells that only differ in Par_amount and SII_amount
other_2021q4 <- other_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
other_2022q1 <- other_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, CQS, Unit_SII_Percentage, Maturity_date) %>% summarise(Par_amount = sum(Par_amount), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
other_2021q4$EntityAssetFundPortfolio <- paste(other_2021q4$Cod_Entidade, other_2021q4$Asset_ID_pk, other_2021q4$Fund, other_2021q4$Portfolio)
other_2021q4 <- other_2021q4[, c(18, 1:5, 16:17, 6:15)]
other_2022q1$EntityAssetFundPortfolio <- paste(other_2022q1$Cod_Entidade, other_2022q1$Asset_ID_pk, other_2022q1$Fund, other_2022q1$Portfolio)
other_2022q1 <- other_2022q1[, c(18, 1:5, 16:17, 6:15)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
other <- full_join(other_2021q4, other_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
other <- other[rowSums(is.na(other)) != ncol(other),]
other <- other[,colSums(is.na(other)) != nrow(other)]
# vi) substitute NA values 0
other$Par_amount.2021Q4[is.na(other$Par_amount.2021Q4)] <- 0
other$SII_amount.2021Q4[is.na(other$SII_amount.2021Q4)] <- 0
other$Unit_SII_Percentage.2021Q4[is.na(other$Unit_SII_Percentage.2021Q4)] <- 0
other$Par_amount.2022Q1[is.na(other$Par_amount.2022Q1)] <- 0
other$SII_amount.2022Q1[is.na(other$SII_amount.2022Q1)] <- 0
other$Unit_SII_Percentage.2022Q1[is.na(other$Unit_SII_Percentage.2022Q1)] <- 0



