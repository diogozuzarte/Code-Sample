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

# Analysis of Investment in Collective Investment Undertakings (except REIF)

# select, merge, and clean data for ciu
# i) remove all rows not related to ciu
ciu_2021q4 <- report_2021q4[report_2021q4$Asset_Category == '4',]
ciu_2022q1 <- report_2022q1[report_2022q1$Asset_Category == '4',]
# remove all empty rows/ columns
ciu_2021q4 <- ciu_2021q4[rowSums(is.na(ciu_2021q4)) != ncol(ciu_2021q4),]
ciu_2021q4 <- ciu_2021q4[,colSums(is.na(ciu_2021q4)) != nrow(ciu_2021q4)]
ciu_2022q1 <- ciu_2022q1[rowSums(is.na(ciu_2022q1)) != ncol(ciu_2022q1),]
ciu_2022q1 <- ciu_2022q1[,colSums(is.na(ciu_2022q1)) != nrow(ciu_2022q1)]
# iii) name rows after Asset, Fund, Entity
# sum cells that only differ in Quantity and SII_amount
ciu_2021q4 <- ciu_2021q4 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Duration, Unit_SII_Price) %>% summarise(Quantity = sum(Quantity), SII_amount = sum(SII_amount)) %>% ungroup()
ciu_2022q1 <- ciu_2022q1 %>% group_by(Cod_Entidade, Asset_ID_pk, Fund, Portfolio, UL, Title, Issuer_Sector_Code, Issuer_country, Currency, Country_Code, Asset_Category, Asset_Sub_Category, Duration, Unit_SII_Price) %>% summarise(Quantity = sum(Quantity), SII_amount = sum(SII_amount)) %>% ungroup()
# create new column that states Entity, Asset, Fund, Portfolio
ciu_2021q4$EntityAssetFundPortfolio <- paste(ciu_2021q4$Cod_Entidade, ciu_2021q4$Asset_ID_pk, ciu_2021q4$Fund, ciu_2021q4$Portfolio)
ciu_2021q4 <- ciu_2021q4[, c(17, 1:5, 15:16, 6:14)]
ciu_2022q1$EntityAssetFundPortfolio <- paste(ciu_2022q1$Cod_Entidade, ciu_2022q1$Asset_ID_pk, ciu_2022q1$Fund, ciu_2022q1$Portfolio)
ciu_2022q1 <- ciu_2022q1[, c(17, 1:5, 15:16, 6:14)]
# sum cells referring to the same Asset and Fund
# iv) merge data from the two data frames
ciu <- full_join(ciu_2021q4, ciu_2022q1, by = c("EntityAssetFundPortfolio"),  suffix = c(".2021Q4", ".2022Q1"), keep = FALSE)
# v) remove all empty rows/ columns
ciu <- ciu[rowSums(is.na(ciu)) != ncol(ciu),]
ciu <- ciu[,colSums(is.na(ciu)) != nrow(ciu)]
# vi) substitute NA values 0
ciu$Quantity.2021Q4[is.na(ciu$Quantity.2021Q4)] <- 0
ciu$SII_amount.2021Q4[is.na(ciu$SII_amount.2021Q4)] <- 0
ciu$Unit_SII_Price.2021Q4[is.na(ciu$Unit_SII_Price.2021Q4)] <- 0
ciu$Quantity.2022Q1[is.na(ciu$Quantity.2022Q1)] <- 0
ciu$SII_amount.2022Q1[is.na(ciu$SII_amount.2022Q1)] <- 0
ciu$Unit_SII_Price.2022Q1[is.na(ciu$Unit_SII_Price.2022Q1)] <- 0



