# cbonds_bought

# i) data cleaning
cbonds_bought <- cbonds[cbonds$Par_amount.2022Q1 > cbonds$Par_amount.2021Q4 | cbonds$Quantity.2022Q1 > cbonds$Quantity.2021Q4,]
cbonds_bought$Change_Par <- cbonds_bought$Par_amount.2022Q1 - cbonds_bought$Par_amount.2021Q4
cbonds_bought$Change_Quantity <- cbonds_bought$Quantity.2022Q1 - cbonds_bought$Quantity.2021Q4
cbonds_bought$Change_SII <- cbonds_bought$SII_amount.2022Q1 - cbonds_bought$SII_amount.2021Q4
cbonds_bought$PChange_Par <- with(cbonds_bought, ifelse(cbonds_bought$Par_amount.2021Q4 != 0, cbonds_bought$Change_Par/cbonds_bought$Par_amount.2021Q4, 1))
cbonds_bought$PChange_Quantity <- with(cbonds_bought, ifelse(cbonds_bought$Quantity.2021Q4 != 0, cbonds_bought$Change_Quantity/cbonds_bought$Quantity.2021Q4, 1))
cbonds_bought$PChange_SII <- with(cbonds_bought, ifelse(cbonds_bought$SII_amount.2021Q4 != 0, cbonds_bought$Change_SII/cbonds_bought$SII_amount.2021Q4, 1))
cbonds_bought$PChange_Unit_Percentage <- with(cbonds_bought, ifelse(cbonds_bought$Unit_SII_Percentage.2021Q4 != 0, (cbonds_bought$Unit_SII_Percentage.2022Q1-cbonds_bought$Unit_SII_Percentage.2021Q4)/cbonds_bought$Unit_SII_Percentage.2021Q4, 100))

cbonds_bought$Entity <- cbonds_bought$Cod_Entidade.2022Q1
cbonds_bought$Asset <- cbonds_bought$Asset_ID_pk.2022Q1
cbonds_bought$Title <- cbonds_bought$Title.2022Q1
cbonds_bought$Portfolio <- cbonds_bought$Portfolio.2022Q1
cbonds_bought$Unit_Linked <- cbonds_bought$UL.2022Q1
cbonds_bought$Fund <- cbonds_bought$Fund.2022Q1
cbonds_bought$Sector_Code <- cbonds_bought$Issuer_Sector_Code.2022Q1
cbonds_bought$Country <- cbonds_bought$Issuer_country.2022Q1
cbonds_bought$Currency <- cbonds_bought$Currency.2022Q1
cbonds_bought$Sub_Category <- cbonds_bought$Asset_Sub_Category.2022Q1

cbonds_bought <- cbonds_bought[-c(1:6, 10:16, 22:26, 30:36)]
cbonds_bought <- cbonds_bought[, c(24:29, 1:3, 6:7, 9:11, 14:15, 17:23, 30:32, 4, 12, 5, 13, 16)]

# ii) Resume table
cbonds_bought2 <- cbonds_bought
cbonds_bought2$Non_Life <- ifelse(cbonds_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
cbonds_bought2$Life_UL <- ifelse(cbonds_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
cbonds_bought2$Life_Not_UL <- ifelse(cbonds_bought2$Portfolio == 'Life [split applicable]' & cbonds_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
cbonds_bought2 <- cbonds_bought2[-c(2:6, 24:26, 31)]
cbonds_bought2 <- transform(cbonds_bought2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_cbonds_bought <- cbonds_bought2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_cbonds_bought <- resume_cbonds_bought[, c(1, 22:24, 2:21)]

# iii) Country table
cbonds_bought_country <- as.data.frame(tab1(cbonds_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
cbonds_bought_sector <- as.data.frame(tab1(cbonds_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(cbonds_bought, "~/Desktop/ASF/R Model/cbonds_bought.xlsx")







