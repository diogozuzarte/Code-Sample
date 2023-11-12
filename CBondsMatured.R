# cbonds_matured

# i) data cleaning
cbonds_matured <- cbonds[cbonds$Maturity_date.2021Q4 <= as.Date('2022-03-31') & !(is.na(cbonds$Maturity_date.2021Q4)),]
cbonds_matured$Change_Par <- cbonds_matured$Par_amount.2022Q1 - cbonds_matured$Par_amount.2021Q4
cbonds_matured$Change_Quantity <- cbonds_matured$Quantity.2022Q1 - cbonds_matured$Quantity.2021Q4
cbonds_matured$Change_SII <- cbonds_matured$SII_amount.2022Q1 - cbonds_matured$SII_amount.2021Q4
cbonds_matured$PChange_Par <- with(cbonds_matured, ifelse(cbonds_matured$Par_amount.2021Q4 != 0, cbonds_matured$Change_Par/cbonds_matured$Par_amount.2021Q4, 1))
cbonds_matured$PChange_Quantity <- with(cbonds_matured, ifelse(cbonds_matured$Quantity.2021Q4 != 0, cbonds_matured$Change_Quantity/cbonds_matured$Quantity.2021Q4, 1))
cbonds_matured$PChange_SII <- with(cbonds_matured, ifelse(cbonds_matured$SII_amount.2021Q4 != 0, cbonds_matured$Change_SII/cbonds_matured$SII_amount.2021Q4, 1))
cbonds_matured$PChange_Unit_Percentage <- with(cbonds_matured, ifelse(cbonds_matured$Unit_SII_Percentage.2021Q4 != 0, (cbonds_matured$Unit_SII_Percentage.2022Q1-cbonds_matured$Unit_SII_Percentage.2021Q4)/cbonds_matured$Unit_SII_Percentage.2021Q4, 100))

cbonds_matured$Entity <- cbonds_matured$Cod_Entidade.2021Q4
cbonds_matured$Asset <- cbonds_matured$Asset_ID_pk.2021Q4
cbonds_matured$Title <- cbonds_matured$Title.2021Q4
cbonds_matured$Portfolio <- cbonds_matured$Portfolio.2021Q4
cbonds_matured$Unit_Linked <- cbonds_matured$UL.2021Q4
cbonds_matured$Fund <- cbonds_matured$Fund.2021Q4
cbonds_matured$Sector_Code <- cbonds_matured$Issuer_Sector_Code.2021Q4
cbonds_matured$Country <- cbonds_matured$Issuer_country.2021Q4
cbonds_matured$Currency <- cbonds_matured$Currency.2021Q4
cbonds_matured$Sub_Category <- cbonds_matured$Asset_Sub_Category.2021Q4

cbonds_matured <- cbonds_matured[-c(1:6, 10:16, 22:26, 30:36)]
cbonds_matured <- cbonds_matured[, c(24:29, 1:3, 6:7, 9:11, 14:15, 17:23, 30:32, 4, 12, 5, 13, 16)]

# ii) Resume table
cbonds_matured2 <- cbonds_matured
cbonds_matured2$Non_Life <- ifelse(cbonds_matured2$Portfolio == 'Non-life [split applicable]', 1, 0)
cbonds_matured2$Life_UL <- ifelse(cbonds_matured2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
cbonds_matured2$Life_Not_UL <- ifelse(cbonds_matured2$Portfolio == 'Life [split applicable]' & cbonds_matured2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
cbonds_matured2 <- cbonds_matured2[-c(2:6, 24:26, 31)]
cbonds_matured2 <- transform(cbonds_matured2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_cbonds_matured <- cbonds_matured2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_cbonds_matured <- resume_cbonds_matured[, c(1, 22:24, 2:21)]

# iii) Country table
cbonds_matured_country <- as.data.frame(tab1(cbonds_matured$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
cbonds_matured_sector <- as.data.frame(tab1(cbonds_matured$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(cbonds_matured, "~/Desktop/ASF/R Model/cbonds_matured.xlsx")







