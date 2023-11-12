# cbonds_sold

# i) data cleaning
cbonds$Maturity_date.2021Q4 <- format(cbonds$Maturity_date.2021Q4, "%Y-%m-%d")
cbonds_sold <- cbonds[(cbonds$Par_amount.2022Q1 < cbonds$Par_amount.2021Q4 | cbonds$Quantity.2022Q1 < cbonds$Quantity.2021Q4) & cbonds$Maturity_date.2021Q4 > as.Date('2022-03-31'),]
cbonds_sold$Change_Par <- cbonds_sold$Par_amount.2022Q1 - cbonds_sold$Par_amount.2021Q4
cbonds_sold$Change_Quantity <- cbonds_sold$Quantity.2022Q1 - cbonds_sold$Quantity.2021Q4
cbonds_sold$Change_SII <- cbonds_sold$SII_amount.2022Q1 - cbonds_sold$SII_amount.2021Q4
cbonds_sold$PChange_Par <- with(cbonds_sold, ifelse(cbonds_sold$Par_amount.2021Q4 != 0, cbonds_sold$Change_Par/cbonds_sold$Par_amount.2021Q4, -1))
cbonds_sold$PChange_Quantity <- with(cbonds_sold, ifelse(cbonds_sold$Quantity.2021Q4 != 0, cbonds_sold$Change_Quantity/cbonds_sold$Quantity.2021Q4, 1))
cbonds_sold$PChange_SII <- with(cbonds_sold, ifelse(cbonds_sold$SII_amount.2021Q4 != 0, cbonds_sold$Change_SII/cbonds_sold$SII_amount.2021Q4, -1))
cbonds_sold$PChange_Unit_Percentage <- with(cbonds_sold, ifelse(cbonds_sold$Unit_SII_Percentage.2021Q4 != 0, (cbonds_sold$Unit_SII_Percentage.2022Q1-cbonds_sold$Unit_SII_Percentage.2021Q4)/cbonds_sold$Unit_SII_Percentage.2021Q4, -100))

cbonds_sold$Entity <- cbonds_sold$Cod_Entidade.2021Q4
cbonds_sold$Asset <- cbonds_sold$Asset_ID_pk.2021Q4
cbonds_sold$Title <- cbonds_sold$Title.2021Q4
cbonds_sold$Portfolio <- cbonds_sold$Portfolio.2021Q4
cbonds_sold$Unit_Linked <- cbonds_sold$UL.2021Q4
cbonds_sold$Fund <- cbonds_sold$Fund.2021Q4
cbonds_sold$Sector_Code <- cbonds_sold$Issuer_Sector_Code.2021Q4
cbonds_sold$Country <- cbonds_sold$Issuer_country.2021Q4
cbonds_sold$Currency <- cbonds_sold$Currency.2021Q4
cbonds_sold$Sub_Category <- cbonds_sold$Asset_Sub_Category.2021Q4

cbonds_sold <- cbonds_sold[-c(1:6, 10:16, 22:26, 30:36)]
cbonds_sold <- cbonds_sold[, c(24:29, 1:3, 6:7, 9:11, 14:15, 17:23, 30:32, 4, 12, 5, 13, 16)]

# ii) Resume table
cbonds_sold2 <- cbonds_sold
cbonds_sold2$Non_Life <- ifelse(cbonds_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
cbonds_sold2$Life_UL <- ifelse(cbonds_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
cbonds_sold2$Life_Not_UL <- ifelse(cbonds_sold2$Portfolio == 'Life [split applicable]' & cbonds_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
cbonds_sold2 <- cbonds_sold2[-c(2:6, 24:26, 31)]
cbonds_sold2 <- transform(cbonds_sold2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_cbonds_sold <- cbonds_sold2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_cbonds_sold <- resume_cbonds_sold[, c(1, 22:24, 2:21)]

# iii) Country table
cbonds_sold_country <- as.data.frame(tab1(cbonds_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
cbonds_sold_sector <- as.data.frame(tab1(cbonds_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(cbonds_sold, "~/Desktop/ASF/R Model/cbonds_sold.xlsx")






