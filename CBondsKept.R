# cbonds_kept

# i) data cleaning
cbonds_kept <- cbonds[(cbonds$Par_amount.2021Q4 != 0 & cbonds$Par_amount.2022Q1 != 0) | (cbonds$Quantity.2021Q4 != 0 & cbonds$Quantity.2022Q1 != 0),]
cbonds_kept$Change_Par <- 0
cbonds_kept$Change_Quantity <- 0
cbonds_kept$Change_SII <- ifelse(cbonds_kept$Par_amount.2022Q1==cbonds_kept$Par_amount.2021Q4, cbonds_kept$SII_amount.2022Q1 - cbonds_kept$SII_amount.2021Q4, 0)
cbonds_kept$PChange_Par <- with(cbonds_kept, ifelse(cbonds_kept$Par_amount.2021Q4 != 0, cbonds_kept$Change_Par/cbonds_kept$Par_amount.2021Q4, 1))
cbonds_kept$PChange_Quantity <- with(cbonds_kept, ifelse(cbonds_kept$Quantity.2021Q4 != 0, cbonds_kept$Change_Par/cbonds_kept$Quantity.2021Q4, 1))
cbonds_kept$PChange_SII <- with(cbonds_kept, ifelse(cbonds_kept$SII_amount.2021Q4 != 0, cbonds_kept$Change_SII/cbonds_kept$SII_amount.2021Q4, 1))
cbonds_kept$PChange_Unit_Percentage <- with(cbonds_kept, ifelse(cbonds_kept$Unit_SII_Percentage.2021Q4 != 0, (cbonds_kept$Unit_SII_Percentage.2022Q1-cbonds_kept$Unit_SII_Percentage.2021Q4)/cbonds_kept$Unit_SII_Percentage.2021Q4, 100))

cbonds_kept$Entity <- cbonds_kept$Cod_Entidade.2022Q1
cbonds_kept$Asset <- cbonds_kept$Asset_ID_pk.2022Q1
cbonds_kept$Title <- cbonds_kept$Title.2022Q1
cbonds_kept$Portfolio <- cbonds_kept$Portfolio.2022Q1
cbonds_kept$Unit_Linked <- cbonds_kept$UL.2022Q1
cbonds_kept$Fund <- cbonds_kept$Fund.2022Q1
cbonds_kept$Sector_Code <- cbonds_kept$Issuer_Sector_Code.2022Q1
cbonds_kept$Country <- cbonds_kept$Issuer_country.2022Q1
cbonds_kept$Currency <- cbonds_kept$Currency.2022Q1
cbonds_kept$Sub_Category <- cbonds_kept$Asset_Sub_Category.2022Q1

cbonds_kept <- cbonds_kept[-c(1:6, 10:16, 22:26, 30:36)]
cbonds_kept <- cbonds_kept[, c(24:29, 1:3, 6:7, 9:11, 14:15, 17:23, 30:32, 4, 12, 5, 13, 16)]

# ii) Resume table
cbonds_kept2 <- cbonds_kept
cbonds_kept2$Non_Life <- ifelse(cbonds_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
cbonds_kept2$Life_UL <- ifelse(cbonds_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
cbonds_kept2$Life_Not_UL <- ifelse(cbonds_kept2$Portfolio == 'Life [split applicable]' & cbonds_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
cbonds_kept2 <- cbonds_kept2[-c(2:6, 24:26, 31)]
cbonds_kept2 <- transform(cbonds_kept2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_cbonds_kept <- cbonds_kept2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_cbonds_kept <- resume_cbonds_kept[, c(1, 22:24, 2:21)]

# iii) Country table
cbonds_kept_country <- as.data.frame(tab1(cbonds_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
cbonds_kept_sector <- as.data.frame(tab1(cbonds_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(cbonds_kept, "~/Desktop/ASF/R Model/cbonds_kept.xlsx")







