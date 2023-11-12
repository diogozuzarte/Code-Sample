# govtbonds_matured

# i) data cleaning
govtbonds_matured <- govtbonds[govtbonds$Maturity_date.2021Q4 <= as.Date('2022-03-31') & !(is.na(govtbonds$Maturity_date.2021Q4)),]
govtbonds_matured$Change_Par <- govtbonds_matured$Par_amount.2022Q1 - govtbonds_matured$Par_amount.2021Q4
govtbonds_matured$Change_SII <- govtbonds_matured$SII_amount.2022Q1 - govtbonds_matured$SII_amount.2021Q4
govtbonds_matured$PChange_Par <- with(govtbonds_matured, ifelse(govtbonds_matured$Par_amount.2021Q4 != 0, govtbonds_matured$Change_Par/govtbonds_matured$Par_amount.2021Q4, 1))
govtbonds_matured$PChange_SII <- with(govtbonds_matured, ifelse(govtbonds_matured$SII_amount.2021Q4 != 0, govtbonds_matured$Change_SII/govtbonds_matured$SII_amount.2021Q4, 1))
govtbonds_matured$PChange_Unit_Percentage <- with(govtbonds_matured, ifelse(govtbonds_matured$Unit_SII_Percentage.2021Q4 != 0, (govtbonds_matured$Unit_SII_Percentage.2022Q1-govtbonds_matured$Unit_SII_Percentage.2021Q4)/govtbonds_matured$Unit_SII_Percentage.2021Q4, 100))

govtbonds_matured$Entity <- govtbonds_matured$Cod_Entidade.2021Q4
govtbonds_matured$Asset <- govtbonds_matured$Asset_ID_pk.2021Q4
govtbonds_matured$Title <- govtbonds_matured$Title.2021Q4
govtbonds_matured$Portfolio <- govtbonds_matured$Portfolio.2021Q4
govtbonds_matured$Unit_Linked <- govtbonds_matured$UL.2021Q4
govtbonds_matured$Fund <- govtbonds_matured$Fund.2021Q4
govtbonds_matured$Sector_Code <- govtbonds_matured$Issuer_Sector_Code.2021Q4
govtbonds_matured$Country <- govtbonds_matured$Issuer_country.2021Q4
govtbonds_matured$Currency <- govtbonds_matured$Currency.2021Q4
govtbonds_matured$Sub_Category <- govtbonds_matured$Asset_Sub_Category.2021Q4

govtbonds_matured <- govtbonds_matured[-c(1:6, 9:15, 19:24, 27:33)]
govtbonds_matured <- govtbonds_matured[, c(17:22, 1:2, 5, 6:7, 10, 12:16, 23:25, 3, 8, 4, 9, 11)]

# ii) Resume table
govtbonds_matured2 <- govtbonds_matured
govtbonds_matured2$Non_Life <- ifelse(govtbonds_matured2$Portfolio == 'Non-life [split applicable]', 1, 0)
govtbonds_matured2$Life_UL <- ifelse(govtbonds_matured2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
govtbonds_matured2$Life_Not_UL <- ifelse(govtbonds_matured2$Portfolio == 'Life [split applicable]' & govtbonds_matured2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
govtbonds_matured2 <- govtbonds_matured2[-c(2:6, 17:20, 25)]
govtbonds_matured2 <- transform(govtbonds_matured2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_govtbonds_matured <- govtbonds_matured2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_govtbonds_matured <- resume_govtbonds_matured[, c(1, 16:18, 2:15)]

# iii) Country table
govt_matured_country <- as.data.frame(tab1(govtbonds_matured$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
govt_matured_sector <- as.data.frame(tab1(govtbonds_matured$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(govtbonds_matured, "~/Desktop/ASF/R Model/govtbonds_matured.xlsx")







