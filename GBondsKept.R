# govtbonds_kept

# i) data cleaning
govtbonds_kept <- govtbonds[govtbonds$Par_amount.2021Q4 != 0 & govtbonds$Par_amount.2022Q1 != 0,]
govtbonds_kept$Change_Par <- 0
govtbonds_kept$Change_SII <- ifelse(govtbonds_kept$Par_amount.2022Q1==govtbonds_kept$Par_amount.2021Q4, govtbonds_kept$SII_amount.2022Q1 - govtbonds_kept$SII_amount.2021Q4, 0)
govtbonds_kept$PChange_Par <- with(govtbonds_kept, ifelse(govtbonds_kept$Par_amount.2021Q4 != 0, govtbonds_kept$Change_Par/govtbonds_kept$Par_amount.2021Q4, 1))
govtbonds_kept$PChange_SII <- with(govtbonds_kept, ifelse(govtbonds_kept$SII_amount.2021Q4 != 0, govtbonds_kept$Change_SII/govtbonds_kept$SII_amount.2021Q4, 1))
govtbonds_kept$PChange_Unit_Percentage <- with(govtbonds_kept, ifelse(govtbonds_kept$Unit_SII_Percentage.2021Q4 != 0, (govtbonds_kept$Unit_SII_Percentage.2022Q1-govtbonds_kept$Unit_SII_Percentage.2021Q4)/govtbonds_kept$Unit_SII_Percentage.2021Q4, 100))

govtbonds_kept$Entity <- govtbonds_kept$Cod_Entidade.2022Q1
govtbonds_kept$Asset <- govtbonds_kept$Asset_ID_pk.2022Q1
govtbonds_kept$Title <- govtbonds_kept$Title.2022Q1
govtbonds_kept$Portfolio <- govtbonds_kept$Portfolio.2022Q1
govtbonds_kept$Unit_Linked <- govtbonds_kept$UL.2022Q1
govtbonds_kept$Fund <- govtbonds_kept$Fund.2022Q1
govtbonds_kept$Sector_Code <- govtbonds_kept$Issuer_Sector_Code.2022Q1
govtbonds_kept$Country <- govtbonds_kept$Issuer_country.2022Q1
govtbonds_kept$Currency <- govtbonds_kept$Currency.2022Q1
govtbonds_kept$Sub_Category <- govtbonds_kept$Asset_Sub_Category.2022Q1

govtbonds_kept <- govtbonds_kept[-c(1:6, 9:15, 19:24, 27:33)]
govtbonds_kept <- govtbonds_kept[, c(17:22, 1:2, 5, 6:7, 10, 12:16, 23:25, 3, 8, 4, 9, 11)]

# ii) Resume table
govtbonds_kept2 <- govtbonds_kept
govtbonds_kept2$Non_Life <- ifelse(govtbonds_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
govtbonds_kept2$Life_UL <- ifelse(govtbonds_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
govtbonds_kept2$Life_Not_UL <- ifelse(govtbonds_kept2$Portfolio == 'Life [split applicable]' & govtbonds_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
govtbonds_kept2 <- govtbonds_kept2[-c(2:6, 17:20, 25)]
govtbonds_kept2 <- transform(govtbonds_kept2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_govtbonds_kept <- govtbonds_kept2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_govtbonds_kept <- resume_govtbonds_kept[, c(1, 16:18, 2:15)]

# iii) Country table
govtbonds_kept_country <- as.data.frame(tab1(govtbonds_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
govtbonds_kept_sector <- as.data.frame(tab1(govtbonds_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(govtbonds_kept, "~/Desktop/ASF/R Model/govtbonds_kept.xlsx")







