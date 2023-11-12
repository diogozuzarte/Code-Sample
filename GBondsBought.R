# govtbonds_bought

# i) data cleaning
govtbonds_bought <- govtbonds[govtbonds$Par_amount.2022Q1 > govtbonds$Par_amount.2021Q4,]
govtbonds_bought$Change_Par <- govtbonds_bought$Par_amount.2022Q1 - govtbonds_bought$Par_amount.2021Q4
govtbonds_bought$Change_SII <- govtbonds_bought$SII_amount.2022Q1 - govtbonds_bought$SII_amount.2021Q4
govtbonds_bought$PChange_Par <- with(govtbonds_bought, ifelse(govtbonds_bought$Par_amount.2021Q4 != 0, govtbonds_bought$Change_Par/govtbonds_bought$Par_amount.2021Q4, 1))
govtbonds_bought$PChange_SII <- with(govtbonds_bought, ifelse(govtbonds_bought$SII_amount.2021Q4 != 0, govtbonds_bought$Change_SII/govtbonds_bought$SII_amount.2021Q4, 1))
govtbonds_bought$PChange_Unit_Percentage <- with(govtbonds_bought, ifelse(govtbonds_bought$Unit_SII_Percentage.2021Q4 != 0, (govtbonds_bought$Unit_SII_Percentage.2022Q1-govtbonds_bought$Unit_SII_Percentage.2021Q4)/govtbonds_bought$Unit_SII_Percentage.2021Q4, 100))

govtbonds_bought$Entity <- govtbonds_bought$Cod_Entidade.2022Q1
govtbonds_bought$Asset <- govtbonds_bought$Asset_ID_pk.2022Q1
govtbonds_bought$Title <- govtbonds_bought$Title.2022Q1
govtbonds_bought$Portfolio <- govtbonds_bought$Portfolio.2022Q1
govtbonds_bought$Unit_Linked <- govtbonds_bought$UL.2022Q1
govtbonds_bought$Fund <- govtbonds_bought$Fund.2022Q1
govtbonds_bought$Sector_Code <- govtbonds_bought$Issuer_Sector_Code.2022Q1
govtbonds_bought$Country <- govtbonds_bought$Issuer_country.2022Q1
govtbonds_bought$Currency <- govtbonds_bought$Currency.2022Q1
govtbonds_bought$Sub_Category <- govtbonds_bought$Asset_Sub_Category.2022Q1

govtbonds_bought <- govtbonds_bought[-c(1:6, 9:15, 19:24, 27:33)]
govtbonds_bought <- govtbonds_bought[, c(17:22, 1:2, 5, 6:7, 10, 12:16, 23:25, 3, 8, 4, 9, 11)]

# ii) Resume table
govtbonds_bought2 <- govtbonds_bought
govtbonds_bought2$Non_Life <- ifelse(govtbonds_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
govtbonds_bought2$Life_UL <- ifelse(govtbonds_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
govtbonds_bought2$Life_Not_UL <- ifelse(govtbonds_bought2$Portfolio == 'Life [split applicable]' & govtbonds_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
govtbonds_bought2 <- govtbonds_bought2[-c(2:6, 17:20, 25)]
govtbonds_bought2 <- transform(govtbonds_bought2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_govtbonds_bought <- govtbonds_bought2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_govtbonds_bought <- resume_govtbonds_bought[, c(1, 16:18, 2:15)]

# iii) Country table
govtbonds_bought_country <- as.data.frame(tab1(govtbonds_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
govtbonds_bought_sector <- as.data.frame(tab1(govtbonds_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(govtbonds_bought, "~/Desktop/ASF/R Model/govtbonds_bought.xlsx")







