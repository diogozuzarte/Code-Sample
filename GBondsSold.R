# govtbonds_sold

# i) data cleaning
govtbonds$Maturity_date.2021Q4 <- format(govtbonds$Maturity_date.2021Q4, "%Y-%m-%d")
govtbonds_sold <- govtbonds[govtbonds$Par_amount.2022Q1 < govtbonds$Par_amount.2021Q4 & govtbonds$Maturity_date.2021Q4 > as.Date('2022-03-31'),]
govtbonds_sold$Change_Par <- govtbonds_sold$Par_amount.2022Q1 - govtbonds_sold$Par_amount.2021Q4
govtbonds_sold$Change_SII <- govtbonds_sold$SII_amount.2022Q1 - govtbonds_sold$SII_amount.2021Q4
govtbonds_sold$PChange_Par <- with(govtbonds_sold, ifelse(govtbonds_sold$Par_amount.2021Q4 != 0, govtbonds_sold$Change_Par/govtbonds_sold$Par_amount.2021Q4, -1))
govtbonds_sold$PChange_SII <- with(govtbonds_sold, ifelse(govtbonds_sold$SII_amount.2021Q4 != 0, govtbonds_sold$Change_SII/govtbonds_sold$SII_amount.2021Q4, -1))
govtbonds_sold$PChange_Unit_Percentage <- with(govtbonds_sold, ifelse(govtbonds_sold$Unit_SII_Percentage.2021Q4 != 0, (govtbonds_sold$Unit_SII_Percentage.2022Q1-govtbonds_sold$Unit_SII_Percentage.2021Q4)/govtbonds_sold$Unit_SII_Percentage.2021Q4, -100))

govtbonds_sold$Entity <- govtbonds_sold$Cod_Entidade.2021Q4
govtbonds_sold$Asset <- govtbonds_sold$Asset_ID_pk.2021Q4
govtbonds_sold$Title <- govtbonds_sold$Title.2021Q4
govtbonds_sold$Portfolio <- govtbonds_sold$Portfolio.2021Q4
govtbonds_sold$Unit_Linked <- govtbonds_sold$UL.2021Q4
govtbonds_sold$Fund <- govtbonds_sold$Fund.2021Q4
govtbonds_sold$Sector_Code <- govtbonds_sold$Issuer_Sector_Code.2021Q4
govtbonds_sold$Country <- govtbonds_sold$Issuer_country.2021Q4
govtbonds_sold$Currency <- govtbonds_sold$Currency.2021Q4
govtbonds_sold$Sub_Category <- govtbonds_sold$Asset_Sub_Category.2021Q4

govtbonds_sold <- govtbonds_sold[-c(1:6, 9:15, 19:24, 27:33)]
govtbonds_sold <- govtbonds_sold[, c(17:22, 1:2, 5, 6:7, 10, 12:16, 23:25, 3, 8, 4, 9, 11)]

# ii) Resume table
govtbonds_sold2 <- govtbonds_sold
govtbonds_sold2$Non_Life <- ifelse(govtbonds_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
govtbonds_sold2$Life_UL <- ifelse(govtbonds_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
govtbonds_sold2$Life_Not_UL <- ifelse(govtbonds_sold2$Portfolio == 'Life [split applicable]' & govtbonds_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
govtbonds_sold2 <- govtbonds_sold2[-c(2:6, 17:20, 25)]
govtbonds_sold2 <- transform(govtbonds_sold2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_govtbonds_sold <- govtbonds_sold2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_govtbonds_sold <- resume_govtbonds_sold[, c(1, 16:18, 2:15)]

# iii) Country table
govtbonds_sold_country <- as.data.frame(tab1(govtbonds_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
govtbonds_sold_sector <- as.data.frame(tab1(govtbonds_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(govtbonds_sold, "~/Desktop/ASF/R Model/govtbonds_sold.xlsx")






