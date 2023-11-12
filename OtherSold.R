# other_sold

# i) data cleaning
other_sold <- other[other$Par_amount.2022Q1 < other$Par_amount.2021Q4,]
other_sold$Change_Par <- other_sold$Par_amount.2022Q1 - other_sold$Par_amount.2021Q4
other_sold$Change_SII <- other_sold$SII_amount.2022Q1 - other_sold$SII_amount.2021Q4
other_sold$PChange_Par <- with(other_sold, ifelse(other_sold$Par_amount.2021Q4 != 0, other_sold$Change_Par/other_sold$Par_amount.2021Q4, -1))
other_sold$PChange_SII <- with(other_sold, ifelse(other_sold$SII_amount.2021Q4 != 0, other_sold$Change_SII/other_sold$SII_amount.2021Q4, -1))
other_sold$PChange_Unit_Percentage <- with(other_sold, ifelse(other_sold$Unit_SII_Percentage.2021Q4 != 0, (other_sold$Unit_SII_Percentage.2022Q1-other_sold$Unit_SII_Percentage.2021Q4)/other_sold$Unit_SII_Percentage.2021Q4, -100))

other_sold$Entity <- other_sold$Cod_Entidade.2021Q4
other_sold$Asset <- other_sold$Asset_ID_pk.2021Q4
other_sold$Title <- other_sold$Title.2021Q4
other_sold$Portfolio <- other_sold$Portfolio.2021Q4
other_sold$Unit_Linked <- other_sold$UL.2021Q4
other_sold$Fund <- other_sold$Fund.2021Q4
other_sold$Sector_Code <- other_sold$Issuer_Sector_Code.2021Q4
other_sold$Country <- other_sold$Issuer_country.2021Q4
other_sold$Currency <- other_sold$Currency.2021Q4
other_sold$Sub_Category <- other_sold$Asset_Sub_Category.2021Q4

other_sold <- other_sold[-c(1:6, 9:15, 19:23, 26:32)]
other_sold <- other_sold[, c(16:21, 1:2, 4, 6:7, 9, 11:15, 22:24, 3, 8, 10)]

# ii) Resume table
other_sold2 <- other_sold
other_sold2$Non_Life <- ifelse(other_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
other_sold2$Life_UL <- ifelse(other_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
other_sold2$Life_Not_UL <- ifelse(other_sold2$Portfolio == 'Life [split applicable]' & other_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
other_sold2 <- other_sold2[-c(2:6, 18:20, 23)]
other_sold2 <- transform(other_sold2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_other_sold <- other_sold2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_other_sold <- resume_other_sold[, c(1, 14:16, 2:13)]

# iii) Country table
other_sold_country <- as.data.frame(tab1(other_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
other_sold_sector <- as.data.frame(tab1(other_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(other_sold, "~/Desktop/ASF/R Model/other_sold.xlsx")






