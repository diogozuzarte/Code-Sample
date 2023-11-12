# other_kept

# i) data cleaning
other_kept <- other[other$Par_amount.2021Q4 != 0 & other$Par_amount.2022Q1 != 0,]
other_kept$Change_Par <- 0
other_kept$Change_SII <- ifelse(other_kept$Par_amount.2022Q1==other_kept$Par_amount.2021Q4, other_kept$SII_amount.2022Q1 - other_kept$SII_amount.2021Q4, 0)
other_kept$PChange_Par <- with(other_kept, ifelse(other_kept$Par_amount.2021Q4 != 0, other_kept$Change_Par/other_kept$Par_amount.2021Q4, 1))
other_kept$PChange_SII <- with(other_kept, ifelse(other_kept$SII_amount.2021Q4 != 0, other_kept$Change_SII/other_kept$SII_amount.2021Q4, 1))
other_kept$PChange_Unit_Percentage <- with(other_kept, ifelse(other_kept$Unit_SII_Percentage.2021Q4 != 0, (other_kept$Unit_SII_Percentage.2022Q1-other_kept$Unit_SII_Percentage.2021Q4)/other_kept$Unit_SII_Percentage.2021Q4, 100))

other_kept$Entity <- other_kept$Cod_Entidade.2022Q1
other_kept$Asset <- other_kept$Asset_ID_pk.2022Q1
other_kept$Title <- other_kept$Title.2022Q1
other_kept$Portfolio <- other_kept$Portfolio.2022Q1
other_kept$Unit_Linked <- other_kept$UL.2022Q1
other_kept$Fund <- other_kept$Fund.2022Q1
other_kept$Sector_Code <- other_kept$Issuer_Sector_Code.2022Q1
other_kept$Country <- other_kept$Issuer_country.2022Q1
other_kept$Currency <- other_kept$Currency.2022Q1
other_kept$Sub_Category <- other_kept$Asset_Sub_Category.2022Q1

other_kept <- other_kept[-c(1:6, 9:15, 19:23, 26:32)]
other_kept <- other_kept[, c(16:21, 1:2, 4, 6:7, 9, 11:15, 22:24, 3, 8, 10)]

# ii) Resume table
other_kept2 <- other_kept
other_kept2$Non_Life <- ifelse(other_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
other_kept2$Life_UL <- ifelse(other_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
other_kept2$Life_Not_UL <- ifelse(other_kept2$Portfolio == 'Life [split applicable]' & other_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
other_kept2 <- other_kept2[-c(2:6, 18:20, 23)]
other_kept2 <- transform(other_kept2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_other_kept <- other_kept2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_other_kept <- resume_other_kept[, c(1, 14:16, 2:13)]

# iii) Country table
other_kept_country <- as.data.frame(tab1(other_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
other_kept_sector <- as.data.frame(tab1(other_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(other_kept, "~/Desktop/ASF/R Model/other_kept.xlsx")







