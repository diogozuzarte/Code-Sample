# deposits_kept

# i) data cleaning
deposits_kept <- deposits[deposits$Par_amount.2021Q4 != 0 & deposits$Par_amount.2022Q1 != 0,]
deposits_kept$Change_Par <- 0
deposits_kept$Change_SII <- ifelse(deposits_kept$Par_amount.2022Q1==deposits_kept$Par_amount.2021Q4, deposits_kept$SII_amount.2022Q1 - deposits_kept$SII_amount.2021Q4, 0)
deposits_kept$PChange_Par <- with(deposits_kept, ifelse(deposits_kept$Par_amount.2021Q4 != 0, deposits_kept$Change_Par/deposits_kept$Par_amount.2021Q4, 1))
deposits_kept$PChange_SII <- with(deposits_kept, ifelse(deposits_kept$SII_amount.2021Q4 != 0, deposits_kept$Change_SII/deposits_kept$SII_amount.2021Q4, 1))
deposits_kept$PChange_Unit_Percentage <- with(deposits_kept, ifelse(deposits_kept$Unit_SII_Percentage.2021Q4 != 0, (deposits_kept$Unit_SII_Percentage.2022Q1-deposits_kept$Unit_SII_Percentage.2021Q4)/deposits_kept$Unit_SII_Percentage.2021Q4, 100))

deposits_kept$Entity <- deposits_kept$Cod_Entidade.2022Q1
deposits_kept$Asset <- deposits_kept$Asset_ID_pk.2022Q1
deposits_kept$Title <- deposits_kept$Title.2022Q1
deposits_kept$Portfolio <- deposits_kept$Portfolio.2022Q1
deposits_kept$Unit_Linked <- deposits_kept$UL.2022Q1
deposits_kept$Fund <- deposits_kept$Fund.2022Q1
deposits_kept$Sector_Code <- deposits_kept$Issuer_Sector_Code.2022Q1
deposits_kept$Country <- deposits_kept$Issuer_country.2022Q1
deposits_kept$Currency <- deposits_kept$Currency.2022Q1
deposits_kept$Sub_Category <- deposits_kept$Asset_Sub_Category.2022Q1

deposits_kept <- deposits_kept[-c(1:6, 9:15, 19:23, 26:32)]
deposits_kept <- deposits_kept[, c(16:21, 1:2, 4, 6:7, 9, 11:15, 22:24, 3, 8, 10)]

# ii) Resume table
deposits_kept2 <- deposits_kept
deposits_kept2$Non_Life <- ifelse(deposits_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
deposits_kept2$Life_UL <- ifelse(deposits_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
deposits_kept2$Life_Not_UL <- ifelse(deposits_kept2$Portfolio == 'Life [split applicable]' & deposits_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
deposits_kept2 <- deposits_kept2[-c(2:6, 18:20, 23)]
deposits_kept2 <- transform(deposits_kept2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_deposits_kept <- deposits_kept2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_deposits_kept <- resume_deposits_kept[, c(1, 14:16, 2:13)]

# iii) Country table
deposits_kept_country <- as.data.frame(tab1(deposits_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
deposits_kept_sector <- as.data.frame(tab1(deposits_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(deposits_kept, "~/Desktop/ASF/R Model/deposits_kept.xlsx")







