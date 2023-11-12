# deposits_bought

# i) data cleaning
deposits_bought <- deposits[deposits$Par_amount.2022Q1 > deposits$Par_amount.2021Q4,]
deposits_bought$Change_Par <- deposits_bought$Par_amount.2022Q1 - deposits_bought$Par_amount.2021Q4
deposits_bought$Change_SII <- deposits_bought$SII_amount.2022Q1 - deposits_bought$SII_amount.2021Q4
deposits_bought$PChange_Par <- with(deposits_bought, ifelse(deposits_bought$Par_amount.2021Q4 != 0, deposits_bought$Change_Par/deposits_bought$Par_amount.2021Q4, 1))
deposits_bought$PChange_SII <- with(deposits_bought, ifelse(deposits_bought$SII_amount.2021Q4 != 0, deposits_bought$Change_SII/deposits_bought$SII_amount.2021Q4, 1))
deposits_bought$PChange_Unit_Percentage <- with(deposits_bought, ifelse(deposits_bought$Unit_SII_Percentage.2021Q4 != 0, (deposits_bought$Unit_SII_Percentage.2022Q1-deposits_bought$Unit_SII_Percentage.2021Q4)/deposits_bought$Unit_SII_Percentage.2021Q4, 100))

deposits_bought$Entity <- deposits_bought$Cod_Entidade.2022Q1
deposits_bought$Asset <- deposits_bought$Asset_ID_pk.2022Q1
deposits_bought$Title <- deposits_bought$Title.2022Q1
deposits_bought$Portfolio <- deposits_bought$Portfolio.2022Q1
deposits_bought$Unit_Linked <- deposits_bought$UL.2022Q1
deposits_bought$Fund <- deposits_bought$Fund.2022Q1
deposits_bought$Sector_Code <- deposits_bought$Issuer_Sector_Code.2022Q1
deposits_bought$Country <- deposits_bought$Issuer_country.2022Q1
deposits_bought$Currency <- deposits_bought$Currency.2022Q1
deposits_bought$Sub_Category <- deposits_bought$Asset_Sub_Category.2022Q1

deposits_bought <- deposits_bought[-c(1:6, 9:15, 19:23, 26:32)]
deposits_bought <- deposits_bought[, c(16:21, 1:2, 4, 6:7, 9, 11:15, 22:24, 3, 8, 10)]

# ii) Resume table
deposits_bought2 <- deposits_bought
deposits_bought2$Non_Life <- ifelse(deposits_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
deposits_bought2$Life_UL <- ifelse(deposits_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
deposits_bought2$Life_Not_UL <- ifelse(deposits_bought2$Portfolio == 'Life [split applicable]' & deposits_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
deposits_bought2 <- deposits_bought2[-c(2:6, 18:20, 23)]
deposits_bought2 <- transform(deposits_bought2, CQS.2021Q4 = as.numeric(CQS.2021Q4), CQS.2022Q1 = as.numeric(CQS.2022Q1))
resume_deposits_bought <- deposits_bought2 %>% group_by(Entity) %>% summarise(Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), CQS.2021Q4 = mean(CQS.2021Q4, na.rm = TRUE), CQS.2022Q1 = mean(CQS.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_deposits_bought <- resume_deposits_bought[, c(1, 14:16, 2:13)]

# iii) Country table
deposits_bought_country <- as.data.frame(tab1(deposits_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
deposits_bought_sector <- as.data.frame(tab1(deposits_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(deposits_bought, "~/Desktop/ASF/R Model/deposits_bought.xlsx")







