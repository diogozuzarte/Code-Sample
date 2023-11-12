# preif_kept

# i) data cleaning
preif_kept <- preif[(preif$Par_amount.2022Q1 != 0 & preif$Par_amount.2021Q4 != 0) | (preif$Quantity.2022Q1 != 0 & preif$Quantity.2021Q4 != 0),]
preif_kept$Change_Par <- preif_kept$Par_amount.2022Q1 - preif_kept$Par_amount.2021Q4
preif_kept$Change_Quantity <- preif_kept$Quantity.2022Q1 - preif_kept$Quantity.2021Q4
preif_kept$Change_SII <- ifelse(preif_kept$Quantity.2022Q1==preif_kept$Quantity.2021Q4, preif_kept$SII_amount.2022Q1 - preif_kept$SII_amount.2021Q4, 0)
preif_kept$PChange_Par <- with(preif_kept, ifelse(preif_kept$Par_amount.2021Q4 != 0, preif_kept$Change_Par/preif_kept$Par_amount.2021Q4, 1))
preif_kept$PChange_Quantity <- with(preif_kept, ifelse(preif_kept$Quantity.2021Q4 != 0, preif_kept$Change_Quantity/preif_kept$Quantity.2021Q4, 1))
preif_kept$PChange_SII <- with(preif_kept, ifelse(preif_kept$SII_amount.2021Q4 != 0, preif_kept$Change_SII/preif_kept$SII_amount.2021Q4, 1))
preif_kept$PChange_Unit_Percentage <- with(preif_kept, ifelse(preif_kept$Unit_SII_Percentage.2021Q4 != 0, (preif_kept$Unit_SII_Percentage.2022Q1-preif_kept$Unit_SII_Percentage.2021Q4)/preif_kept$Unit_SII_Percentage.2021Q4, 100))

preif_kept$Entity <- preif_kept$Cod_Entidade.2022Q1
preif_kept$Asset <- preif_kept$Asset_ID_pk.2022Q1
preif_kept$Title <- preif_kept$Title.2022Q1
preif_kept$Portfolio <- preif_kept$Portfolio.2022Q1
preif_kept$Unit_Linked <- preif_kept$UL.2022Q1
preif_kept$Fund <- preif_kept$Fund.2022Q1
preif_kept$Sector_Code <- preif_kept$Issuer_Sector_Code.2022Q1
preif_kept$Country <- preif_kept$Issuer_country.2022Q1
preif_kept$Currency <- preif_kept$Currency.2022Q1
preif_kept$Sub_Category <- preif_kept$Asset_Sub_Category.2022Q1

preif_kept <- preif_kept[-c(1:6, 10:16, 19:23, 27:33)]
preif_kept <- preif_kept[, c(18:23, 1:5, 6:17, 24:26)]

# ii) Resume table
preif_kept2 <- preif_kept
preif_kept2$Non_Life <- ifelse(preif_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
preif_kept2$Life_UL <- ifelse(preif_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
preif_kept2$Life_Not_UL <- ifelse(preif_kept2$Portfolio == 'Life [split applicable]' & preif_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
preif_kept2 <- preif_kept2[-c(2:6, 24:26)]
resume_preif_kept <- preif_kept2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_preif_kept <- resume_preif_kept[, c(1, 18:20, 2:17)]

# iii) Country table
preif_kept_country <- as.data.frame(tab1(preif_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
preif_kept_sector <- as.data.frame(tab1(preif_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(preif_kept, "~/Desktop/ASF/R Model/preif_kept.xlsx")







