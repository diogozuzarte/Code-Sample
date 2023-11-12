# preif_bought

# i) data cleaning
preif_bought <- preif[(preif$Par_amount.2022Q1 > preif$Par_amount.2021Q4) | (preif$Quantity.2022Q1 > preif$Quantity.2021Q4),]
preif_bought$Change_Par <- preif_bought$Par_amount.2022Q1 - preif_bought$Par_amount.2021Q4
preif_bought$Change_Quantity <- preif_bought$Quantity.2022Q1 - preif_bought$Quantity.2021Q4
preif_bought$Change_SII <- preif_bought$SII_amount.2022Q1 - preif_bought$SII_amount.2021Q4
preif_bought$PChange_Par <- with(preif_bought, ifelse(preif_bought$Par_amount.2021Q4 != 0, preif_bought$Change_Par/preif_bought$Par_amount.2021Q4, 1))
preif_bought$PChange_Quantity <- with(preif_bought, ifelse(preif_bought$Quantity.2021Q4 != 0, preif_bought$Change_Quantity/preif_bought$Quantity.2021Q4, 1))
preif_bought$PChange_SII <- with(preif_bought, ifelse(preif_bought$SII_amount.2021Q4 != 0, preif_bought$Change_SII/preif_bought$SII_amount.2021Q4, 1))
preif_bought$PChange_Unit_Percentage <- with(preif_bought, ifelse(preif_bought$Unit_SII_Percentage.2021Q4 != 0, (preif_bought$Unit_SII_Percentage.2022Q1-preif_bought$Unit_SII_Percentage.2021Q4)/preif_bought$Unit_SII_Percentage.2021Q4, 100))

preif_bought$Entity <- preif_bought$Cod_Entidade.2022Q1
preif_bought$Asset <- preif_bought$Asset_ID_pk.2022Q1
preif_bought$Title <- preif_bought$Title.2022Q1
preif_bought$Portfolio <- preif_bought$Portfolio.2022Q1
preif_bought$Unit_Linked <- preif_bought$UL.2022Q1
preif_bought$Fund <- preif_bought$Fund.2022Q1
preif_bought$Sector_Code <- preif_bought$Issuer_Sector_Code.2022Q1
preif_bought$Country <- preif_bought$Issuer_country.2022Q1
preif_bought$Currency <- preif_bought$Currency.2022Q1
preif_bought$Sub_Category <- preif_bought$Asset_Sub_Category.2022Q1

preif_bought <- preif_bought[-c(1:6, 10:16, 19:23, 27:33)]
preif_bought <- preif_bought[, c(18:23, 1:5, 6:17, 24:26)]

# ii) Resume table
preif_bought2 <- preif_bought
preif_bought2$Non_Life <- ifelse(preif_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
preif_bought2$Life_UL <- ifelse(preif_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
preif_bought2$Life_Not_UL <- ifelse(preif_bought2$Portfolio == 'Life [split applicable]' & preif_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
preif_bought2 <- preif_bought2[-c(2:6, 24:26)]
resume_preif_bought <- preif_bought2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_preif_bought <- resume_preif_bought[, c(1, 18:20, 2:17)]

# iii) Country table
preif_bought_country <- as.data.frame(tab1(preif_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
preif_bought_sector <- as.data.frame(tab1(preif_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(preif_bought, "~/Desktop/ASF/R Model/preif_bought.xlsx")






