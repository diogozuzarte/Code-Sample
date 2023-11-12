# preif_sold

# i) data cleaning
preif_sold <- preif[preif$Par_amount.2022Q1 < preif$Par_amount.2021Q4 | preif$Quantity.2022Q1 < preif$Quantity.2021Q4,]
preif_sold$Change_Par <- preif_sold$Par_amount.2022Q1 - preif_sold$Par_amount.2021Q4
preif_sold$Change_Quantity <- preif_sold$Quantity.2022Q1 - preif_sold$Quantity.2021Q4
preif_sold$Change_SII <- preif_sold$SII_amount.2022Q1 - preif_sold$SII_amount.2021Q4
preif_sold$PChange_Par <- with(preif_sold, ifelse(preif_sold$Par_amount.2021Q4 != 0, preif_sold$Change_Par/preif_sold$Par_amount.2021Q4, 1))
preif_sold$PChange_Quantity <- with(preif_sold, ifelse(preif_sold$Quantity.2021Q4 != 0, preif_sold$Change_Quantity/preif_sold$Quantity.2021Q4, 1))
preif_sold$PChange_SII <- with(preif_sold, ifelse(preif_sold$SII_amount.2021Q4 != 0, preif_sold$Change_SII/preif_sold$SII_amount.2021Q4, 1))
preif_sold$PChange_Unit_Percentage <- with(preif_sold, ifelse(preif_sold$Unit_SII_Percentage.2021Q4 != 0, (preif_sold$Unit_SII_Percentage.2022Q1-preif_sold$Unit_SII_Percentage.2021Q4)/preif_sold$Unit_SII_Percentage.2021Q4, 100))

preif_sold$Entity <- preif_sold$Cod_Entidade.2021Q4
preif_sold$Asset <- preif_sold$Asset_ID_pk.2021Q4
preif_sold$Title <- preif_sold$Title.2021Q4
preif_sold$Portfolio <- preif_sold$Portfolio.2021Q4
preif_sold$Unit_Linked <- preif_sold$UL.2021Q4
preif_sold$Fund <- preif_sold$Fund.2021Q4
preif_sold$Sector_Code <- preif_sold$Issuer_Sector_Code.2021Q4
preif_sold$Country <- preif_sold$Issuer_country.2021Q4
preif_sold$Currency <- preif_sold$Currency.2021Q4
preif_sold$Sub_Category <- preif_sold$Asset_Sub_Category.2021Q4

preif_sold <- preif_sold[-c(1:6, 10:16, 19:23, 27:33)]
preif_sold <- preif_sold[, c(18:23, 1:5, 6:17, 24:26)]

# ii) Resume table
preif_sold2 <- preif_sold
preif_sold2$Non_Life <- ifelse(preif_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
preif_sold2$Life_UL <- ifelse(preif_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
preif_sold2$Life_Not_UL <- ifelse(preif_sold2$Portfolio == 'Life [split applicable]' & preif_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
preif_sold2 <- preif_sold2[-c(2:6, 24:26)]
resume_preif_sold <- preif_sold2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Par_amount.2021Q4 = sum(Par_amount.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4, na.rm = TRUE), Unit_SII_Percentage.2021Q4 = mean(Unit_SII_Percentage.2021Q4, na.rm = TRUE), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Par_amount.2022Q1 = sum(Par_amount.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1, na.rm = TRUE), Unit_SII_Percentage.2022Q1 = mean(Unit_SII_Percentage.2022Q1, na.rm = TRUE), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Par = sum(Change_Par), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Par = sum(Change_Par)/sum(Par_amount.2021Q4), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_preif_sold <- resume_preif_sold[, c(1, 18:20, 2:17)]

# iii) Country table
preif_sold_country <- as.data.frame(tab1(preif_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
preif_sold_sector <- as.data.frame(tab1(preif_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(preif_sold, "~/Desktop/ASF/R Model/preif_sold.xlsx")






