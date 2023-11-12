# equity_sold

# i) data cleaning
equity_sold <- equity[equity$Quantity.2022Q1 < equity$Quantity.2021Q4,]
equity_sold$Change_Quantity <- equity_sold$Quantity.2022Q1 - equity_sold$Quantity.2021Q4
equity_sold$Change_SII <- equity_sold$SII_amount.2022Q1 - equity_sold$SII_amount.2021Q4
equity_sold$PChange_Quantity <- with(equity_sold, ifelse(equity_sold$Quantity.2021Q4 != 0, equity_sold$Change_Quantity/equity_sold$Quantity.2021Q4, 1))
equity_sold$PChange_SII <- with(equity_sold, ifelse(equity_sold$SII_amount.2021Q4 != 0, equity_sold$Change_SII/equity_sold$SII_amount.2021Q4, 1))
equity_sold$PChange_Unit_Price <- with(equity_sold, ifelse(equity_sold$Unit_SII_Price.2021Q4 != 0, (equity_sold$Unit_SII_Price.2022Q1-equity_sold$Unit_SII_Price.2021Q4)/equity_sold$Unit_SII_Price.2021Q4, 100))

equity_sold$Entity <- equity_sold$Cod_Entidade.2021Q4
equity_sold$Asset <- equity_sold$Asset_ID_pk.2021Q4
equity_sold$Title <- equity_sold$Title.2021Q4
equity_sold$Portfolio <- equity_sold$Portfolio.2021Q4
equity_sold$Unit_Linked <- equity_sold$UL.2021Q4
equity_sold$Fund <- equity_sold$Fund.2021Q4
equity_sold$Sector_Code <- equity_sold$Issuer_Sector_Code.2021Q4
equity_sold$Country <- equity_sold$Issuer_country.2021Q4
equity_sold$Currency <- equity_sold$Currency.2021Q4
equity_sold$Sub_Category <- equity_sold$Asset_Sub_Category.2021Q4

equity_sold <- equity_sold[-c(1:6, 9:15, 17:21, 24:30)]
equity_sold <- equity_sold[, c(12:17, 1:11, 18:20)]

# ii) Resume table
equity_sold2 <- equity_sold
equity_sold2$Non_Life <- ifelse(equity_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
equity_sold2$Life_UL <- ifelse(equity_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
equity_sold2$Life_Not_UL <- ifelse(equity_sold2$Portfolio == 'Life [split applicable]' & equity_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
equity_sold2 <- equity_sold2[-c(2:6, 18:20)]
resume_equity_sold <- equity_sold2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_equity_sold <- resume_equity_sold[, c(1, 12:14, 2:11)]

# iii) Country table
equity_sold_country <- as.data.frame(tab1(equity_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
equity_sold_sector <- as.data.frame(tab1(equity_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(equity_sold, "~/Desktop/ASF/R Model/equity_sold.xlsx")






