# equity_bought

# i) data cleaning
equity_bought <- equity[equity$Quantity.2022Q1 > equity$Quantity.2021Q4,]
equity_bought$Change_Quantity <- equity_bought$Quantity.2022Q1 - equity_bought$Quantity.2021Q4
equity_bought$Change_SII <- equity_bought$SII_amount.2022Q1 - equity_bought$SII_amount.2021Q4
equity_bought$PChange_Quantity <- with(equity_bought, ifelse(equity_bought$Quantity.2021Q4 != 0, equity_bought$Change_Quantity/equity_bought$Quantity.2021Q4, 1))
equity_bought$PChange_SII <- with(equity_bought, ifelse(equity_bought$SII_amount.2021Q4 != 0, equity_bought$Change_SII/equity_bought$SII_amount.2021Q4, 1))
equity_bought$PChange_Unit_Price <- with(equity_bought, ifelse(equity_bought$Unit_SII_Price.2021Q4 != 0, (equity_bought$Unit_SII_Price.2022Q1-equity_bought$Unit_SII_Price.2021Q4)/equity_bought$Unit_SII_Price.2021Q4, 100))

equity_bought$Entity <- equity_bought$Cod_Entidade.2022Q1
equity_bought$Asset <- equity_bought$Asset_ID_pk.2022Q1
equity_bought$Title <- equity_bought$Title.2022Q1
equity_bought$Portfolio <- equity_bought$Portfolio.2022Q1
equity_bought$Unit_Linked <- equity_bought$UL.2022Q1
equity_bought$Fund <- equity_bought$Fund.2022Q1
equity_bought$Sector_Code <- equity_bought$Issuer_Sector_Code.2022Q1
equity_bought$Country <- equity_bought$Issuer_country.2022Q1
equity_bought$Currency <- equity_bought$Currency.2022Q1
equity_bought$Sub_Category <- equity_bought$Asset_Sub_Category.2022Q1

equity_bought <- equity_bought[-c(1:6, 9:15, 17:21, 24:30)]
equity_bought <- equity_bought[, c(12:17, 1:11, 18:20)]

# ii) Resume table
equity_bought2 <- equity_bought
equity_bought2$Non_Life <- ifelse(equity_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
equity_bought2$Life_UL <- ifelse(equity_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
equity_bought2$Life_Not_UL <- ifelse(equity_bought2$Portfolio == 'Life [split applicable]' & equity_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
equity_bought2 <- equity_bought2[-c(2:6, 18:20)]
resume_equity_bought <- equity_bought2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_equity_bought <- resume_equity_bought[, c(1, 12:14, 2:11)]

# iii) Country table
equity_bought_country <- as.data.frame(tab1(equity_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
equity_bought_sector <- as.data.frame(tab1(equity_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(equity_bought, "~/Desktop/ASF/R Model/equity_bought.xlsx")







