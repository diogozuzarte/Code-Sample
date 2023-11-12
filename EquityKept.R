# equity_kept

# i) data cleaning
equity_kept <- equity[equity$Quantity.2022Q1 != 0 & equity$Quantity.2021Q4 != 0,]
equity_kept$Change_Quantity <- 0
equity_kept$Change_SII <- ifelse(equity_kept$Quantity.2022Q1==equity_kept$Quantity.2021Q4, equity_kept$SII_amount.2022Q1 - equity_kept$SII_amount.2021Q4, 0)
equity_kept$PChange_Quantity <- with(equity_kept, ifelse(equity_kept$Quantity.2021Q4 != 0, equity_kept$Change_Quantity/equity_kept$Quantity.2021Q4, 1))
equity_kept$PChange_SII <- with(equity_kept, ifelse(equity_kept$SII_amount.2021Q4 != 0, equity_kept$Change_SII/equity_kept$SII_amount.2021Q4, 1))
equity_kept$PChange_Unit_Price <- with(equity_kept, ifelse(equity_kept$Unit_SII_Price.2021Q4 != 0, (equity_kept$Unit_SII_Price.2022Q1-equity_kept$Unit_SII_Price.2021Q4)/equity_kept$Unit_SII_Price.2021Q4, 100))

equity_kept$Entity <- equity_kept$Cod_Entidade.2022Q1
equity_kept$Asset <- equity_kept$Asset_ID_pk.2022Q1
equity_kept$Title <- equity_kept$Title.2022Q1
equity_kept$Portfolio <- equity_kept$Portfolio.2022Q1
equity_kept$Unit_Linked <- equity_kept$UL.2022Q1
equity_kept$Fund <- equity_kept$Fund.2022Q1
equity_kept$Sector_Code <- equity_kept$Issuer_Sector_Code.2022Q1
equity_kept$Country <- equity_kept$Issuer_country.2022Q1
equity_kept$Currency <- equity_kept$Currency.2022Q1
equity_kept$Sub_Category <- equity_kept$Asset_Sub_Category.2022Q1

equity_kept <- equity_kept[-c(1:6, 9:15, 17:21, 24:30)]
equity_kept <- equity_kept[, c(12:17, 1:11, 18:20)]

# ii) Resume table
equity_kept2 <- equity_kept
equity_kept2$Non_Life <- ifelse(equity_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
equity_kept2$Life_UL <- ifelse(equity_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
equity_kept2$Life_Not_UL <- ifelse(equity_kept2$Portfolio == 'Life [split applicable]' & equity_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
equity_kept2 <- equity_kept2[-c(2:6, 18:20)]
resume_equity_kept <- equity_kept2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_equity_kept <- resume_equity_kept[, c(1, 12:14, 2:11)]

# iii) Country table
equity_kept_country <- as.data.frame(tab1(equity_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
equity_kept_sector <- as.data.frame(tab1(equity_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(equity_kept, "~/Desktop/ASF/R Model/equity_kept.xlsx")







