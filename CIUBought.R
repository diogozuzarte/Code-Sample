# ciu_bought

# i) data cleaning
ciu_bought <- ciu[ciu$Quantity.2022Q1 > ciu$Quantity.2021Q4,]
ciu_bought$Change_Quantity <- ciu_bought$Quantity.2022Q1 - ciu_bought$Quantity.2021Q4
ciu_bought$Change_SII <- ciu_bought$SII_amount.2022Q1 - ciu_bought$SII_amount.2021Q4
ciu_bought$PChange_Quantity <- with(ciu_bought, ifelse(ciu_bought$Quantity.2021Q4 != 0, ciu_bought$Change_Quantity/ciu_bought$Quantity.2021Q4, 1))
ciu_bought$PChange_SII <- with(ciu_bought, ifelse(ciu_bought$SII_amount.2021Q4 != 0, ciu_bought$Change_SII/ciu_bought$SII_amount.2021Q4, 1))
ciu_bought$PChange_Unit_Price <- with(ciu_bought, ifelse(ciu_bought$Unit_SII_Price.2021Q4 != 0, (ciu_bought$Unit_SII_Price.2022Q1-ciu_bought$Unit_SII_Price.2021Q4)/ciu_bought$Unit_SII_Price.2021Q4, 100))

ciu_bought$Entity <- ciu_bought$Cod_Entidade.2022Q1
ciu_bought$Asset <- ciu_bought$Asset_ID_pk.2022Q1
ciu_bought$Title <- ciu_bought$Title.2022Q1
ciu_bought$Portfolio <- ciu_bought$Portfolio.2022Q1
ciu_bought$Unit_Linked <- ciu_bought$UL.2022Q1
ciu_bought$Fund <- ciu_bought$Fund.2022Q1
ciu_bought$Sector_Code <- ciu_bought$Issuer_Sector_Code.2022Q1
ciu_bought$Country <- ciu_bought$Issuer_country.2022Q1
ciu_bought$Currency <- ciu_bought$Currency.2022Q1
ciu_bought$Sub_Category <- ciu_bought$Asset_Sub_Category.2022Q1

ciu_bought <- ciu_bought[-c(1:6, 9:15, 18:22, 25:31)]
ciu_bought <- ciu_bought[, c(14:19, 1:2, 4:6, 8, 9:13, 20:22, 3, 7)]

# ii) Resume table
ciu_bought2 <- ciu_bought
ciu_bought2$Non_Life <- ifelse(ciu_bought2$Portfolio == 'Non-life [split applicable]', 1, 0)
ciu_bought2$Life_UL <- ifelse(ciu_bought2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
ciu_bought2$Life_Not_UL <- ifelse(ciu_bought2$Portfolio == 'Life [split applicable]' & ciu_bought2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
ciu_bought2 <- ciu_bought2[-c(2:6, 18:20)]
resume_ciu_bought <- ciu_bought2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_ciu_bought <- resume_ciu_bought[, c(1, 14:16, 2:13)]

# iii) Country table
ciu_bought_country <- as.data.frame(tab1(ciu_bought$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
ciu_bought_sector <- as.data.frame(tab1(ciu_bought$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(ciu_bought, "~/Desktop/ASF/R Model/ciu_bought.xlsx")







