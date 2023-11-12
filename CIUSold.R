# ciu_sold

# i) data cleaning
ciu_sold <- ciu[ciu$Quantity.2022Q1 < ciu$Quantity.2021Q4,]
ciu_sold$Change_Quantity <- ciu_sold$Quantity.2022Q1 - ciu_sold$Quantity.2021Q4
ciu_sold$Change_SII <- ciu_sold$SII_amount.2022Q1 - ciu_sold$SII_amount.2021Q4
ciu_sold$PChange_Quantity <- with(ciu_sold, ifelse(ciu_sold$Quantity.2021Q4 != 0, ciu_sold$Change_Quantity/ciu_sold$Quantity.2021Q4, 1))
ciu_sold$PChange_SII <- with(ciu_sold, ifelse(ciu_sold$SII_amount.2021Q4 != 0, ciu_sold$Change_SII/ciu_sold$SII_amount.2021Q4, 1))
ciu_sold$PChange_Unit_Price <- with(ciu_sold, ifelse(ciu_sold$Unit_SII_Price.2021Q4 != 0, (ciu_sold$Unit_SII_Price.2022Q1-ciu_sold$Unit_SII_Price.2021Q4)/ciu_sold$Unit_SII_Price.2021Q4, 100))

ciu_sold$Entity <- ciu_sold$Cod_Entidade.2021Q4
ciu_sold$Asset <- ciu_sold$Asset_ID_pk.2021Q4
ciu_sold$Title <- ciu_sold$Title.2021Q4
ciu_sold$Portfolio <- ciu_sold$Portfolio.2021Q4
ciu_sold$Unit_Linked <- ciu_sold$UL.2021Q4
ciu_sold$Fund <- ciu_sold$Fund.2021Q4
ciu_sold$Sector_Code <- ciu_sold$Issuer_Sector_Code.2021Q4
ciu_sold$Country <- ciu_sold$Issuer_country.2021Q4
ciu_sold$Currency <- ciu_sold$Currency.2021Q4
ciu_sold$Sub_Category <- ciu_sold$Asset_Sub_Category.2021Q4

ciu_sold <- ciu_sold[-c(1:6, 9:15, 18:22, 25:31)]
ciu_sold <- ciu_sold[, c(14:19, 1:2, 4:6, 8, 9:13, 20:22, 3, 7)]

# ii) Resume table
ciu_sold2 <- ciu_sold
ciu_sold2$Non_Life <- ifelse(ciu_sold2$Portfolio == 'Non-life [split applicable]', 1, 0)
ciu_sold2$Life_UL <- ifelse(ciu_sold2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
ciu_sold2$Life_Not_UL <- ifelse(ciu_sold2$Portfolio == 'Life [split applicable]' & ciu_sold2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
ciu_sold2 <- ciu_sold2[-c(2:6, 18:20)]
resume_ciu_sold <- ciu_sold2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_ciu_sold <- resume_ciu_sold[, c(1, 14:16, 2:13)]

# iii) Country table
ciu_sold_country <- as.data.frame(tab1(ciu_sold$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
ciu_sold_sector <- as.data.frame(tab1(ciu_sold$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(ciu_sold, "~/Desktop/ASF/R Model/ciu_sold.xlsx")






