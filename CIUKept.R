# ciu_kept

# i) data cleaning
ciu_kept <- ciu[ciu$Quantity.2022Q1 != 0 & ciu$Quantity.2021Q4 != 0,]
ciu_kept$Change_Quantity <- 0
ciu_kept$Change_SII <- ifelse(ciu_kept$Quantity.2022Q1==ciu_kept$Quantity.2021Q4, ciu_kept$SII_amount.2022Q1 - ciu_kept$SII_amount.2021Q4, 0)
ciu_kept$PChange_Quantity <- with(ciu_kept, ifelse(ciu_kept$Quantity.2021Q4 != 0, ciu_kept$Change_Quantity/ciu_kept$Quantity.2021Q4, 1))
ciu_kept$PChange_SII <- with(ciu_kept, ifelse(ciu_kept$SII_amount.2021Q4 != 0, ciu_kept$Change_SII/ciu_kept$SII_amount.2021Q4, 1))
ciu_kept$PChange_Unit_Price <- with(ciu_kept, ifelse(ciu_kept$Unit_SII_Price.2021Q4 != 0, (ciu_kept$Unit_SII_Price.2022Q1-ciu_kept$Unit_SII_Price.2021Q4)/ciu_kept$Unit_SII_Price.2021Q4, 100))

ciu_kept$Entity <- ciu_kept$Cod_Entidade.2022Q1
ciu_kept$Asset <- ciu_kept$Asset_ID_pk.2022Q1
ciu_kept$Title <- ciu_kept$Title.2022Q1
ciu_kept$Portfolio <- ciu_kept$Portfolio.2022Q1
ciu_kept$Unit_Linked <- ciu_kept$UL.2022Q1
ciu_kept$Fund <- ciu_kept$Fund.2022Q1
ciu_kept$Sector_Code <- ciu_kept$Issuer_Sector_Code.2022Q1
ciu_kept$Country <- ciu_kept$Issuer_country.2022Q1
ciu_kept$Currency <- ciu_kept$Currency.2022Q1
ciu_kept$Sub_Category <- ciu_kept$Asset_Sub_Category.2022Q1

ciu_kept <- ciu_kept[-c(1:6, 9:15, 18:22, 25:31)]
ciu_kept <- ciu_kept[, c(14:19, 1:2, 4:6, 8, 9:13, 20:22, 3, 7)]

# ii) Resume table
ciu_kept2 <- ciu_kept
ciu_kept2$Non_Life <- ifelse(ciu_kept2$Portfolio == 'Non-life [split applicable]', 1, 0)
ciu_kept2$Life_UL <- ifelse(ciu_kept2$Unit_Linked == 'Unit-linked or index-linked', 1, 0)
ciu_kept2$Life_Not_UL <- ifelse(ciu_kept2$Portfolio == 'Life [split applicable]' & ciu_kept2$Unit_Linked == 'Neither unit-linked nor index-linked', 1, 0)
ciu_kept2 <- ciu_kept2[-c(2:6, 18:20)]
resume_ciu_kept <- ciu_kept2 %>% group_by(Entity) %>% summarise(Quantity.2021Q4 = sum(Quantity.2021Q4), Unit_SII_Price.2021Q4 = mean(Unit_SII_Price.2021Q4), SII_amount.2021Q4 = sum(SII_amount.2021Q4), Quantity.2022Q1 = sum(Quantity.2022Q1), Unit_SII_Price.2022Q1 = mean(Unit_SII_Price.2022Q1), SII_amount.2022Q1 = sum(SII_amount.2022Q1), Change_Quantity = sum(Change_Quantity), Change_SII = sum(Change_SII), PChange_Quantity = sum(Change_Quantity)/sum(Quantity.2021Q4), PChange_SII = sum(Change_SII)/sum(SII_amount.2021Q4), Duration.2021Q4 = mean(Duration.2021Q4, na.rm = TRUE), Duration.2022Q1 = mean(Duration.2022Q1, na.rm = TRUE), Non_Life = sum(Non_Life), Life_UL = sum(Life_UL), Life_Not_UL = sum(Life_Not_UL)) %>% ungroup
resume_ciu_kept <- resume_ciu_kept[, c(1, 14:16, 2:13)]

# iii) Country table
ciu_kept_country <- as.data.frame(tab1(ciu_kept$Country, sort.group = "decreasing", cum.percent = FALSE))

# iv) Sector table
ciu_kept_sector <- as.data.frame(tab1(ciu_kept$Sector_Code, sort.group = "decreasing", cum.percent = FALSE))

# v) Export data frame to excel file
writexl::write_xlsx(ciu_kept, "~/Desktop/ASF/R Model/ciu_kept.xlsx")







