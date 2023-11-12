# ----------------------------------------------------------------------------------- #
#     
#     Departamento de Análise de Riscos e Solvência  
#     ASF - Autoridade de Supervisão de Seguros e Fundos de Pensões
#
#     Diogo Salvador Zuzarte
#     Julho 2022
#
#     Código com o objetivo de identificar os ativos que são comprados
#     e vendidos entre períodos de análise e de fazer a sua respetiva caracterização.
#     
#------------------------------------------------------------------------------------ #

# Set working directory
setwd("~/Desktop/ASF/R Model")

# Database cleaning script
source("CleaningScript.R")

## Creation of a Government Bonds' database
source("GBonds.R")
### Government Bonds Bought
source("GBondsBought.R")
### Government Bonds Sold
source("GBondsSold.R")
### Government Bonds Matured
source("GBondsMatured.R")
### Government Bonds Kept
source("GBondsKept.R")
#### Waterfall Graphic for Analysis
source("GBondsWaterfall.R")
waterfall(Waterfall_GBonds, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Corporate Bonds' database
source("CBonds.R")
### Corporate Bonds Bought
source("CBondsBought.R")
### Corporate Bonds Sold
source("CBondsSold.R")
### Corporate Bonds Matured
source("CBondsMatured.R")
### Corporate Bonds Kept
source("CBondsKept.R")
#### Waterfall Graphic for Analysis
source("CBondsWaterfall.R")
waterfall(Waterfall_CBonds, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Property and REIF' database
source("PREIF.R")
### Property and REF Bought
source("PREIFBought.R")
### Property and REF Sold
source("PREIFSold.R")
### Property and REF Kept
source("PREIFKept.R")
#### Waterfall Graphic for Analysis
source("PREIFWaterfall.R")
waterfall(Waterfall_PREIF, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Collective Investment Undertakings' database
source("CIU.R")
### Collective Investment Undertakings Bought
source("CIUBought.R")
### Collective Investment Undertakings Sold
source("CIUSold.R")
### Collective Investment Undertakings Kept
source("CIUKept.R")
#### Waterfall Graphic for Analysis
source("CIUWaterfall.R")
waterfall(Waterfall_CIU, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Deposits' database
source("Deposits.R")
### Deposits Bought
source("DepositsBought.R")
### Deposits Sold
source("DepositsSold.R")
### Deposits Kept
source("DepositsKept.R")
### Waterfall Graphic for Analysis
source("DepositsWaterfall.R")
waterfall(Waterfall_Deposits, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Equity' database
source("Equity.R")
### Equity Bought
source("EquityBought.R")
### Equity Sold
source("EquitySold.R")
### Equity Kept
source("EquityKept.R")
#### Waterfall Graphic for Analysis
source("EquityWaterfall.R")
waterfall(Waterfall_Equity, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)

## Creation of a Others' database
source("Other.R")
### Other Bought
source("OtherBought.R")
### Other Sold
source("OtherSold.R")
### Other Kept
source("OtherKept.R")
#### Waterfall Graphic for Analysis
source("OtherWaterfall.R")
waterfall(Waterfall_Other, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)
