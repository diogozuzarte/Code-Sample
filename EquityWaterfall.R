# Equity Bonds Waterfall

# create dataframe for the waterfall
Label <- c('Overall 2021Q4', 'Purchases', 'Sales', 'Kept')
Value <- c(sum(equity_2021q4$SII_amount), sum(equity_bought$Change_SII), sum(equity_sold$Change_SII), sum(equity_kept$Change_SII))
Waterfall_Equity <- data.frame(x = Label, y = Value)

waterfall(Waterfall_Equity, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)
sum(equity_2022q1$SII_amount) - sum(sum(equity_2021q4$SII_amount), sum(equity_bought$Change_SII), sum(equity_sold$Change_SII), sum(equity_kept$Change_SII))