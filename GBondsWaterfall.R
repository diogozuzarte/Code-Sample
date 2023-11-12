# Government Bonds Waterfall

# create dataframe for the waterfall
Label <- c('Overall 2021Q4', 'Purchases', 'Sales', 'Matured', 'Kept')
Value <- c(sum(govtbonds_2021q4$SII_amount), sum(govtbonds_bought$Change_SII), sum(govtbonds_sold$Change_SII), sum(govtbonds_matured$Change_SII), sum(govtbonds_kept$Change_SII))
Waterfall_GBonds <- data.frame(x = Label, y = Value)

waterfall(Waterfall_GBonds, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)
sum(govtbonds_2022q1$SII_amount) - sum(sum(govtbonds_2021q4$SII_amount), sum(govtbonds_bought$Change_SII), sum(govtbonds_sold$Change_SII), sum(govtbonds_matured$Change_SII), sum(govtbonds_kept$Change_SII))