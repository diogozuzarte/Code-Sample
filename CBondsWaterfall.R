# Corporate Bonds Waterfall

# create dataframe for the waterfall
Label <- c('Overall 2021Q4', 'Purchases', 'Sales', 'Matured', 'Kept')
Value <- c(sum(cbonds_2021q4$SII_amount), sum(cbonds_bought$Change_SII), sum(cbonds_sold$Change_SII), sum(cbonds_matured$Change_SII), sum(cbonds_kept$Change_SII))
Waterfall_CBonds <- data.frame(x = Label, y = Value)

waterfall(Waterfall_CBonds, calc_total = TRUE, total_axis_text = 'Overall 2022Q1', total_rect_color = '#CC9900', total_rect_text_color = '#000000', linetype = 1, fill_colours = c('#CC9900', '#006699', '#990000', '#990000', '#CCCCCC'), fill_by_sign = FALSE, draw_axis.x = FALSE)
sum(cbonds_2022q1$SII_amount) - sum(sum(cbonds_2021q4$SII_amount), sum(cbonds_bought$Change_SII), sum(cbonds_sold$Change_SII), sum(cbonds_matured$Change_SII), sum(cbonds_kept$Change_SII))