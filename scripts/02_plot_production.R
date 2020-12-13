# plot coal production data
# created: december 13, 2020
# author: meas meng (@measrainsey)

# --------------------- inputs ----------------------

  data_file       = 'weekly_coal_production_1984_2020.csv'

# --------------------- outputs ----------------------

# --------------------- main ----------------------
  
# load packages -------
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)

# load data ---------
  
  dt_prod = fread(here::here('data', data_file), header = T)
  dt_prod = dt_prod[!is.na(production_tons)]
  dt_prod = dt_prod[region %in% c('U.S. Total', 'US Total') ]
  dt_prod = dt_prod[! week == 'annual']
  
# combine week 53 of previous with week 1 of current year ------
  
  half_weeks = dt_prod[(week == '53' & region %in% c('U.S. Total', 'US Total') & production_tons/1e6 < 10) | 
                         (week == '1' & region %in% c('U.S. Total', 'US Total') & production_tons/1e6 < 10)]
  
  half_weeks_full = dt_prod[(week == '53' & year %in% half_weeks[week == '53', year]) | 
                              (week == '1' & year %in% (half_weeks[week == '53', year] + 1)) | 
                              (week == '1' & year %in% half_weeks[week == '1', year])  | 
                              (week == '53' & year %in% (half_weeks[week == '1', year] - 1)) ]
  half_weeks_full[week == '53', new_year := year + 1]
  half_weeks_full[week == '53', new_week := '1']
  half_weeks_full[week == '1', new_year := year]
  half_weeks_full[week == '1', new_week := '1']
  
# remove rows with half weeks ------
  
  dt_prod_2 = dt_prod[!half_weeks_full, on = .(year, region, week)]
  dt_prod_2[, new_year := year]
  dt_prod_2[, new_week := week]
  
  dt_prod_3 = rbindlist(list(dt_prod_2, half_weeks), use.names = T, fill = T)
  
# recalculate production ------
  
  dt_prod_new = dt_prod_3[, .(production_tons = sum(production_tons, na.rm = T)),
                          by = .(new_year, new_week, region)]
  setnames(dt_prod_new, 'new_year', 'year')
  setnames(dt_prod_new, 'new_week', 'week')
  
# get historic weekly us total ------
  
  dt_hist = dt_prod_new[region %in% c('U.S. Total', 'US Total') & ! week == 'annual' & year < 2020]
  
# get 2020 weekly us total -------
  
  dt_2020 = dt_prod_new[region %in% c('U.S. Total', 'US Total') & ! week == 'annual' & year == 2020]
  
# get min and max for each historic date -----
  
  stat_week = dt_hist[, .(min_prod = min(production_tons, na.rm = T),
                          median_prod = median(production_tons, na.rm = T),
                          max_prod = max(production_tons, na.rm = T)), by = .(week)]

# ------------------------- plot ------------------------- 
  
  # theme and color palettes -----------
  
    theme_line = theme_ipsum(base_family = 'Secca Soft',
                             grid = 'X', 
                             plot_title_size = 20, 
                             subtitle_size = 18,
                             axis_title_just = 'center',
                             axis_title_size = 18, 
                             axis_text_size = 16,
                             strip_text_size = 16)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 11, color = '#5c5c5c', face = 'plain'),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.25, "cm"),
            axis.text.x = element_text(hjust = 0.5),
            axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
            plot.margin = unit(c(1,2,1,1), "lines"),
            legend.text = element_text(size = 16),
            legend.position = 'bottom',
            panel.grid.major.x = element_line(color = '#b8b8b8', linetype = 2, size = 0.3))
  
  # segment: coal ---------
  
    fig_coal = ggplot() +
      geom_segment(data = stat_week, 
                   aes(x = as.numeric(week), xend = as.numeric(week),  y = min_prod/1e6, yend = max_prod/1e6), color = '#f0dfb2', alpha = 0.7, size = 10) + 
      geom_line(data = stat_week, aes(x = as.numeric(week), y = median_prod/1e6), color = '#cfa255', size = 1) + 
      geom_line(data = dt_2020[week < 40], aes(x = as.numeric(week), y = production_tons/1e6), color = '#00526d', size = 1) + 
      labs(title = 'U.S. weekly coal production',
           subtitle = 'Million tons',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(1, 52, 1), limits = c(1, 52), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 25), expand = c(0, 0)) +
      theme_line
    fig_coal
    