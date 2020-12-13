# combine weekly data from each year
# created: december 13, 2020
# author: meas meng (@measrainsey)

# --------------------- inputs ----------------------

  data_path       = '/Users/MEAS/Google Drive/data/eia/weekly-coal-production'
  
# --------------------- outputs ----------------------
  
  processed_file  = 'weekly_coal_production_1984_2020.csv'
  
# --------------------- main ----------------------
  
# load packages -------
  
  library(data.table)
  library(readxl)

# load 1984-2000 data -------
  
  l_1984_2000 = lapply(lapply(file.path(data_path, paste0('weekprod', 1984:2000, 'tot.xls')), read_excel), as.data.table)
  l_1984_2000 = lapply(l_1984_2000, function(x){
    if (ncol(x) == 54) {
      setNames(x, nm = c('region', paste0('week_', 1:52), 'annual'))
    } else {
      setNames(x, nm = c('region', paste0('week_', 1:53), 'annual'))
    }
  })    
  
  years = 1984:2000
  list_years = list()
  for (i in seq_along(years)) {
    
    list_years[[i]] = data.table(year = rep(years[i], nrow(l_1984_2000[[i]])))
    
  }

  l_1984_2000 = Map(cbind, list_years, l_1984_2000)

  dt_1984_2000 = rbindlist(l_1984_2000, fill = T)
  dt_1984_2000 = dt_1984_2000[!is.na(region)]
  
# load 2001 data ---------
    
  dt_2001 = as.data.table(read_excel(file.path(data_path, paste0('weekprod', 2001, 'tot.xls')), skip = 1))
  dt_2001 = setNames(dt_2001, nm = c('region', paste0('week_', 1:53), 'annual'))
  dt_2001 = dt_2001[!is.na(region)]
  dt_2001[, year := 2001]
  setcolorder(dt_2001, c('year', c('region', paste0('week_', 1:53), 'annual')))
    
# load 2002-2012 data ---------
    
  l_2002_2012 = lapply(lapply(file.path(data_path, paste0('weekprod', 2002:2012, 'tot.xls')), read_excel, skip = 1), as.data.table)
  
  l_2002_2012 = lapply(l_2002_2012, function(x){
    x[, c('Q1 Total', 'Q2 Total', 'Q3 Total', 'Q4 Total') := NULL]
    x
  })    

  l_2002_2012 = lapply(l_2002_2012, function(x){
    
    if (ncol(x) == 56) {
      x = setNames(x, nm = c('region',
                             paste0('week_', 1:39), 'week_40_1', 'week_40_2',
                             paste0('week_', 41:53), 'annual'))
      x[, week_40 := week_40_1 + week_40_2]
      x[, c('week_40_1', 'week_40_2') := NULL]
      setcolorder(x, c('region', paste0('week_', 1:53), 'annual'))
    } else {
      if (ncol(x) == 57) {
        x = setNames(x, nm = c('region',
                               paste0('week_', 1:12), 'week_13_1', 'week_13_2',
                               paste0('week_', 14:25), 'week_26_1', 'week_26_2',
                               paste0('week_', 27:53), 'annual'))
        x[, week_13 := week_13_1 + week_13_2]
        x[, week_26 := week_26_1 + week_26_2]
        x[, c('week_13_1', 'week_13_2', 'week_26_1', 'week_26_2') := NULL]
        setcolorder(x, c('region', paste0('week_', 1:53), 'annual'))
      } else {
        if (ncol(x) == 58) {
          x = setNames(x, nm = c('region',
                                 paste0('week_', 1:13), 'week_14_1', 'week_14_2',
                                 paste0('week_', 15:26), 'week_27_1', 'week_27_2',
                                 paste0('week_', 28:39), 'week_40_1', 'week_40_2',
                                 paste0('week_', 41:53), 'annual'))
          x[, week_14 := week_14_1 + week_14_2]
          x[, week_27 := week_27_1 + week_27_2]
          x[, week_40 := week_40_1 + week_40_2]
          x[, c('week_14_1', 'week_14_2', 'week_27_1', 'week_27_2', 'week_40_1', 'week_40_2') := NULL]
          setcolorder(x, c('region', paste0('week_', 1:53), 'annual'))
        }
      }
    }
  })    
  
  years = 2002:2012
  list_years = list()
  for (i in seq_along(years)) {
    
    list_years[[i]] = data.table(year = rep(years[i], nrow(l_2002_2012[[i]])))
    
  }
  
  l_2002_2012 = Map(cbind, list_years, l_2002_2012)
  
  dt_2002_2012 = rbindlist(l_2002_2012, fill = T)
  dt_2002_2012 = dt_2002_2012[!is.na(region)]
    

# load 2013-2019 data ---------
    
  l_2013_2019 = lapply(lapply(file.path(data_path, paste0('weekprod', 2013:2019, 'tot.xls')), read_excel), as.data.table)
  l_2013_2019 = lapply(l_2013_2019, function(x){
    if (ncol(x) == 54) {
      setNames(x, nm = c('region', paste0('week_', 1:52), 'annual'))
    } else {
      setNames(x, nm = c('region', paste0('week_', 1:53), 'annual'))
    }
  })    
  
  years = 2013:2019
  list_years = list()
  for (i in seq_along(years)) {
    list_years[[i]] = data.table(year = rep(years[i], nrow(l_2013_2019[[i]])))
  }
  
  l_2013_2019 = Map(cbind, list_years, l_2013_2019)
  
  dt_2013_2019 = rbindlist(l_2013_2019, fill = T)
  dt_2013_2019 = dt_2013_2019[!is.na(region)]
  
# load 2020 data ---------
  
  dt_2020 = as.data.table(read_excel(file.path(data_path, paste0('weekprodforecast', 2020, 'tot.xls'))))
  dt_2020 = setNames(dt_2020, nm = c('region', paste0('week_', 1:49)))
  dt_2020 = dt_2020[!is.na(region)]
  dt_2020[, year := 2020]
  setcolorder(dt_2020, c('year', c('region', paste0('week_', 1:49))))

# combine all years ---------
  
  dt_all = rbindlist(list(dt_1984_2000, dt_2001, dt_2002_2012, dt_2013_2019, dt_2020), use.names = T, fill = T)
  
# melt from wide to long --------
  
  dt_long = melt(dt_all, 
                 id.vars = c('year', 'region'),
                 measure.vars = c(paste0('week_', 1:53), 'annual'),
                 variable.name = 'week',
                 value.name = 'production_tons')
  
# rename weeks ------
  
  dt_long[, week := gsub('week_', '', week)]
  
# convert production to numeric ------
  
  dt_long[, week := factor(week, levels = c(1:53, 'annual'))]
  dt_long[, production_tons := as.numeric(production_tons)]
  
# reorder rows -------
  
  setorder(dt_long, year, week, region)
  
# export to csv -----------
  
  fwrite(dt_long, here::here('data', processed_file), row.names = F)
  