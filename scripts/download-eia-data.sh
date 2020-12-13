#!/bin/bash
cd '/Users/MEAS/Google Drive/data/eia/weekly-coal-production'

for i in $(seq -w 2015 2020)
    do wget -nc 'https://www.eia.gov/coal/production/weekly/current_year/weekprod'$i'tot.xls'
    done
    
for i in $(seq -w 1984 2014)
    do wget -nc 'https://www.eia.gov/coal/production/weekly/archive/weekprod'$i'tot.xls'
    done