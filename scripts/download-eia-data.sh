#!/bin/bash
cd '/Users/MEAS/Google Drive/data/eia/weekly-coal-production'

# for 2020, get original estimates:
    wget -nc 'https://www.eia.gov/coal/production/weekly/current_year/weekprodforecast2020tot.xls'

# for all historic years, get revised estimates:
    for i in $(seq -w 2015 2019)
        do wget -nc 'https://www.eia.gov/coal/production/weekly/current_year/weekprod'$i'tot.xls'
        done

    for i in $(seq -w 1984 2014)
        do wget -nc 'https://www.eia.gov/coal/production/weekly/archive/weekprod'$i'tot.xls'
        done