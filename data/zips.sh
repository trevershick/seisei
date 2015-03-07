curl -o zips.zip ftp://ftp.census.gov/econ2012/CBP_CSV/zbp12totals.zip
unzip zips.zip
mv zbp12totals.txt zips.csv
cp zips.csv cities.csv
