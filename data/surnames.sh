rm -f surnames.zip
curl -o surnames.zip http://www2.census.gov/topics/genealogy/2000surnames/names.zip
unzip surnames.zip
rm -f surnames.zip
rm -f app_c.xlsx
mv app_c.csv surnames.csv
