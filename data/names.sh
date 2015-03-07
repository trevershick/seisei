rm -f names.zip
rm -f NationalReadMe.pdf
rm -f yob*.txt
curl -o names.zip http://www.ssa.gov/oact/babynames/names.zip
unzip names.zip
cp yob2013.txt names.csv
rm yob*.txt
rm NationalReadMe.pdf
