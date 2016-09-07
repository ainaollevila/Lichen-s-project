egrep Qb mat3_Statistics.txt | cut -f 3 | sed 's/[[:space:]]//g'>mod.dat
egrep "Nestedness value" mat3_Statistics.txt | cut -f 3 | sed 's/[[:space:]]//g' >> mod.dat