#!/bin/bash
#TODO let the user choos input folder and ouptut file
#
#cat data/Results/MatrixS_mutualism_michaelis-menten_30000ticks_10sexualreproduction_replicateA.dat_genprop.txt | sed "s/\s\+/,/g"


foldername=../data/Results/

echo "">$foldername/result.csv

cat data/Results/MatrixS_mutualism_michaelis-menten_30000ticks_10sexualreproduction_replicateA.dat_genprop.txt |  sed "s/\s*//g" | awk 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($2 != ""){varname=$1","varname; }}END{print varname "mutualism,ticks,sexproba,rep"}' > "$foldername"/concatenate_result.csv

#for mutualism in michaelis-menten linear sigmoid;
for mutualism in michaelis-menten sigmoid;
do
    #for rep in A B C D E;
    for rep in A;
    do
	for ticks in `seq 10000 5000 100000`;
	do
	    for sexprob in 1 5 10 50 100; 
	    do
		filename=MatrixS_mutualism_"$mutualism"_"$ticks"ticks_"$sexprob"sexualreproduction_replicate"$rep".dat_genprop.txt
		file="$foldername"/"$filename"
		if [ -f $file ];
		then
		    cat  $file | sed "s/\s*//g" | awk -v mut="$mutualism" -v ticks="$ticks" -v sp="$sexprob" -v rep="$rep" 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($2 != ""){varname=$1","varname; varvalue=$2","varvalue}}END{print varvalue mut","ticks","sp","rep}' >> "$foldername"/concatenate_result.csv
		fi
	    done
	done
    done
done
