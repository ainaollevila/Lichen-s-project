#!/bin/bash
#TODO let the user choose input folder and ouptut file
#
#cat data/Results/MatrixS_mutualism_michaelis-menten_30000ticks_10sexualreproduction_replicateA.dat_genprop.txt | sed "s/\s\+/,/g"

foldername=$1
suffix=$2

if [[ $foldername == "" ]];
then
    echo "please add the folder name where statfile are stored"
    echo "usage : ./merge foldername suffix"
    echo " 	where suffix is 'genprop' or 'Statistics' "
    exit 0
fi

if [[ $suffix != "genprop" && $suffix != "Statistics"  ]];
then
    echo "usage : ./merge foldername suffix"
    echo " 	where suffix is 'genprop' or 'Statistics' "
    exit 0
fi

outputname="$foldername"/concatenate_result_"$suffix".csv


echo "">$outputname

for file in "$foldername"/*;  #this just to get the head, so to run only on the first file
do
    cat "$file" |  sed "s/\s*//g" | awk 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($2 != ""){varname=$1","varname; }}END{print varname "mutualism,ticks,sexproba,rep"}' > "$outputname"
    break 1
done

for mutualism in michaelis-menten linear sigmoid;
do
    for rep in A B C D E;
    do
	for ticks in `seq 10000 5000 100000`;
	do
	    for sexprob in 1 5 10 50 100; 
	    do
		filename=MatrixS_mutualism_"$mutualism"_"$ticks"ticks_"$sexprob"sexualreproduction_replicate"$rep".dat_"$suffix".txt
		file="$foldername"/"$filename"
		if [ -f $file ];
		then
		    echo "Parsing file:$file"
		    cat  $file | sed "s/\s*//g" | awk -v mut="$mutualism" -v ticks="$ticks" -v sp="$sexprob" -v rep="$rep" 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($1 == "string"){exit}if($2 != ""){varname=$1","varname; varvalue=$2","varvalue}}END{print varvalue mut","ticks","sp","rep}' >> "$outputname"
		fi
	    done
	done
    done
done
