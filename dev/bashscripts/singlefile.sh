##Shhould be somehow merged with merge.sh 


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


for file in "$foldername"/*"$suffix".txt; 
do 
    echo $file
    if [ -f $file ];
    then
	cat  $file | sed "s/\s*//g" | awk 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($1 == "string"){exit}if($2 != ""){varname=$1","varname; varvalue=$2","varvalue}}END{print varvalue}' >> "$outputname"
    fi

done

