##Shhould be somehow merged with merge.sh 


foldername=../../data/cooc_mat/Results/
outputname=$1
suffix=$2

echo "" >  $outputname 
for file in "$foldername"/*"$suffix".txt; 
do 
    echo $file
    if [ -f $file ];
    then
	cat  $file | sed "s/\s*//g" | awk 'BEGIN{FS=":"; varname="" ; varvalue=""}{if($1 == "string"){exit}if($2 != ""){varname=$1","varname; varvalue=$2","varvalue}}END{print varvalue}' >> "$outputname"
    fi

done

