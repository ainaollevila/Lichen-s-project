library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)

source("clustering.R") ##Load my Rscript (to use plotNetwork() and loadData()
options("scipen"=100, "digits"=4)##this is very import as it allow to avoid LOT of problems comming from the fact that will reading the 


#A simple function that should be avoid by using read.table and the right set of option (such as as.is and noloss
readMatrix<-function(filename){
    temp=read.csv(filename, header = TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 
    temp1=temp[,2:ncol(temp)]
    return(as.matrix(temp1))
}


getCorrespLichens <- function(rawdata,genotype,type,pop=""){
    print(genotype)
    print(type)
    print(pop)
    res=rawdata
    if(pop!=""){res=res[res$Population == pop,]}

    if(type=="A"){
	res=res[as.character(apply(res[,11:17],1,paste,collapse="")) == as.character(genotype),]
    }

    if(type=="F"){
	res=res[as.character(apply(res[,3:10],1,paste,collapse="")) == as.character(genotype),]
    }
    return(res)
}




computeForAllPop<-function(){
    #The idea would be to read each matrix for each pop, compute the different properties as done on the full matrix. 
    #Then in parallel check the spatial properties of the nodes (if the node include algae/fungus that are spread wide or not) 
    #And do some stat, ie : among our populations, nodes with Hight centrality are fungal nodes that cover big geographical distance 
    #(if that turns out to be true this would be peferct to show that sexual reproduction is used to interact on bigger distance with different genotypes of algae)

    ##If the file clustering.R is load, so all the data are in fullD and we can directly generate the matrices without passing through file write. I will store all the matrices in the variable allmatrices doing what follows:
    loadData() #to population fullD
    allmatrices=sapply(unique(fullD$Population),function(a){createNetwork(fullD[fullD$Population == a ,])})

    names(allmatrices)=unique(fullD$Population)#then just set the names in the list allmatrices as the population id of eahc pop

   #Now it's far more easier to compute the network properties and store it for every node
   #and create database
    wholeset=data.frame()
    sapply(names(allmatrices),function(ind){

	#   ind="15"
	   mat=allmatrices[[ind]]

	   fungusId=rownames(mat) 
	   algaeId=colnames(mat) 
	   coln=c("node_id","type","Population")
	   alg=cbind.data.frame(algaeId,"A",ind,stringsAsFactors=F)
	   colnames(alg)=coln
	   fung=cbind.data.frame(fungusId,"F",ind,stringsAsFactors=F)
	   colnames(fung)=coln
	   join=  rbind(alg,fung)
	   join$node_id=as.character(join$node_id)

	   web2edges(mat,is.one.mode=F,verbose=TRUE,both.directions=T)
	   edgelist=read.table("web.pairs",sep="\t")
	   idkeyTable=read.table("web-names.lut",sep="\t",header=T,as.is=2,numerals="no.loss")
	   ##The read table here is primordial. Without the no.loss (that somehowe avoid some cut when the integer is computed) lot of errors emerge. The as.is is here jsute to avoid factor and keep number as number.


	   
	   #compute different metrics
	   betweenness_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
	   closeness_netw = closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)


	   #compute matrix of spatial distance beetween every similar MLG
		      
	   spatial=apply(join,1,function(n){
			    gen=n[["node_id"]]
			    subst=getCorrespLichens(rawdata=fullD,pop=ind,type=n[["type"]],genotype=gen)
			    dist_mat=dist(subst[,c("x","y")])
			    c(mean(dist_mat), median(dist_mat),mad(dist_mat),min(dist_mat),max(dist_mat),sd(dist_mat))
	})
	   spatial=t(spatial)
	   colnames(spatial)=c("mean_dist","median_dist","mad","min_dist","max_dist","sd_dist")
	
	   join=cbind(join,spatial)
	   #norm_netw=ND(edgelist,normalised=T)
	   #....

	   #prepare the merging of those metrics
	   clsn_withid=merge(idkeyTable,closeness_netw,by.x="virtual",by.y="node")
	   btwn_withid=merge(idkeyTable,betweenness_netw,by.x="virtual",by.y="node")


	   #merge it
	   join=merge(join,btwn_withid[,c("real","betweenness")],by.x="node_id",by.y="real",all.x=T)
	   join=merge(join,clsn_withid[,c("real","closeness","n.closeness")],by.x="node_id",by.y="real",all.x=T)

	   wholeset<<-rbind(wholeset,join)
	})
	#save in a file just in case 
    write.csv(wholeset,"nodes_with_netmetrics.csv")

    wholeset[is.na(wholeset)]=0
    whola=wholeset[wholeset$type=="A",]
    wholf=wholeset[wholeset$type=="F",]
    plot(whola$closeness ~ whola$betweenness)
    points(wholf$closeness ~ wholf$betweenness,col="red")
    plot(whola$betweenness ~whola$mad,log= "x")
    points(wholf$betweenness ~wholf$mad,col="red")
    plot(whola$closeness ~whola$max_dist)
    points(wholf$closeness ~wholf$max_dist,col="red")
    rang=(wholeset$max_dist - wholeset$min_dist)
    ranga=(whola$max_dist - whola$min_dist)
    plot(whola$betweenness ~ranga,log="x")
    rangf=(wholf$max_dist - wholf$min_dist)
    points(wholf$betweenness ~ rangf,col="red" )
    

    plot(wholeset$mad[wholeset$mad>0] ~wholeset$closeness[wholeset$mad>0])
}



datafolder="../../data/cooc_mat/"

temp=read.csv(paste(datafolder,"mat_fulldataset.csv",sep=""), header=TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 

temp1=readMatrix("../../data/cooc_mat/mat_fulldataset.csv") #if you don't do something to say that the first clumn of the file is the name of the lines you have a matrix with a first column full of too hig number.
#Cf difference between :
#image(temp) and image(temp1)
plotNetwork(temp)


edgelist=web2edges(temp1, webName=NULL, weight.column=TRUE, both.directions=FALSE,
          is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
          return=TRUE, verbose=TRUE)

betweennes_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
#Betweenness_w() return stuff if I put both.direction=TRUE in the pprevious version
#write.table(betweennes_netw,"betweenness_scores.txt")

closeness_netw = closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)

#write.table(closeness_netw,"closeness_scores.txt")

hist(betweennes_netw[,2])




normalised_degree = ND(temp1, normalised=TRUE)
ND(temp1, normalised=TRUE)
write.table(normalised_degree,"normalised_degree.txt")

#these functions tardan mucho
BC(temp1,rescale = TRUE, cmode="undirected", weighted=TRUE)
CC(temp1, cmode="suminvundir", rescale=TRUE, weighted=TRUE)




computeModules(temp1,deep=FALSE,deleteOriginalFiles = TRUE,steps=1000000,tolerance=1e-10,experimental=FALSE)
BC_netw
sampledata <- rbind(
  c(1,2,1),
  c(1,3,5),
  c(2,1,1),
  c(2,4,6),
  c(3,1,5),
  c(3,4,10),
  c(4,2,6),
  c(4,3,10))
sampledata
betweenness_w(sampledata)

edgelist=web2edges(sampledata, webName=NULL, weight.column=TRUE, both.directions=FALSE,
                   is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
                   return=TRUE, verbose=TRUE)
