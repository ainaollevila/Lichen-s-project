library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)
source(clustering.R) ##Load my Rscript (to use plotNetwork)


datafolder="../../data/cooc_mat/"

temp=read.csv(paste(datafolder,"mat_fulldataset.csv",sep=""), header=TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 

readMatrix<-function(filename){
    temp=read.csv(filename, header = TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 
    temp1=temp[,2:ncol(temp)]
    return(as.matrix(temp1))
}




computeForAllPop<-function(){
    #The idea would be to read each matrix for each pop, compute the different properties as done on the full matrix. 
    #Then in parallel check the spatial properties of the nodes (if the node include algae/fungus that are spread wide or not) 
    #And do some stat, ie : among our populations, nodes with Hight centrality are fungal nodes that cover big geographical distance 
    #(if that turns out to be true this would be peferct to show that sexual reproduction is used to interact on bigger distance with different genotypes of algae)

	for(popfile in list.files(datafolder,pattern="mat_pop*")){
	    print(popfile)
		 #if you don't do something to say that the first clumn of the file is the name of the lines you have a matrix with a first column full of too hig number.
		
		edgelist=web2edges(readMatrix(paste(datafolder,popfile,sep="")), webName=NULL, weight.column=TRUE, both.directions=TRUE,
		          is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
		          return=TRUE, verbose=TRUE)
		
		betweennes_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
		print(max(betweennes_netw[,2]))
		
		closeness_netw = closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)
		print(apply(closeness_netw[,2:3],2,max))
	}


}



temp1=readMatrix("../../data/cooc_mat/mat_fulldataset.csv") #if you don't do something to say that the first clumn of the file is the name of the lines you have a matrix with a first column full of too hig number.
#Cf difference between :
#image(temp) and image(temp1)
plotNetwork(temp1)


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
