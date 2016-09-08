library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)
library(scales)

options("scipen"=100, "digits"=4)##this is very import as it allow to avoid LOT of problems comming from the fact that will reading the 

#Row = Fungus
#Col = Algae

#Global variables 
fullD=NULL
fullW=NULL



#Function to load and merge data from Widmer 2012 et Dal Grande 2012
#As those data can be useuful to check divers things and are used in various functions they are global. Should be change
loadData<-function(){
	print(paste("Load data"))
    rawdataW=read.csv("../../data/Widmer et al_2012.xlsx")
    locW=read.csv("../../data/loc_widmer2012.csv")

    fullW<<-merge(rawdataW,locW,by="Population")

    rawdataD=read.csv("../../data/DalGrande_et_al_2012.csv")
    locD=read.csv("../../data/loc_dalgrande2012.csv")
    fullD<<-merge(rawdataD,locD,by="Population")
	print(paste("done."))
}


#The 4 following function should be merged into two coherent function that could handle different kind of genopypes (list of microsatellites/characters/numbers...)


#return the list of lichens where is found the symbiont (algal or fungal symbtion, depending on type) with the MLG genotypes.
getCorrespLichensModel <- function(rawdata,genotype,type){
    res=rawdata

    if(type=="A"){
	res=res[as.character(res[,2]) == as.character(genotype),]
    }

    if(type=="F"){
	res=res[as.character(res[,1]) == as.character(genotype),]
    }
    return(res)
}

#same as befor but for result of the model 
getCorrespLichens <- function(rawdata,genotype,type,pop=""){
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



#from the list of lichens create bipartite matrix of intereaction between algal and fungal symbionts
cooccurenceMat <- function(datas,groupf=c(),groupa=c()){

    res=matrix(0,nrow=length(groupf),ncol=length(groupa),dimnames=list(groupf,groupa))

    for(i in 1:nrow(datas)){
	popiMlgF=paste(datas[i,3:10],collapse="")
	popiMlgA=paste(datas[i,11:17],collapse="")
	res[popiMlgF,popiMlgA]=res[popiMlgF,popiMlgA]+1
    }
    return(res)



}
##same as before for the result of the model
cooccurenceModel <- function(datas){
    res=matrix(0,nrow=length(unique(datas$A)),ncol=length(unique(datas$F)))

    rownames(res)=unique(datas$A)
    colnames(res)=unique(datas$F)

    for(i in 1:nrow(datas)){
	popiMlgF=as.character(datas[i,1])
	popiMlgA=as.character(datas[i,2])
	res[popiMlgF,popiMlgA]=res[popiMlgF,popiMlgA]+1
    }
    return(res)

}



getNodesAndProp<-function(){
    #The idea would be to read each matrix for each pop, compute the different properties as done on the full matrix. 
    #Then in parallel check the spatial properties of the nodes (if the node include algae/fungus that are spread wide or not) 
    #And do some stat, ie : among our populations, nodes with Hight centrality are fungal nodes that cover big geographical distance 
    #(if that turns out to be true this would be peferct to show that sexual reproduction is used to interact on bigger distance with different genotypes of algae)
    ##If the file clustering.R is load, so all the data are in fullD and we can directly generate the matrices without passing through file write. I will store all the matrices in the variable allmatrices doing what follows:
    loadData() #to population fullD
    allmatrices=loadAllMatrices()

   #Now it's far more easier to compute the network properties and store it for every node
   #and create database

	print(paste("Calculate metrics for all pop"))
    wholeset=data.frame()
    sapply(names(allmatrices),function(ind){

	print(paste("   Metrics in red pop:",ind))
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

	   web2edges(mat,is.one.mode=F,verbose=F,both.directions=T)
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
	print(paste("done."))
	#save in a file just in case 
    #write.csv(wholeset,"nodes_with_netmetrics.csv")
    return(wholeset)
}

getNodesAndPropModel<-function(mat,rawdatas){
	colnames(rawdatas)=c("A","F","x","y") #put that in the model output
	result=data.frame()
	fungusId=rownames(mat) 
	algaeId=colnames(mat) 
	coln=c("node_id","type")
	alg=cbind.data.frame(algaeId,"A",stringsAsFactors=F)
	colnames(alg)=coln
	fung=cbind.data.frame(fungusId,"F",stringsAsFactors=F)
	colnames(fung)=coln
	join=rbind(alg,fung)
	join$node_id=as.character(join$node_id)

	web2edges(mat,is.one.mode=F,verbose=F,both.directions=T)
	edgelist=read.table("web.pairs",sep="\t")
	idkeyTable=read.table("web-names.lut",sep="\t",header=T,as.is=2,numerals="no.loss")
	##The read table here is primordial. Without the no.loss (that somehowe avoid some cut when the integer is computed) lot of errors emerge. The as.is is here jsute to avoid factor and keep number as number.

	#compute different metrics
	betweenness_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
	closeness_netw = closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)


	#compute matrix of spatial distance beetween every similar MLG
	spatial=apply(join,1,function(n){
		      gen=n[["node_id"]]
		      subst=getCorrespLichensModel(rawdata=data_model,type=n[["type"]],genotype=gen)
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

	result<-rbind(result,join)
	return(result)
}

##Function that return a list with all matrices corresponding to the bipartite network of all populations. The indices of the list are the names of the populations
loadAllMatrices<-function(){
    print(paste("Compute all bipartite"))
    allmatrices<-sapply(unique(fullD$Population),function(a){createNetwork(fullD[fullD$Population == a ,])})
    names(allmatrices)<-unique(fullD$Population)#then just set the names in the list allmatrices as the population id of eahc pop
    print(paste("done."))
    return(allmatrices)
}

#This take a table with all node grouping different symbiont MLG and plot each MLG given the choose properties x and y (those has to be in the table, for sure
plotProperties<-function(datas="wholeset",x="mad",y="betweenness",...){

	if(!(x %in% colnames(datas))){ 
		errmess(x,colnames(datas))
	   return(NA)
	}
	if(!(y %in% colnames(datas))){ 
		errmess(y,colnames(datas))
	   return(NA)
	}
    datas[is.na(datas)]=0
    whola=datas[datas$type=="A",]
    wholf=datas[datas$type=="F",]
    plot(whola[,y] ~ whola[,x],xlab=x,ylab=y,pch=20,...)
    points(wholf[,y] ~ wholf[,x],pch=20,col="red")
    legend("topleft",legend=c("Algae","Fungus"),col=c(1,"red"),pch=c(20,20))
    
}

#simple error message
errmess <- function(a,d){
	print(paste("The metrics '",a,"' you try to plot is not in your dataset",sep=""))
   print(paste("Choose between: '",paste(d,collapse="' '"),"'",sep=""))
}

#plot two graph !warnings: suppose that result re stored in global variables
plotModelVsData<-function(x,y){
	print(paste(x,y))
    par(mfrow=c(1,2))
    plotProperties(wholeset,x,y,log="x",main="Data")
    plotProperties(wholesetModel,x,y,log="x",main="Model")
}




#Return the number of difference between two rows
simil=function(d){
    sum(apply(d,2,function(i){ if(diff(i) == 0) return(0) else return(1)}))
}	




computeDist=function(d){
    res=matrix(nrow=nrow(d),ncol=nrow(d))
    for(i in 1:(nrow(d)-1)){
	for(j in (i+1):(nrow(d))){
	    res[i,j]=simil(d[c(i,j),])
	    res[j,i]=simil(d[c(i,j),])
	}
    }
    return(as.dist(res))
}

#Function to separate the fungus and algae micro satellite, return a list of two table with the micros Sattellite for each
getSeparate <- function(rawdata){
    #separate the microsat
    seplist=list(rawdata[,3:11],rawdata[,12:17])
    names(seplist)=c("algae","fungus")
    return(seplist)
}

#Compute the matrix by checking if the difference is less thatn a certain value
cooccurenceMatTocheck <- function(datas,algdif=2,fungdif=2,groupf=c(),groupa=c()){
    if(length(groupa)==0){
	groupa=as.character(unique(datas$Sample))
	groupf=groupa
    }
    res=matrix(0,nrow=length(groupf),ncol=length(groupa),dimnames=list(groupf,groupa))
    print(res)

    for(i in 1:(nrow(datas)-1)){
	for(j in (i):(nrow(datas))){
	    #if(length(groupa)==0){
	    #	popa=as.character(datas[i,"Sample.ID"])
	    #	popb=as.character(datas[j,"Sample.ID"])
	    #}
	    #else{
	    #ifungusA=paste(datas[i,3:10],collapse="")
	    #ifungusB=paste(datas[j,3:10],collapse="")
	    #ialgaeA=paste(datas[i,11:17],collapse="")
	    #ialgaeB=paste(datas[j,11:17],collapse="")

	    popa=paste(datas[i,3:10],collapse="")
	    popb=paste(datas[j,11:17],collapse="")
	    #}


	    print(paste(i,j))
	    print(paste(popa,popb))
	    vfungusA=datas[i,3:10]
	    vfungusB=datas[j,3:10]
	    if(simil(rbind(vfungusB,vfungusA))<fungdif){
		res[popa,popb]=res[popa,popb]+1
		#res[popb,popa]=res[popb,popa]+1
	    }

	    valgaeA=datas[i,11:17]
	    valgaeB=datas[j,11:17]
	    if(simil(rbind(valgaeB,valgaeA))<algdif){
		res[popa,popb]=res[popa,popb]+1
		#res[popb,popa]=res[popb,popa]+1
	    }
	}
    }
    #res=list(resF,resA)
    #names(res)=c("fungus","algae")
    return(res)

}

###looks like garbage
#plot2mt <- function(fdistmat,adistmat,...){
#    par(mfrow=c(2,1),mar=c(0,0,0,0))
#    image(fdistmat)#,axis="n",main="fungus")
#    image(adistmat)#,axis="n",,main="algae")
#}
#
#writeMat <- function(test){
#    sapply(names(test),function(i){write.csv(test[[i]],paste(i,".csv",sep=""))})
#}
#
#
#writeAdj <- function(d){
#
#    res=c()
#    for(i in colnames(d)){
#	for(j in colnames(d)){
#	    print(paste(i,j))
#	    if(d[i,j] > 0 ){
#		res=rbind(res,cbind(i,j,d[i,j]))
#	    }
#
#	}
#    }
#    return(res)
#}
#

#Plot the bipartite network with size of link prop to the weight of the relation
#Some fixes need
plotNetwork<-function(mat,id="",...){
    fungus=1:nrow(mat)*4 
    algae=1:ncol(mat)*4 
    ptsize=3
    plot(fungus,rep(-1,nrow(mat)),cex=ptsize,bty="n",ylim=c(-1.5,1.5),xlim=c(-10,max(fungus)+5),col=alpha("red",0.5),pch=20,xaxt="n",xlab="",yaxt="n", ylab="",...)
    text(fungus,rep(-.7,nrow(mat))-.5,label=rownames(mat),cex=1,srt=60,c(1,1))
    points(algae,rep(1,ncol(mat)),cex=ptsize,col=alpha("yellow",0.5),pch=20)
    text(algae,rep(.7,ncol(mat))+.5,label=colnames(mat),cex=1,srt=300,adj=c(1,1))
    text(-5,-1,"fungi")
    text(-5,1,"algae")
    text(0,0,id,cex=1)
    cx0=c()
    cy0=c()
    cx1=c()
    cy1=c()
    if(nrow(mat)>0){
	for( f in 1:(nrow(mat))){
	    for(a in 1:(ncol(mat))){
		xp=algae[a]
		yp=1
		xf=fungus[f]
		yf=-1
		if(mat[f,a]>=1){
		    segments(x0=xf,y0=yf+.05,x1=xp,y1=yp-.05,col=alpha("green",.6),lwd=mat[f,a]/2,length=.1)
		}
	    }
	}
    }
}

#A function that should not exist that read a matrix and set colnames and rownames in the good way
readMat <- function(filename){
    m=read.csv(filename,header=T)
    rownames(m)=m[,1]
    m=m[,2:ncol(m)]
    return(m)
}


#plot all sample with the coordinate sof the population where the samples come from
plotSample<-function(data,...){
    colscale=topo.colors(length(unique(data$Population)))
    names(colscale)=unique(data$Population)
    plot(data$Longitude,data$Latitude,col=colscale[data$Population],...)

}



createNetwork<-function(rwdt){

    #separate th microsat
    fungus=rwdt[,3:10]
    algae=rwdt[,11:17]
    al=apply(algae,1,paste,collapse="")
    fu=apply(fungus,1,paste,collapse="")
    uaid=unique(apply(algae,1,paste,collapse=""))
    ufid=unique(apply(fungus,1,paste,collapse=""))
    res=cooccurenceMat(rwdt,groupf=ufid,groupa=uaid)
    return(res)

}

plotpop<-function(d,n,...){
    curpop=d[d$Population == n,]
    plot(curpop$x,curpop$y)
}



#A simple function that should be avoid by using read.table and the right set of option (such as as.is and noloss
readMatrix<-function(filename){
    temp=read.csv(filename, header = TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 
    temp1=temp[,2:ncol(temp)]
    return(as.matrix(temp1))
}

