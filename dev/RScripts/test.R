 source("utils.R")
 source("AsymmetryValuesFunction.R")
 

unitTestPopSize<-function(){
    allrep=c("A","B","C","D","E")
    allModel=data.frame()
    for(nrep  in  allrep){
	data_model1=read.csv(paste("../../dev/data/ECHOresults/mutualism_michaelis-menten_100000ticks_1sexualreproduction_replicate",nrep,".dat",sep=""),header=F)
	colnames(data_model1)=c("A","F","x","y")
	showDistanceProp(data_model1) #print NA 'cause there is initially no pop in salva's output
	data_model1=splitSpace(data_model1)
	data_model1$y = data_model1$y * 35 #This to se a scale more close to real scale (1unit model ~ 35m in reality
	data_model1$x = data_model1$x * 35


	showDistanceProp(data_model1) #This Ho!Magic! we have stuff cause we splitted in different pop.

	#matMod=cooccurenceMat(data_model1)

	#wholesetModel=getNodesAndProp(matMod,data_model1) #this should not be used as this time the idea is to get node and properties for all matrices of all pop: use computeAllProp

	todoModel=computeAllPop(data_model1)
	allModel=rbind(allModel,todoModel)
    }
    plotProperties(allModel)
    compareDataset(todo,allModel,y="strength")


}


testNestAndCompares <- function(){

    testNestednesStat=read.csv("../data/Results/concatenate_result_Statistics.csv")
    testNestednesGen=read.csv("../data/Results/concatenate_result_genprop.csv")
    testReal=read.csv("../bashscripts/test.csv",head=F)
    colnames(testReal)=colnames(testNestednesGen)[1:14]
    test=testNestednesGen[testNestednesGen$ticks == 10001, ]
    boxplot(testReal$NODF.Nestednessvalue. ~ test$sexproba,xlab="sexproba",ylab="Nestednessvalue")

    plot(testNestednesGen$NODF.Nestednessvalue. ~ testNestednesGen$ticks)

    testOnfile=read.csv("../data/ECHOresults/mutualism_michaelis-menten_10000ticks_1sexualreproduction_replicateA.dat",header=F)
    testOnfile=read.csv("../data/ECHOresults/mutualism_michaelis-menten_10000ticks_5sexualreproduction_replicateA.dat",header=F)
    testOnfile=read.csv("../data/ECHOresults/mutualism_michaelis-menten_10000ticks_10sexualreproduction_replicateA.dat",header=F)
    colnames(testOnfile)=c("A","F","x","y")
    testNestednesGen[3,]
    length(unique(testOnfile$A))
    length(unique(testOnfile$F))
}



checkSpecies<-function(){
    comp=c()
    rown=181
    for( rown in 1:10){
	this=testNestednesGen[rown,]
	testOnfile=read.csv(paste("../data/ECHOresults/mutualism_michaelis-menten_",this$ticks,"ticks_",this$sexproba,"sexualreproduction_replicate",this$rep,".dat",sep=""),header=F)
	colnames(testOnfile)=c("A","F","x","y")
	bicheck=cooccurenceMat(testOnfile)
	nspecirow=length(unique(testOnfile$A))
	nspecicol=length(unique(testOnfile$F))
	testMod=computeModules(bicheck)
	testNest=nested(bicheck,method="NODF",rescale=F,normalised=T)
	testNestV=nestednodf(bicheck,weighted=T)

	comp=rbind(comp,c(nspecicol,this$Numberofcolumnspecies,nrow(bicheck),nspecirow,this$Numberofrowspecies,ncol(bicheck),nrow(attr(testMod,"modules")),attr(testMod,"likelihood"),this[,c(5,6)]))

    }

}


unitTestDalGrande2014<-function(){

    taiw=read.csv("../../data/DalGrande_et_al_New_Phytol.csv")
    made=read.csv("../../data/DalGrande_et_al_New_PhytolM.csv")
    taiw=cbind(taiw,"Taiwan") 
    made=cbind(made,"Madera") 
    colnames(taiw)[ncol(taiw)]="Population"
    colnames(made)[ncol(made)]="Population"
    alldat=rbind(taiw,made)
    alldat$A=apply(alldat[,3:9],1,paste,collapse="")
    alldat$F=as.character(alldat$Host_species)
    wholepop=cooccurenceMat(alldat)
    plotNetwork(wholepop)
    dosmat=loadAllMatrices(alldat)
    plotNetwork(dosmat[["Taiwan"]])
#    taiwProp=getNodesAndProp(dosmat[["Taiwan"]],alldat[alldat$Population == "Taiwan",])
# this isn't working as there is not x-y position in the 2014's dataset
}

unitTestDalGrande2012<-function(){

    loadData()
    wholepop=cooccurenceMat(fullD)
    plotNetwork(wholepop)
    allmatrice=loadAllMatrices(fullD)
    plotNetwork(allmatrice[["2"]])
    plotNetwork(allmatrice[["62"]])
    m1=getNodesAndProp(allmatrice[["2"]],fullD[fullD$Population == "2",])
    plotProperties(m1,log="x")
    m2=getNodesAndProp(wholepop,fullD)
    plotProperties(m2,log="xy")
    todo=computeAllPop(fullD)
    compareDataset(m2,todo)
    

}

unitTestWid2012<-function(){

    loadData()
    wholepop=cooccurenceMat(fullW)
    plotNetwork(wholepop)
    allmatrice=loadAllMatrices(fullW)
    plotNetwork(allmatrice[["A2"]])
    plotNetwork(allmatrice[["MOL"]])
    #m3=getNodesAndProp(allmatrice[["A2"]],fullW[fullW$Population == "A2",])
    #impossible car pas de x-y position des sample (mais long lat des pop oui)
    #plotProperties(m3,log="x")
    #m4=getNodesAndProp(wholepop,fullW)
    #plotProperties(m4,log="xy")

}

unitTestModel<-function(){

    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_michaelis-menten_10000ticks_10sexualreproduction_replicateA.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    matMod=cooccurenceMat(data_model1)
    wholesetModel=getNodesAndProp(matMod,data_model1)

    plotNetwork(matMod)
    allmatrice=loadAllMatrices(data_model1)
    m3=getNodesAndProp(matMod,data_model1)
    plotProperties(m3,log="x")

    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_sigmoid_10000ticks_10sexualreproduction_replicateE.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    data_model1$x=data_model1$x * 10
    data_model1$y=data_model1$y * 10
    matMod2=cooccurenceMat(data_model1)
    m4=getNodesAndProp(matMod,data_model1)
    plotProperties(m4,log="x")
    compareDataset(m3,m4)

    compareDataset(m3,m4)

    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_michaelis-menten_10000ticks_100sexualreproduction_replicateA.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    matMod=cooccurenceMat(data_model1)
    m3b=getNodesAndProp(matMod,data_model1)
    compareDataset(m3,m3b)

    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_sigmoid_100000ticks_100sexualreproduction_replicateA.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    matMod=cooccurenceMat(data_model1)
    m4b=getNodesAndProp(matMod,data_model1)
    compareDataset(m4,m4b)

    compareDataset(m3,m2)

}


    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_linear_45000ticks_1000sexualreproduction_replicateA.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    matMod=cooccurenceMat(data_model1)
    m3b=getNodesAndProp(matMod,data_model1)
    compareDataset(m3b,m2)
#

oldstuf <- function(){
    ###I get the raw data from supplementary material of Widmer et al 2012 thanks to this website : https://pdftables.com/ (it was free during the summer, I put all table of SI in data.

    #Get the raw data
    #rawdata=dal
    #get a subset
    rownames(rawdata)=rawdata[,3]
    rawdata=rawdata[,]

    #separate th microsat
    fungus=rawdata[,3:10]
    algae=rawdata[,11:17]
    uf=unique(fungus)
    ua=unique(algae)
    uaid=unique(apply(algae,1,paste,collapse=""))
    ufid=unique(apply(fungus,1,paste,collapse=""))
    usfid=apply(fungus,1,paste,collapse="")
    usaid=apply(algae,1,paste,collapse="")
    cooccurenceMat(rawdata,groupf=ufid,groupa=uaid)

    algae=t(sapply(unique(rawdata$Population),function(i){
		   apply(rawdata[rawdata$Population==i,3:11],2,mean)

}))

    fungus=t(sapply(unique(rawdata$Population),function(i){
		    apply(rawdata[rawdata$Population==i,12:17],2,mean)

}))

    fdistmat=as.matrix(round(dist(fungus),digit=-1))
    adistmat=as.matrix(round(dist(algae),digit=-1))
    radist=round(dist(algae),digit=-1)

    pdf("dist_mat.pdf")#,width=7,height=8)
    par(mfrow=c(2,1),mar=c(0,0,0,0))
    image(fdistmat)#,axis="n",main="fungus")
    image(radistmat)#,axis="n",,main="algae")
    dev.off()
    aramp=colorRampPalette(c("yellow","red"))(max(adistmat))
    framp=colorRampPalette(c("yellow","red"))(max(fdistmat))
    plot(rawdata$x,rawdata$y,col=aramp[adistmat[2,]],ylim=c(-1000,500),xlim=c(-2000,1000))
    points(rawdata$x,rawdata$y+10,col=framp[fdistmat[2,]],ylim=c(-1000,500),xlim=c(-2000,1000))

    png("dist_mat.png",width=2000,height=4000)
    dev.off()

    #compute distance and clust
    fungdist=computeDist(round(fungus))
    algdist=computeDist(round(algae))

    fungclust=hclust(fungdist)
    algclust= hclust(algdist)

    fungclust_dend=	as.dendrogram(fungclust)
    algclust_dend=	as.dendrogram(algclust)

    algclust_dendmod=color_branches(algclust_dend,k=100)
    fungclust_dendmod=color_branches(fungclust_dend,k=100)
    unttangdend=untangle_step_rotate_1side(algclust_dend,fungclust_dend)

    png("dalgrande.png",width=800,height=1000,pointsize=17)
    #tanglegram(unttangdend[[1]],unttangdend[[2]])
    tanglegram(algclust_dend,fungclust_dend,axes=F,main_left="Algae",main_right="Funghi")
    dev.off()
}



main<-function(){
    plotpop(fullD,2)
    a=15

    sapply(unique(fullD$Population),function(a){
	   test=createNetwork(fullD[fullD$Population == a ,])
	   write.csv(test,paste("../../data/cooc_mat/mat_pop-",a,".csv",sep=""))
	   png(paste("../../data/cooc_mat/bipartite_pop-",a,".png",sep=""),height=600,width=900)
	   plotNetwork(test,main=paste("../../data/cooc_mat/Population",a))
	   dev.off()
})

    fulltest=createNetwork(fullD)
    write.csv(fulltest,paste("../../data/cooc_mat/mat_fulldataset.csv",sep=""))
    plotNetwork(fulltest)
}
