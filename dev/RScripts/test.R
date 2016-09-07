source("utils.R")


data_model=read.csv("../../dev/data/mutualism_michaelis-menten_10000ticks_10sexualreproduction_replicateA.dat",header=F)
colnames(data_model)=c("A","F","x","y")

loadData()


wholeset=getNodesAndProp()

mat=cooccurenceModel(data_model)
wholesetModel=getNodesAndPropModel(mat,data_model)

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
