plotOneStuff <- function(){
	dev.off()
	par(mar=rep(1,4))
	plot(m29all$x[m29all$A != maxAlgae],m29all$y[m29all$A != maxAlgae],axes=F,pch=20,col="grey",xlab="lat",ylab="long",cex=2)
	box()
	points(m29all$x[m29all$A == maxAlgae],m29all$y[m29all$A == maxAlgae],col="dark red",pch=20,cex=2)
	text(m29all$x[m29all$A == maxAlgae],m29all$y[m29all$A == maxAlgae]+5,"A4",col="dark red")
	#points(m29all$x[m29all$A == sec],m29all$y[m29all$A == sec],col="blue",pch=20)
	#points(m29all$x[m29all$F == sec]+1,m29all$y[m29all$F == sec],col="blue",pch=20)
}

 unitTestPopSize<-function(y="betweenness", x="mad"){

	 dir.create("img/ModelVsData/")
	 allrep=c("A","B","C","D","E")
	 nrep="A"

	 x="mean_dist"
	 for(nsex  in  c(1,5,10,100)){
		 for(tick  in  c(10000,100000,50000)){
		     nsex=100
		     tick=10000
			 allModel=data.frame()
			 for(nrep  in  allrep){
				 dat=read.csv(paste("../../dev/data/ECHOresults/mutualism_michaelis-menten_",tick,"ticks_",nsex,"sexualreproduction_replicate",nrep,".dat",sep=""),header=F)
				 colnames(dat)=c("A","F","x","y")
				 showDistanceProp(dat) #print NA 'cause there is initially no pop in salva's output
				 dat=splitSpace(dat)
				 dat$y = dat$y*10 #This to se a scale more close to real scale (1unit model ~ 35m in reality
				 dat$x = dat$x*10 


				 showDistanceProp(dat) #This Ho!Magic! we have stuff cause we splitted in different pop.

				 #matMod=cooccurenceMat(dat)

				 #wholesetModel=getNodesAndProp(matMod,dat) #this should not be used as this time the idea is to get node and properties for all matrices of all pop: use computeAllProp

				 todoModel=computeAllPop(dat)
				 allModel=rbind(allModel,todoModel)
			 }
			 png(paste("img/ModelVsData/",x,"VS",y,"-ticks=",tick,"_sp=",nsex,".png",sep=""),width=600)
			 par(mfrow=c(1,2),mar=c(5,4,4,.5))
			 plotProperties(todoB,x,y,ma="Dal Grande 2012",log="x")
			 mar=par()$mar

			allModelB10000=allModel[allModel$betweenness>1,]
			 par(mar=c(5,2,4,1))
			 plotProperties(allModelB10000,x,y,main=paste("Model with:\nsexprob=",nsex,", ticks=",tick,sep=""),log="x")
			 par(mar=mar)
			 dev.off()
		 }
	 }


 }


plotOneModel <- function(){

    par(mar=rep(1,4))
    data_model1=read.csv("../../dev/data/ECHOresults/mutualism_michaelis-menten_10000ticks_10sexualreproduction_replicateA.dat",header=F)
    colnames(data_model1)=c("A","F","x","y")
    plot(data_model1$x,data_model1$y,axes=F,xlab="",ylab="")
    box()
}

plotAll <- function(allmatrice){
    sapply(names(allmatrice),function(a){
	   #write.csv(test,paste("../../data/cooc_mat/mat_pop-",a,".csv",sep=""))
	   png(paste("pop-",a,".png",sep=""))
	   par(mar=rep(0.1,4))
	   plotNetwork(allmatrice[[a]])
	   dev.off()
})
}



par(mar=rep(.5,4))
aa=read.csv("../data/ECHOresults/mutualism_michaelis-menten_100000ticks_1sexualreproduction_replicateA.dat",header=F)    
pdf("split.pdf")
 plot(aa$V3,aa$V4,axes=F,ylab= "",xlab="")
splitGraph()             
box()
dev.off()

 plotAllPop <- function(dat){
	par(mar=rep(.5,4))
	 npop=round(sqrt(length(unique(dat$Population))))
	 par(mfrow=c(npop,npop),mar=rep(.1,4),oma=c(2,2,5,2))
	 sapply(unique(dat$Population),function(pop){plot(dat[dat$Population == pop,c("x","y")],axes=F);box();})
	 #mtext("Spatial Disperstion of Sample\n within each pop (real Data)",side=3,outer=T,line=2) 
	 par(resetPar())
 }

