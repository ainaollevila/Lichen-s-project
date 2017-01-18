source("utils.R")
source("AsymmetryValuesFunction.R")

#    allModel=data.frame()
#
#for(files in list.file(path="../data/ResultsCut/",pattern="*genprop.csv"){
#    for(nrep  in  allrep){
#nrep='B'
#	data_model1=read.csv(paste("../../dev/data/ECHOresults/mutualism_michaelis-menten_10000ticks_1sexualreproduction_replicate",nrep,".dat",sep=""),header=F)
#	colnames(data_model1)=c("A","F","x","y")
#	showDistanceProp(data_model1) #print NA 'cause there is initially no pop in salva's output
#	data_model1=splitSpace(data_model1)
#	data_model1$y = data_model1$y * 35 #This to se a scale more close to real scale (1unit model ~ 35m in reality
#	data_model1$x = data_model1$x * 35
#
#
#	showDistanceProp(data_model1) #This Ho!Magic! we have stuff cause we splitted in different pop.
#
#    matMod=cooccurenceMat(data_model1[data_model1$Population == 31,])
#
#	#wholesetModel=getNodesAndProp(matMod,data_model1) #this should not be used as this time the idea is to get node and properties for all matrices of all pop: use computeAllProp
#
#	todoModel=computeAllPop(data_model1)
#	allModel=rbind(allModel,todoModel)
#    }
#    plotProperties(allModel)
#}
#

printGraph<-function(){

	testModelPop=read.csv("../data/ResultsCut//concatenate_result_genprop.csv",header=F)
	testModelMut=read.csv("../data/Results/concatenate_result_genprop.csv",header=T)
	colnames(testModelPop)=c(colnames(testModelMut),"pop")

	testModelPar=read.csv("../data/ResultsParasitism/concatenate_result_genprop.csv",header=T)


	dalgrande2012=read.csv("../../data/cooc_mat/Results/concatenate_result_genprop.csv",head=F)
	colnames(dalgrande2012)=colnames(testModelMut)[1:14]

	modSP1subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 1,1:14 ]
	modSP1subpop$type= "Model Sp=001a"
	modSP5subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 5,1:14 ]
	modSP5subpop$type= "Model Sp=5"
	modSP10subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 10,1:14 ]
	modSP10subpop$type= "Model Sp=010a"
	modSP100subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 100,1:14 ]
	modSP100subpop$type= "Model Sp=100a"
	modSP1=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 1,1:14 ]
	modSP1$type= "Model Sp=001b"
	modSP100=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 100,1:14 ]
	modSP100$type= "Model Sp=100b"
	modSP10=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 10,1:14 ]
	modSP10$type= "Model Sp=010b"
	dalgrande2012$type= "DalGrande12"

	compare=rbind(dalgrande2012,modSP1,modSP1subpop,modSP5,modSP5subpop,modSP10,modSP10subpop)
	compare=rbind(dalgrande2012,modSP1subpop,modSP10subpop,modSP100subpop)
	compare=rbind(dalgrande2012,modSP1subpop,modSP10subpop,modSP100subpop,modSP1,modSP10,modSP100)

	par(mar=c(5,4,2,2))
	space=myseq(3,3,2,.5)
	meanspace=pos(space)
	pdf("img/compareAll/QbMetcompare.pdf",pointsize=14)
	par(mar=c(5,4,2,2))
	boxplot(compare$Qb.Standardmetric. ~ compare$type,ylab="Modularity",xlab="",main="",outline=F,ylim=c(0,1),col=c("white",rep(c("green","blue"),3)),boxwex=c(1,rep(.5,6)),at=c(1,space),xaxt="n",medlwd=.8,boxlwd=.5)
	axis(1,at=c(1,meanspace),labels=c("",1,10,100),cex.axis=.8,line=0)
	mtext("SRP(%)",1,2,at=meanspace[2],cex=.8)
	mtext("DalGrande2012",1,3.5,at=1)
	mtext("Simulations",1,3.5,at=meanspace[2])
	legend(meanspace[2],y=.4,fill=c("blue","green"),col=c("blue","green"),legend=c("Non-scaled","Scaled"),title="Model results",xjust=.5)
	dev.off()

	pdf("img/compareAll/NestedNODFcompare.pdf",pointsize=14)
	par(mar=c(5,4,2,2))
	boxplot(compare$N.Numberofmodules. ~ compare$type,ylab="Number of Node",xlab="",main="",outline=F,ylim=c(0,210),col=c("white",rep(c("green","blue"),3)),boxwex=c(1,rep(.5,6)),at=c(1,space),xaxt="n",medlwd=.8,boxlwd=.5)
	axis(1,at=c(1,meanspace),labels=c("",1,10,100))
	mtext("SRP(%)",1,2,at=meanspace[2],cex=.8)
	mtext("DalGrande2012",1,3.5,at=1)
	mtext("Simulations",1,3.5,at=meanspace[2])
	dev.off()

	png("img/compareAll/NestedNODFcompare.png",width=750)
	boxplot(compare$N.Numberofmodules. ~ compare$type,ylab="Number of Node",xlab="",main="Modularity",outline=F,ylim=c(0,210),at=c(1,myseq(1,3,.5)))
	dev.off()

	png("img/compareAll/QrRatiocompare.png",width=750)
	boxplot(compare$Qr.Ratioofint.extinter. ~ compare$type,ylab="Qr Ratio",xlab="",main="Qr Ratio")
	dev.off()

	png("img/compareAll/aNModuelcompare.png",width=750)
	boxplot(as.numeric(compare$N.Numberofmodules.) ~ compare$type,ylab="Number of Modules",xlab="",main="Number of Modules")
	dev.off()

	pdf("img/allPropOverTime/NumModuleOverTimeFULL.pdf",pointsize=14)
	par(mar=c(4,4,2,2))
	printOverTime(testModelMut,"N.Numberofmodules.")
	dev.off()
	pdf("img/allPropOverTime/NumModuleOverTimeRESC.pdf",pointsize=14)
	par(mar=c(4,4,2,2))
	printOverTime(testModelPop,"N.Numberofmodules.",grad=c("green","darkgreen"))
	dev.off()
	pdf("img/allPropOverTime/QbMetOverTimeFULL.pdf",pointsize=14)
	par(mar=c(4,4,2,2))
	printOverTime(testModelMut,"Qb.Standardmetric.")
	dev.off()
	pdf("img/allPropOverTime/QbMetOverTimeRESC.pdf",pointsize=14)
	par(mar=c(4,4,2,2))
	printOverTime(testModelPop,"Qb.Standardmetric.",grad=c("green","darkgreen"))
	dev.off()
	printOverTime(testModelPop,"N.Numberofmodules.","cut")
	printOverTime(testModelPar,"N.Numberofmodules.","par")
	printOverTime(testModelMut,"NODF.Nestednessvalue.")
	printOverTime(testModelPop,"NODF.Nestednessvalue.","cut")
	printOverTime(testModelPar,"NODF.Nestednessvalue.","par")
	printOverTime(testModelMut,"Qr.Ratioofint.extinter.")
	printOverTime(testModelPop,"Qr.Ratioofint.extinter.","cut")
	printOverTime(testModelPar,"Qr.Ratioofint.extinter.","par")
	printOverTime(testModelMut,"Qb.Standardmetric.")
	printOverTime(testModelPop,"Qb.Standardmetric.","cut")
	printOverTime(testModelPar,"Qb.Standardmetric.","par")

	boxprintOverTime(testModelMut,"N.Numberofmodules.")
	boxprintOverTime(testModelPop,"N.Numberofmodules.","cut")
	boxprintOverTime(testModelPar,"N.Numberofmodules.","par")
	boxprintOverTime(testModelMut,"NODF.Nestednessvalue.")
	boxprintOverTime(testModelPop,"NODF.Nestednessvalue.","cut")
	boxprintOverTime(testModelPar,"NODF.Nestednessvalue.","par")
	boxprintOverTime(testModelMut,"Qr.Ratioofint.extinter.")
	boxprintOverTime(testModelPop,"Qr.Ratioofint.extinter.","cut")
	boxprintOverTime(testModelPar,"Qr.Ratioofint.extinter.","par")
	boxprintOverTime(testModelMut,"Qb.Standardmetric.")
	boxprintOverTime(testModelPop,"Qb.Standardmetric.","cut")
	boxprintOverTime(testModelPar,"Qb.Standardmetric.","par")

	png("img/QbMetvsSexProb_tsup60k.png")
	test=testModelMut[testModelMut$ticks > 60000, ]
	boxplot(test$Qb.Standardmetric. ~ test$sexproba,ylab="Qb Metrics",xlab="Sex. Proba",main="Qb Metrics wrt Sex Prob\n(after 60000ticks)")
	dev.off()
	png("img/QbMetvsSexProb_tsup60k.png")
	test=testModelMut[testModelMut$ticks > 60000, ]
	boxplot(test$Qb.Standardmetric. ~ test$sexproba,ylab="Qb Metrics",xlab="Sex. Proba",main="Qb Metrics wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/NestedNODFvsSexProb_tsup60k.png")
	boxplot(test$NODF.Nestednessvalue. ~ test$sexproba,ylab="NODF Nestedness",xlab="Sex. Proba",main="Nestedness value wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/QrRatiovsSexProb_tsup60k.png")
	boxplot(test$Qr.Ratioofint.extinter. ~ test$sexproba,ylab="Qr Ratio",xlab="Sex. Proba",main="Qr ratio wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	printSnapshotTime(testModelMut,"N.Numberofmodules.","mut")
	printSnapshotTime(testModelPop,"N.Numberofmodules.","cut")
	printSnapshotTime(testModelMut,"Qr.Ratioofint.extinter.","mut")
	printSnapshotTime(testModelPop,"Qr.Ratioofint.extinter.","cut")
	printSnapshotTime(testModelMut,"Qb.Standardmetric.","mut")
	printSnapshotTime(testModelPop,"Qb.Standardmetric.","cut")
	printSnapshotTime(testModelMut,"NODF.Nestednessvalue.","mut")
	printSnapshotTime(testModelPop,"NODF.Nestednessvalue.","cut")



	sapply(unique(testModelPar$ticks),function(ti){
	       testPar60k=testModelPar[testModelPar$ticks >= 60000 , ]
	       testMut60k=testModelMut[testModelMut$ticks >= 60000 , ]
#	       png(paste("img/compareParaVsMu/NModulevsSexProb-ParaVsMutu_t=",ti,".png",sep=""))
#	       boxplot(testPar$N.Numberofmodules. ~ testPar$sexproba,ylab="N. Modules",xlab="Sex. Proba",main=paste("Num of modules wrt Sex Proba (ticks=",ti,")",sep=""),ylim=c(0,max(testModelMut$N.Numberofmodules.,testModelPar$N.Numberofmodules.)))
#	       boxplot(testMut$N.Numberofmodules. ~ testMut$sexproba,add=T,col="green")
#	       legendMut()
#	       dev.off()
})

	tapply(cbind(testPar60k$N.Numberofmodules.,testMut60k$N.Numberofmodules.),testPar60k$sexproba,t.test)
	res=sapply(unique(testPar60k$sexproba),function(sp){
	       li=c()

	       test=t.test(testPar60k$N.Numberofmodules.[testPar60k$sexproba == sp ],testMut60k$N.Numberofmodules.[testMut60k$sexproba == sp])
	       li=rbind(li,c(test$estimate,test$p.value))

	       test=t.test(testPar60k$Qr.Ratio[testPar60k$sexproba == sp ],testMut60k$Qr.Ratio[testMut60k$sexproba == sp])
	       li=rbind(li,round(c(test$estimate,test$p.value),3))
#print(round(c(test$estimate,test$p.value),3)))
	       test=t.test(testPar60k$Qb.[testPar60k$sexproba == sp ],testMut60k$Qb.[testMut60k$sexproba == sp])
	       li=rbind(li,round(c(test$estimate,test$p.value),3))
	       test=t.test(testPar60k$NODF.Nest[testPar60k$sexproba == sp ],testMut60k$NODF.Nest[testMut60k$sexproba == sp])
	       li=rbind(li,round(c(test$estimate,test$p.value),3))
#
})
	tablStatistique<function(){
	    #To write a latex table with mean and pvalues
	    res=c()
	    for (sp in (unique(testPar60k$sexproba)))
	    {
		li=c()

		test=t.test(testPar60k$N.Numberofmodules.[testPar60k$sexproba == sp ],testMut60k$N.Numberofmodules.[testMut60k$sexproba == sp])
		li=c(li,c(test$estimate,test$p.value))

		test=t.test(testPar60k$Qr.Ratio[testPar60k$sexproba == sp ],testMut60k$Qr.Ratio[testMut60k$sexproba == sp])
		li=c(li,round(c(test$estimate,test$p.value),3))
		#print(round(c(test$estimate,test$p.value),3)))
		test=t.test(testPar60k$Qb.[testPar60k$sexproba == sp ],testMut60k$Qb.[testMut60k$sexproba == sp])
		li=c(li,round(c(test$estimate,test$p.value),3))
		test=t.test(testPar60k$NODF.Nest[testPar60k$sexproba == sp ],testMut60k$NODF.Nest[testMut60k$sexproba == sp])
		li=c(li,round(c(test$estimate,test$p.value),3))
		#
		res=rbind(res,li)
	    }

	    rownames(res)=c("Numer of Module","Qr ration","Qb metrics","Nestedness")
	    colnames(res)=rep(c("P","M","pval"),4)
	    rownames(res)=c(1,5,10,50,100)
	    addmult=list()
	    addmult$pos=list(0)
	    addmult$command=("& \\multicolumn{3}{c}{Numer of Module}& \\multicolumn{3}{c}{Qr ration} & \\multicolumn{3}{c}{Qb metrics} & \\multicolumn{3}{c}{Nestedness}")
	    print(xtable(res),add.to.row=addmult)
	    #	       png(paste("img/compareParaVsMu/NModulevsSexProb-ParaVsMutu_t=",ti,".png",sep=""))
	    #	       boxplot(testPar$N.Numberofmodules. ~ testPar$sexproba,ylab="N. Modules",xlab="Sex. Proba",main=paste("Num of modules wrt Sex Proba (ticks=",ti,")",sep=""),ylim=c(0,max(testModelMut$N.Numberofmodules.,testModelPar$N.Numberofmodules.)))
	    #	       boxplot(testMut$N.Numberofmodules. ~ testMut$sexproba,add=T,col="green")
	    #	       legendMut()
	    #	       dev.off()
	}

	png("img/NestedNODFvsSexProb-cut_tsup60k.png")
	boxplot(test$NODF.Nestednessvalue. ~ test$sexproba,ylab="NODF Nestedness",xlab="Sex. Proba",main="Nestedness value wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/QrRatiovsSexProb-cut_tsup60k.png")
	boxplot(test$Qr.Ratioofint.extinter. ~ test$sexproba,ylab="Qr Ratio",xlab="Sex. Proba",main="Qr ratio wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/QbMetvsSexProb-cut_tsup60k.png")
	test=testModelPop[testModelPop$ticks > 60000, ]
	boxplot(test$Qb.Standardmetric. ~ test$sexproba,ylab="Qb Metrics",xlab="Sex. Proba",main="Qb Metrics wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/NestedNODFvsSexProb-cut_tsup60k.png")
	boxplot(test$NODF.Nestednessvalue. ~ test$sexproba,ylab="NODF Nestedness",xlab="Sex. Proba",main="Nestedness value wrt Sex Prob\n(after 60000ticks)")
	dev.off()

	png("img/QrRatiovsSexProb-cut_tsup60k.png")
	boxplot(test$Qr.Ratioofint.extinter. ~ test$sexproba,ylab="Qr Ratio",xlab="Sex. Proba",main="Qr ratio wrt Sex Prob\n(after 60000ticks)")
	dev.off()




	testModelPar=testModelPar[testModelPar$ticks > 60000, ]
	testModelPop=testModelPop[testModelPop$ticks > 60000, ]
	testModelMut=testModelMut[testModelMut$ticks > 60000, ]
	png("img/compareParaVsMu/QrRatiovsSexProb_tsup60kParaVsMutu.png")
	boxplot(testModelPar$Qr.Ratioofint.extinter. ~ testModelPar$sexproba,ylab="Qr Ratio",xlab="Sex. Proba",main="Qr ratio wrt Sex Prob\n(after 60000ticks)")
	boxplot(testModelMut$Qr.Ratioofint.extinter. ~ testModelMut$sexproba,add=T,col="green")
	legendMut()
	dev.off()

	png("img/compareParaVsMu/QbMetvsSexProb_tsup60kParaVsMutu.png")
	boxplot(testModelPar$Qb.Standardmetric. ~ testModelPar$sexproba,ylab="Qb Metrics",xlab="Sex. Proba",main="Qb Metrics wrt Sex Prob\n(after 60000ticks)")
	boxplot(testModelMut$Qb.Standardmetric. ~ testModelMut$sexproba,add=T,col="green")
	legendMut()
	dev.off()

	png("img/compareParaVsMu/NestedNODFvsSexProb_tsup60kParaVsMutu.png")
	boxplot(testModelPar$NODF.Nestednessvalue. ~ testModelPar$sexproba,ylab="NODF Nestedness",xlab="Sex. Proba",main="Nestedness value wrt Sex Prob\n(after 60000ticks)")
	boxplot(testModelMut$NODF.Nestednessvalue. ~ testModelMut$sexproba,add=T,col="green")
	legendMut()
	dev.off()
}

legendMut<-function(){
	legend("topright",c("Parasitism","mutualism"),fill=c("green","white"))
}

printOverTime <- function(dat,prop,suff="mut",grad=c("skyblue","darkblue"),...){
	#pdf(paste("img/allPropOverTime/",prop,"vsTime-",suff,".pdf",sep=""),pointsize=14)
	
	plot(dat[,prop] ~ dat$ticks,type="n",ylab="Modularity (Qb)",xlab="Iterations",main="",ylim=c(0,1))#paste(prop,"over Time"))
	colsex=1:length(unique(dat$sexproba))
	colsex=colorRampPalette(grad)(length(unique(dat$sexproba)))
	names(colsex)=unique(dat$sexproba)
	sapply(unique(dat$sexproba) , function(i){
	       this=dat[ dat$sexproba == i,]
	       #points(this[,prop]~ this$ticks,col=colsex[as.character(i)])
	       #lines(unique(this$ticks),+tapply(this[,prop],this$ticks,sd),col=colsex[as.character(i)],lwd=1,lty=2)
	       sdev=tapply(this[,prop],this$ticks,sd)
	       avg=tapply(this[,prop],this$ticks,mean)
	       x=unique(this$ticks)
	       lines(x,avg,col=colsex[as.character(i)],lwd=4)
	       arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3,col=colsex[as.character(i)],lwd=4)
	       #lines(unique(this$ticks),tapply(this[,prop],this$ticks,mean)-tapply(this[,prop],this$ticks,sd),col=colsex[as.character(i)],lwd=1,lty=2)
})
	legend("bottom",legend=names(colsex),col=colsex,lwd=4,lty=1,title="SRP(%)")
#	dev.off()
}

boxprintOverTime <- function(dat,prop,suff="mut"){
	#png(paste("img/allPropOverTime/",prop,"vsTime-",suff,"-box.png",sep=""))
	plot(dat[,prop] ~ dat$ticks,type="n",ylab=prop,xlab="time",main=paste(prop,"B"),axes=F,xlim=c(0,20))
	colsex=1:length(unique(dat$sexproba))
	names(colsex)=unique(dat$sexproba)
	sapply(unique(dat$sexproba) , function(i){
	       this=dat[ dat$sexproba == i,]
	       boxplot(this[,prop]~ this$ticks,col=colsex[as.character(i)],add=T,outline=F)
})
	legend("bottomright",legend=names(colsex),col=colsex,pch=1,title="Sex. Proba.")
	#dev.off()
}

printSnapshotTime <- function(dat,prop,suff="mut"){
	dir.create(paste("img/","SnapshotOverTime",prop,"/",sep=""))
	sapply(unique(dat$ticks),function(ti){
	       test=dat[dat$ticks == ti , ]
	       png(paste("img/","SnapshotOverTime",prop,"/",prop,"vsSexProb-",suff,"_t=",ti,".png",sep=""))
	       boxplot(test[,prop] ~ test$sexproba,ylab=prop,xlab="Sex. Proba",main=paste(prop," wrt Sex Proba (ticks=",ti,")",sep=""),ylim=c(0,max(dat[,prop])))
	       dev.off()
})
}

#Create a sequence of tuple of the form seq=(X,X+.5),(X+sep,X+sep+.5)
#Pretty sure there is a one liner for that.
myseq<-function(x,n,sep,isep){
    res=c(x,x+isep)
    for(i in 1:(n-1)){
	prev=res[length(res)]
	res=c(res,c(prev+sep,prev+sep+isep))
    }
    return(res)
}

pos<-function(sequ){
 return((sequ[seq(1,length(sequ),2)]+sequ[seq(2,length(sequ),2)])/2)
}


  pdf("bipartite29.pdf",height=2)                                                      
  par(mar=c(0,0,0,0))                                                           
  plotNetwork(allmat[["29"]])                                                     
  dev.off() 
