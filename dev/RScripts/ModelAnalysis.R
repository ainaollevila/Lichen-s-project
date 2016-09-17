source("utils.R")
source("AsymmetryValuesFunction.R")

#    allModel=data.frame()
#
#for(files in list.file(path="../data/ResultsCut/",pattern="*genprop.csv"){
#    for(nrep  in  allrep){
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
#	#matMod=cooccurenceMat(data_model1)
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
    colnames(testModelPop)=c(colnames(testModelMut),"pop")

    testModelPar=read.csv("../data/ResultsParasitism/concatenate_result_genprop.csv",header=T)
    testModelMut=read.csv("../data/Results/concatenate_result_genprop.csv",header=T)


    dalgrande2012=read.csv("../../data/cooc_mat/Results/concatenate_result_genprop.csv",head=F)
    colnames(dalgrande2012)=colnames(testModelMut)[1:14]

    modSP1subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 1,1:14 ]
    modSP1subpop$type= "Model Sp=1"
    modSP5subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 5,1:14 ]
    modSP5subpop$type= "Model Sp=5"
    modSP10subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 10,1:14 ]
    modSP10subpop$type= "Model Sp=10"
    modSP100subpop=testModelPop[testModelPop$ticks > 60000 & testModelPop$sexproba == 100,1:14 ]
    modSP100subpop$type= "Model Sp=100"
    modSP1=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 1,1:14 ]
    modSP1$type= "Model Sp=1"
    modSP100=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 100,1:14 ]
    modSP100$type= "Model Sp=100"
    modSP10=testModelMut[testModelMut$ticks > 60000 & testModelMut$sexproba == 10,1:14 ]
    modSP10$type= "Model Sp=10"
    dalgrande2012$type= "DalGrande12"

    compare=rbind(dalgrande2012,modSP1,modSP1subpop,modSP5,modSP5subpop,modSP10,modSP10subpop)
    compare=rbind(dalgrande2012,modSP1subpop,modSP10subpop,modSP100subpop)
    compare=rbind(dalgrande2012,modSP1,modSP10,modSP100)

    png("img/compareAll/QbMetcompare.png",width=750)
    boxplot(compare$Qb.Standardmetric. ~ compare$type,ylab="Modularity",xlab="",main="Modularity",outline=F,ylim=c(0,1))
    dev.off()

    png("img/compareAll/NestedNODFcompare.png",width=750)
    boxplot(compare$N.Numberofmodules. ~ compare$type,ylab="Number of Node",xlab="",main="Modularity",outline=F,ylim=c(0,210))
    dev.off()

    png("img/compareAll/QrRatiocompare.png",width=750)
    boxplot(compare$Qr.Ratioofint.extinter. ~ compare$type,ylab="Qr Ratio",xlab="",main="Qr Ratio")
    dev.off()

    png("img/compareAll/aNModuelcompare.png",width=750)
    boxplot(as.numeric(compare$N.Numberofmodules.) ~ compare$type,ylab="Number of Modules",xlab="",main="Number of Modules")
    dev.off()

    printOverTime(testModelMut,"N.Numberofmodules.")
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
	   testPar=testModelPar[testModelPar$ticks == ti , ]
	   testMut=testModelMut[testModelMut$ticks == ti , ]
	   png(paste("img/compareParaVsMu/NModulevsSexProb-ParaVsMutu_t=",ti,".png",sep=""))
	   boxplot(testPar$N.Numberofmodules. ~ testPar$sexproba,ylab="N. Modules",xlab="Sex. Proba",main=paste("Num of modules wrt Sex Proba (ticks=",ti,")",sep=""),ylim=c(0,max(testModelMut$N.Numberofmodules.,testModelPar$N.Numberofmodules.)))
	   boxplot(testMut$N.Numberofmodules. ~ testMut$sexproba,add=T,col="green")
	   legendMut()
	   dev.off()
	})


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

printOverTime <- function(dat,prop,suff="mut"){
    png(paste("img/allPropOverTime/",prop,"vsTime-",suff,".png",sep=""))
    plot(dat[,prop] ~ dat$ticks,type="n",ylab=prop,xlab="time",main=paste(prop,"over Time"))
    colsex=1:length(unique(dat$sexproba))
    names(colsex)=unique(dat$sexproba)
    sapply(unique(dat$sexproba) , function(i){
	this=dat[ dat$sexproba == i,]
    	points(this[,prop]~ this$ticks,col=colsex[as.character(i)])
	 })
    legend("center",legend=names(colsex),col=colsex,pch=1,title="Sex. Proba.")
    dev.off()
}

boxprintOverTime <- function(dat,prop,suff="mut"){
    png(paste("img/allPropOverTime/",prop,"vsTime-",suff,"-box.png",sep=""))
    boxplot(dat[,prop] ~ dat$ticks,type="n",ylab=prop,xlab="time",main=paste(prop,"over Time"))
    colsex=1:length(unique(dat$sexproba))
    names(colsex)=unique(dat$sexproba)
    sapply(unique(dat$sexproba) , function(i){
	this=dat[ dat$sexproba == i,]
    	boxplot(this[,prop]~ this$ticks,col=colsex[as.character(i)],add=T)
	 })
    legend("center",legend=names(colsex),col=colsex,pch=1,title="Sex. Proba.")
    dev.off()
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
