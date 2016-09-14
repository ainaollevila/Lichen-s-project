source("utils.R")
source("AsymmetryValuesFunction.R")

    allModel=data.frame()
for(files in list.file(path="../data/ResultsCut/",pattern="*genprop.csv"gT
    for(nrep  in  allrep){
	data_model1=read.csv(paste("../../dev/data/ECHOresults/mutualism_michaelis-menten_10000ticks_1sexualreproduction_replicate",nrep,".dat",sep=""),header=F)
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

    testModelPop=read.csv("../data/ResultsCut//concatenate_result_genprop.csv",header=F)
    colnames(testModelPop)=c(colnames(testNestednesGen),"pop")
    testModelPop=testModelPop[testModelPop$ticks > 60000, ]
    testModel
    boxplot(testModelPop$Qr.Ratioofint.extinter. ~ testModelPop$sexproba)

    testModelPar=read.csv("../data/ResultsParasitism/concatenate_result_genprop.csv",header=T)
    testModelPar=testModelPar[testModelPar$ticks > 60000, ]
    boxplot(testModelPar$Qr.Ratioofint.extinter. ~ testModelPar$sexproba)
    testModelMut=read.csv("../data/Results/concatenate_result_genprop.csv",header=T)
    testModelMut=testModelMut[testModelMut$ticks > 60000, ]
    boxplot(testModelMut$Qr.Ratioofint.extinter. ~ testModelMut$sexproba,add=T,col="green")
