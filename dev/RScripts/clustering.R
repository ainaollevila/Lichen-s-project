library(scales)

#Row = Fungus
#Col = Algae

#Function to load and merge data from Widmer 2012 et Dal Grande 2012
fullD=NULL
fullW=NULL

loadData<-function(){
	rawdataW=read.csv("../../data/Widmer et al_2012.xlsx",sep='\t')
	locW=read.csv("../../data/loc_widmer2012.csv")

	fullW<<-merge(rawdataW,locW,by.x="Population",by.y="Population.code")

	rawdataD=read.csv("../../data/DalGrande_et_al_2012.csv",sep='\t')
	locD=read.csv("../../data/loc_dalgrande2012.csv")
	fullD<<-merge(rawdataD,locD,by.x="Population.ID",by.y="Population.n..")
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

#ccurenceMat=function(mat){
#	res=matrix(0,nrow=nrow(mat),ncol=ncol(mat))
#	for(i in 1:(nrow(mat)-1)){
#		for(j in (i+1):(ncol(d))){
#			if(mat[i,j]<1)
#				res[i,j]=mat[i,j]
#		}
#	}
#}

getSeparate <- function(a){
	#separate th microsat
	seplist=list(rawdata[,3:11],rawdata[,12:17])
	names(seplist)=c("algae","fungus")
	return(seplist)
}

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

	algae=t(sapply(unique(rawdata$Population.ID),function(i){
		       apply(rawdata[rawdata$Population.ID==i,3:11],2,mean)

}))

	fungus=t(sapply(unique(rawdata$Population.ID),function(i){
			apply(rawdata[rawdata$Population.ID==i,12:17],2,mean)

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

buildRel<- function(datas,algdif=2,fungdif=2,groupf=c(),groupa=c()){
	groupf=
	res=matrix(0,nrow=length(groupf),ncol=length(groupa),dimnames=list(groupf,groupa))

	for(i in 1:(nrow(datas)-1)){
		print(paste(i))
		for(j in (i):(nrow(datas))){
			ifungusA=paste(datas[i,3:10],collapse="")
			ifungusB=paste(datas[j,3:10],collapse="")
			ialgaeA=paste(datas[i,11:17],collapse="")
			ialgaeB=paste(datas[j,11:17],collapse="")



			#print(paste(ialgaeB,ialgaeA,ifungusB,ifungusA,sep=","))
			vfungusA=datas[i,3:10]
			vfungusB=datas[j,3:10]
			if(simil(rbind(vfungusB,vfungusA))<fungdif){
				res[ifungusB,ialgaeA]=res[ifungusB,ialgaeA]+1
				res[ifungusA,ialgaeB]=res[ifungusA,ialgaeB]+1
			}

			valgaeA=datas[i,11:17]
			valgaeB=datas[j,11:17]
			if(simil(rbind(valgaeB,valgaeA))<algdif){
				res[ifungusB,ialgaeA]=res[ifungusB,ialgaeA]+1
				res[ifungusA,ialgaeB]=res[ifungusA,ialgaeB]+1
			}
		}
	}
	#res=list(resF,resA)
	#names(res)=c("fungus","algae")
	return(res)

}

#Compute the matrix
cooccurenceMatTocheck <- function(datas,algdif=2,fungdif=2,groupf=c(),groupa=c()){
	if(length(groupa)==0){
		groupa=as.character(unique(datas$Sample.ID))
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

#cooccurenceMat <- function(datas,algdif=2,fungdif=2,groupf=c(),groupa=c()){
#	if(length(groupa)==0){
#		groupa=as.character(unique(datas$Population.ID))
#		groupf=groua
#	}
#	res=matrix(0,nrow=length(groupf),ncol=length(groupa),dimnames=list(groupf,groupa))
#	print(res)
#
#	for(i in 1:(nrow(datas))){
#		for(j in (i):(nrow(datas))){
#			#print(paste(i,j))
#			if(length(groupa)==0){
#				popa=as.character(datas[i,"Population.ID"])
#				popb=as.character(datas[j,"Population.ID"])
#			}
#			else{
#				popa=paste(datas[i,3:10],collapse="")
#				popb=paste(datas[j,11:17],collapse="")
#			}
#
#
#			vfungusA=datas[i,3:10]
#			vfungusB=datas[j,3:10]
#			if(simil(rbind(vfungusB,vfungusA))<fungdif)
#				res[popa,popb]=res[popa,popb]+1
#
#			valgaeA=datas[i,11:17]
#			valgaeB=datas[j,11:17]
#			if(simil(rbind(valgaeB,valgaeA))<algdif){
#				res[popa,popb]=res[popa,popb]+1
#			}
#		}
#	}
#	#res=list(resF,resA)
#	#names(res)=c("fungus","algae")
#	return(res)
#
#
#
#}
#fillSimil <- function(v1,v2,matrices){
#	if(simil(v1,v2)<2)
#		mat[a
#}

#allResult=cooccurenceMat(rawdata,2,2)
#
#plot2mt(test[[1]],test[[2]])
#par(mfrow=c(2,1))
#plot(as.dendrogram(hclust(as.dist(1/(t(test[[1]])+1)))))
#a=c(1,1,12,13,600)
#b=c(1,-11,12,101,4)
#te=rbind(a,b)
#res=simil(te)
#print(res)


plot2mt <- function(fdistmat,adistmat,...){
	par(mfrow=c(2,1),mar=c(0,0,0,0))
	image(fdistmat)#,axis="n",main="fungus")
	image(adistmat)#,axis="n",,main="algae")
}

writeMat <- function(test){
	sapply(names(test),function(i){write.csv(test[[i]],paste(i,".csv",sep=""))})
}
writeAdj <- function(d){

	res=c()
	for(i in colnames(d)){
		for(j in colnames(d)){
			print(paste(i,j))
			if(d[i,j] > 0 ){
				res=rbind(res,cbind(i,j,d[i,j]))
			}

		}
	}
	return(res)
}

#other=writeAdj(test[[2]])
cluster <- function(){
	algalloc=namerow(read.csv("data/algal"))
	fungalloc=namerow(read.csv("data/fungal"))
	

}

namerow <- function(x){
	rownames(x)=x[,1]
	x=x[,2:ncol(x)]

}

plotNetwork<-function(mat,id="",...){
	fungus=1:nrow(mat)*4 #cbind(inPoints,names(inWeigth))
	algae=1:ncol(mat)*4 #cbind(outPoints,names(outWeigth))

	ptsize=3
	plot(fungus,rep(-1,nrow(mat)),cex=ptsize,bty="n",ylim=c(-1.5,1.5),xlim=c(-10,max(fungus)+5),col=alpha("red",0.5),pch=20,xaxt="n",xlab="",yaxt="n", ylab="",...)
	text(fungus,rep(-.7,nrow(mat))-.5,label=rownames(mat),cex=1,srt=60,c(1,1))

	points(algae,rep(1,ncol(mat)),cex=ptsize,col=alpha("yellow",0.5),pch=20)
	text(algae,rep(.7,ncol(mat))+.5,label=colnames(mat),cex=1,srt=300,adj=c(1,1))
	text(-5,-1,"fungi")
	text(-5,1,"algae")
	#text(-5+length(fungus),1,"projects")
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
				#print(paste("i",i))
				#print(paste("j",j))
				#print(mat[i,j])
				if(mat[f,a]>=1){
					segments(x0=xf,y0=yf+.05,x1=xp,y1=yp-.05,col=alpha("green",.6),lwd=mat[f,a]/2,length=.1)
				}
				#if(mat[j,i]>=1){

				#	arrows(x0=xp,y0=yp-.05,x1=xf,y1=yf+.05,col=alpha("yellow",.6),lwd=log(mat[j,i]),length=.1)
				#}
			}
		}
	}
}

readMat <- function(filename){
	m=read.csv(filename,header=T)
	rownames(m)=m[,1]
	m=m[,2:ncol(m)]
	return(m)
}

#m1=readMat("pop_similiarity1.csv")
#m0=readMat("pop_similiarity0.csv")
#png("biparte_net_pop_0.png", width=1200,height=800)
#plotNetwork(m0)
#dev.off()
#png("biparte_net_pop_1.png", width=1200,height=800)
#plotNetwork(m1)
#dev.off()

plotSample<-function(data,...){

    colscale=topo.colors(length(unique(data$Population.ID)))
    names(colscale)=unique(data$Population.ID)
    plot(data$Longitude..E.,data$Latitude..N.,col=colscale[data$Population],...)

}



createNetwork<-function(rwdt){

	#separate th microsat
	fungus=rwdt[,3:10]
	algae=rwdt[,11:17]
	
	print(nrow(rwdt))
	al=apply(algae,1,paste,collapse="")
	fu=apply(fungus,1,paste,collapse="")
	uaid=unique(apply(algae,1,paste,collapse=""))
	ufid=unique(apply(fungus,1,paste,collapse=""))
	res=cooccurenceMat(rwdt,1,1,groupf=ufid,groupa=uaid)
	return(res)

}

plotpop<-function(d,n,...){
	curpop=d[d$Population.ID == n,]
	plot(curpop$x,curpop$y)
}


cooccurenceMat <- function(datas,algdif=2,fungdif=2,groupf=c(),groupa=c()){

    res=matrix(0,nrow=length(groupf),ncol=length(groupa),dimnames=list(groupf,groupa))

    for(i in 1:nrow(datas)){
	popiMlgF=paste(datas[i,3:10],collapse="")
	popiMlgA=paste(datas[i,11:17],collapse="")
	res[popiMlgF,popiMlgA]=res[popiMlgF,popiMlgA]+1

	#for(j in (i+1):nrow(datas)){

	#    popjMlgF=paste(datas[j,3:10],collapse="")
	#    popjMlgA=paste(datas[j,11:17],collapse="")
	#    print(paste(i," - ",j," res=",res[popiMlgF,popiMlgA]))
	#    print(paste("i:",popiMlgA,popiMlgF))
	#    print(paste("j:",popjMlgA,popjMlgF))


	#    if(popiMlgF == popjMlgF){
	#	res[popiMlgF,popjMlgA]=res[popiMlgF,popjMlgA]+1
	#	#	res[popjMlgF,popiMlgA]=res[popjMlgF,popjMlgA]+1
	#    }
	#}
    }
    return(res)



}



main<-function(){
plotpop(fullD,2)
a=15

sapply(unique(fullD$Population.ID),function(a){
test=createNetwork(fullD[fullD$Population.ID == a ,])
write.csv(test,paste("../../data/cooc_mat/mat_pop-",a,".csv",sep=""))
png(paste("../../data/cooc_mat/bipartite_pop-",a,".png",sep=""),height=600,width=900)
plotNetwork(test,main=paste("../../data/cooc_mat/Population",a))
dev.off()
})

fulltest=createNetwork(fullD)
write.csv(test,paste("../../data/cooc_mat/mat_fulldataset.csv",sep=""))
plotNetwork(fulltest)
}
