

library(dendextend)
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

cooccurenceMat=function(mat){
	res=matrix(0,nrow=nrow(mat),ncol=ncol(mat))
	for(i in 1:(nrow(mat)-1)){
		for(j in (i+1):(ncol(d))){
			if(mat[i,j]<1)
				res[i,j]=mat[i,j]
		}
	}
}
#Get the raw data
rawdata=read.csv("data/Widmer et al_2012.xlsx",sep='\t')
rawdata=read.csv("data/DalGrande_et_al_2012.csv",sep='\t')
#rawdata=dal
#get a subset
rawdata=rawdata[1:100,]
rownames(rawdata)=rawdata[,1]

#separate th microsat
funghus=rawdata[,3:11]
algae=rawdata[,12:17]
fdistmat=as.matrix(round(dist(funghus),digit=-1))
adistmat=as.matrix(round(dist(algae),digit=-1))
radist=round(dist(algae),digit=-1)

pdf("dist_mat.pdf")#,width=7,height=8)
par(mfrow=c(2,1),mar=c(0,0,0,0))
image(fdistmat)#,axis="n",main="funghi")
image(radistmat)#,axis="n",,main="algae")
dev.off()
aramp=colorRampPalette(c("yellow","red"))(max(adistmat))
framp=colorRampPalette(c("yellow","red"))(max(fdistmat))
plot(rawdata$x,rawdata$y,col=aramp[adistmat[2,]],ylim=c(-1000,500),xlim=c(-2000,1000))
points(rawdata$x,rawdata$y+10,col=framp[fdistmat[2,]],ylim=c(-1000,500),xlim=c(-2000,1000))

png("dist_mat.png",width=2000,height=4000)
par(mfrow=c(2,1),mar=c(0,0,0,0))
image(fdistmat)#,axis="n",main="funghi")
image(adistmat)#,axis="n",,main="algae")
dev.off()

#compute distance and clust
fungdist=computeDist(funghus)
algdist=computeDist(algae)

fungclust=hclust(fungdist)
algclust= hclust(algdist)

fungclust_dend=as.dendrogram(fungclust)
algclust_dend=as.dendrogram(algclust)

algclust_dendmod=color_branches(algclust_dend,k=100)
fungclust_dendmod=color_branches(fungclust_dend,k=100)
unttangdend=untangle_step_rotate_1side(algclust_dend,fungclust_dend)

png("dalgrande.png",width=800,height=1000,pointsize=17)
#tanglegram(unttangdend[[1]],unttangdend[[2]])
tanglegram(algclust_dend,fungclust_dend,axes=F,main_left="Algae",main_right="Funghi")
dev.off()



#a=c(1,1,12,13,600)
#b=c(1,-11,12,101,4)
#te=rbind(a,b)
#res=simil(te)
#print(res)

