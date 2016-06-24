if(require("scales")){library(scales)}
library(dendextend)
#Get the raw data
rawdata=read.csv("data/Widmer et al_2012.xlsx",sep='\t')
rawdata=read.csv("data/DalGrande_et_al_2012.csv",sep='\t')
#rawdata=dal
#get a subset
rawdata=rawdata[1:200,]
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
fungclust=hclust(dist(funghus))
algclust=hclust(dist(algae))

fungclust_dend=as.dendrogram(fungclust)
algclust_dend=as.dendrogram(algclust)

algclust_dendmod=color_branches(algclust_dend,k=100)
fungclust_dendmod=color_branches(fungclust_dend,k=100)
unttangdend=untangle_step_rotate_1side(algclust_dend,fungclust_dend)

png("cutdown.png",width=800,height=1000,pointsize=17)
#tanglegram(unttangdend[[1]],unttangdend[[2]])
tanglegram(algclust_dend,fungclust_dend)
dev.off()
