library(dendextend)
#Get the raw data
rawdata=read.csv("data/Widmer et al_2012.xlsx",sep='\t')

#get a subset
rawdata=rawdata[1:100,]
rownames(rawdata)=rawdata[,1]

#separate th microsat
funghus=rawdata[,3:11]
algae=rawdata[,12:17]
fdistmat=as.matrix(dist(funghus))
adistmat=as.matrix(dist(algae))

pdf("dist_mat.pdf")#,width=7,height=8)
par(mfrow=c(2,1),mar=c(0,0,0,0))
image(fdistmat)#,axis="n",main="funghi")
image(adistmat)#,axis="n",,main="algae")
dev.off()

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

png("testuntang.png",width=800,height=1000,pointsize=17)
#tanglegram(unttangdend[[1]],unttangdend[[2]])
tanglegram(algclust_dend,fungclust_dend)
dev.off()
