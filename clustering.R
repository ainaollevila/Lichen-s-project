library(dendextend)
#Get the raw data
rawdata=read.csv("Widmer et al_2012.xlsx",sep='\t')

#get a subset
rawdata=rawdata[1:100,]
rownames(rawdata)=rawdata[,1]

#separate th microsat
funghus=rawdata[,3:8]
algae=rawdata[,8:17]

#compute distance and clust
fungclust=hclust(dist(funghus))
algclust=hclust(dist(algae))
fungclust_dend=as.dendrogram(fungclust)
algclust_dend=as.dendrogram(algclust)
algclust_dendmod=color_branches(algclust_dend,k=10)
fungclust_dendmod=color_branches(fungclust_dend,k=10)

png("test.png",width=800,height=1200)
tanglegram(algclust_dendmod,fungclust_dendmod)
dev.off()
