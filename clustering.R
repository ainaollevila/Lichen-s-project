library(dendextend)
#Get the raw data
rawdata=read.csv("data/Widmer et al_2012.xlsx",sep='\t')

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
algclust_dendmod=color_branches(algclust_dend,k=100)
fungclust_dendmod=color_branches(fungclust_dend,k=100)
unttangdend=untangle_step_rotate_1side(algclust_dendmod,fungclust_dendmod)
png("test.png",width=1000,height=4200)
#tanglegram(unttangdend[[1]],unttangdend[[2]])
tanglegram(algclust_dendmod,fungclust_dendmod)
dev.off()
