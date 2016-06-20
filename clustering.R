rawdata=read.csv("Widmer et al_2012.xlsx",sep='\t')
microsat=rawdata[,3:17]
rownames(microsat)=rawdata[,1]
plot(hclust(dist(microsat)))

