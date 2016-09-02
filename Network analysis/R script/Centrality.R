library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)

temp=read.csv("/Users/aina/Dropbox/Lichen-s-project/data/cooc_mat/mat_fulldataset.csv", header = TRUE, sep = ",")
temp1 <- as.matrix(temp)
temp1

edgelist=web2edges(temp1, webName=NULL, weight.column=TRUE, both.directions=FALSE,
          is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
          return=TRUE, verbose=TRUE)

betweennes_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)
write.table(betweennes_netw,"/Users/aina/Desktop/prova.txt")
computeModules(temp1,deep=FALSE,deleteOriginalFiles = TRUE,steps=1000000,tolerance=1e-10,experimental=FALSE)




sampledata <- rbind(
  c(1,2,1),
  c(1,3,5),
  c(2,1,1),
  c(2,4,6),
  c(3,1,5),
  c(3,4,10),
  c(4,2,6),
  c(4,3,10))
sampledata
betweenness_w(sampledata)
edgelist=web2edges(sampledata, webName=NULL, weight.column=TRUE, both.directions=FALSE,
                   is.one.mode=FALSE, out.files=c("edges", "names", "groups")[1:2],
                   return=TRUE, verbose=TRUE)
