library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)

datafolder="../../data/cooc_mat/"
datafolder
temp=read.csv(paste(datafolder,"mat_pop-4.csv",sep=""), header = TRUE, sep = ",")
temp1=temp[,2:ncol(temp)]
mat <- as.matrix(temp1)
dimnames(mat) <- NULL
mat
rows=nrow(mat)
rows
cols=ncol(mat)
cols
m<-matrix(0,nrow=rows+cols,ncol=rows+cols)
m
#oh shit, igraph needs a square matrix!!! puc crear-la artificialment a partir de la bipartite...
for (i in 1:rows){
  for (j in 1:cols){
    m[i,rows+j] = mat[i,j] 
  }
}
for (i in 1:rows){
  for (j in 1:cols){
    m[rows+j,i] = mat[i,j] 
  }
}
m
mat
#graph=graph_from_adjacency_matrix(m, mode = c("directed", "undirected", "max", "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)
graph=graph_from_adjacency_matrix(m, mode = "undirected", weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)

graph
assortativity_degree(graph) #sale valor casi de 1, osea que habr??a assortativity

#Si lo he entendido bien, el output de esta funci??n es un Pearson correlation coefficient, lying in range -1 <=r<=1. If it is implemented as stated in the paper, 
#the value itself has statistical significance, but I'm not sure. I think we should create randomized networks using the degree. 
#I don't know either if this measure applies for bipartite networks, which I have converted to a square matrix to be able to take the measure...

#SEE PAPER: M. E. J. Newman: Assortative mixing in networks, Phys. Rev. Lett. 89, 208701 (2002) http: //arxiv.org/abs/cond-mat/0205405/

#from igraph package
# random network, close to zero
assortativity_degree(sample_gnp(10000, 3/10000))
# BA model, tends to be dissortative
assortativity_degree(sample_pa(10000, m=4))




##FROM SALVA. JUST PLOTTING ACCORDING TO THE DEFINITION OF ASSORTATIVITY
m=mat
length=nrow(m)
length
assort_A<-vector(length=nrow(m))
assort_A
assort_num_A<-vector(length=nrow(m))
assort_num_A
assort_F<-vector(length=ncol(m))
assort_F
assort_num_F<-vector(length=ncol(m))
assort_num_F
total_cols<-colSums(m)
total_cols
total_rows<-rowSums(m)
total_rows
for (i in 1:nrow(m)){
  assort_num_A[total_rows[i]]<-assort_num_A[total_rows[i]]+1
  print(i)
  for (j in 1:ncol(m)){
    assort_A[total_rows[i]]<-assort_A[total_rows[i]]+m[i,j]*total_cols[j]
  }
}

for (i in 1:length(assort_A)){
  assort_A[i]<-assort_A[i]/(assort_num_A[i]*i)
}

#FUNGI

for (i in 1:ncol(m)){
  assort_num_F[total_cols[i]]<-assort_num_F[total_cols[i]]+1
  print(i)
  for (j in 1:nrow(m)){
    assort_F[total_cols[i]]<-assort_F[total_cols[i]]+m[j,i]*total_rows[j]
  }
}

for (i in 1:length(assort_F)){
  assort_F[i]<-assort_F[i]/(assort_num_F[i]*i)
}

plot(assort_F[1:130])
#OK, IT DOESN'T WORK!! I'll talk to you tomorrow Salva, do you think it is the final code? I don't get what you are doing at a first glance
