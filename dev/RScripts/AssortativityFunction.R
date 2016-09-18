#Si lo he entendido bien, el output de esta funci??n es un Pearson correlation coefficient, lying in range -1 <=r<=1. If it is implemented as stated in the paper, 
#the value itself has statistical significance, but I'm not sure. I think we should create randomized networks using the degree. 
#I don't know either if this measure applies for bipartite networks, which I have converted to a square matrix to be able to take the measure...

#SEE PAPER: M. E. J. Newman: Assortative mixing in networks, Phys. Rev. Lett. 89, 208701 (2002) http: //arxiv.org/abs/cond-mat/0205405/
library(igraph)
library(bipartite)

ComputeAssortativity <-function(mat){
  m=fromBipartiteToSquareMatrix(mat)
  graph=graph_from_adjacency_matrix(m, mode = "undirected", weighted = T, diag = TRUE, add.colnames = NULL, add.rownames = NA)
  assort_degree=assortativity_degree(graph) 
  return(assort_degree)
}

fromBipartiteToSquareMatrix <-function(mat){
  rows=nrow(mat)
  cols=ncol(mat)
  m<-matrix(0,nrow=rows+cols,ncol=rows+cols)
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
  return(m)
}

#See Assortativity.R for Salva's function, which I haven't found the way to make it work!
