library(bipartite)
library(vegan)
library(network)
library(igraph)
library(gplots)

#CODE PREPARED TO ANALYSE THE NETWORKS, JUST DONE WITH ONE AS AN EXAMPLE
datafolder="/Users/aina/Dropbox/Lichen-s-project/data/cooc_mat/"
datafolder
temp=read.csv(paste(datafolder,"mat_pop-4.csv",sep=""), header = TRUE, sep = ",")
temp1=temp[,2:ncol(temp)]
mat <- as.matrix(temp1)
dimnames(mat) <- NULL
mat

#TEST MATRIX
#mat <-matrix(0,nrow=4,ncol=5)
#mat[1,1] = 2
#mat[2,1] = 1
#mat[2,2] = 3
#mat[2,3] = 4
#mat[2,4] = 5
#mat[3,4] = 10
#mat[4,4] = 3
#mat[4,5] = 4
#mat

matP <-matrix(0,nrow=nrow(mat),ncol=ncol(mat))
matP
matA <-matrix(0,nrow=ncol(mat),ncol=nrow(mat))
matA

#compute matP, dependence of fungi i on algae j (fraction of all links of node i coming from node j)
for (i in 1:nrow(mat)){
  total_i = 0
  for (j in 1:ncol(mat)){
    total_i = total_i + mat[i,j]
  }
  for (j in 1:ncol(mat)){
    matP[i,j] = mat[i,j]/total_i
  }
}
matP

#compute matA, dependence of algae j on fungi i (fraction of all links of node j coming from node i)
for (j in 1:ncol(mat)){
  total_j = 0
  for (i in 1:nrow(mat)){
    total_j = total_j + mat[i,j]
  }
  for (i in 1:nrow(mat)){
    matA[j,i] = mat[i,j]/total_j
  }
}
matA

#Calculate assymetry matrix AS(i,j) = (d_ij^P -d_ij^A)/max(d_ij^P,d_ij^A)

AS <-matrix(0,nrow=nrow(mat),ncol=ncol(mat))
for (i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    AS[i,j] = (matP[i,j] - matA[j,i])/max(matP[i,j],matA[j,i])
  }
}
AS[is.nan(AS)] <- 0
AS
AS_abs <-AS
AS_abs
for (i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
  if (AS_abs[i,j] < 0){AS_abs[i,j] = AS_abs[i,j]*-1}
      }
}
AS_abs

#Calculate species strength (see Barrat)
strength_rows = numeric(nrow(mat))
strength_cols = numeric(ncol(mat))
for(i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    strength_rows[i] = strength_rows[i] + mat[i,j] 
  }
}
strength_rows
for(j in 1:ncol(mat)){
  for (i in 1:nrow(mat)){
    strength_cols[j] = strength_cols[j] + mat[i,j] 
  }
}
strength_cols
strength_merged = c(strength_rows,strength_cols)
strength_merged

#Compute nodes degree
rows_degree = numeric(nrow(mat))
cols_degree = numeric(ncol(mat))
for(i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    if (mat[i,j]!=0){rows_degree[i] = rows_degree[i] + 1}  
  }
}
for(j in 1:ncol(mat)){
  for (i in 1:nrow(mat)){
    if (mat[i,j]!=0){cols_degree[j] = cols_degree[j] + 1}  
  }
}
rows_degree
cols_degree
degree_merged = c(rows_degree,cols_degree)
degree_merged

#PLOT FREQUENCY DISTRIBUTIONS OF DEPENDENCE (from matrices A and P)
dependence_values = c(as.vector(t(matA)),as.vector(t(matP)))
dependence_values
hist(dependence_values,breaks=5)

#PLOT FREQUENCY DISTRIBUTIONS OF ASYMMETRY VALUES (from the absolute value of matrix AS)
hist(as.vector(t(AS_abs)))

#PLOT SPECIES DEGREE VERSUS SPECIES STRENGTH
plot(degree_merged,strength_merged)

