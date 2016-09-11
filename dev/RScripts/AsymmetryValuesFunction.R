#the function reads the matrix as it is read by your function cooccurenceMat(). I think this should be called by the function GetMatProperties()

#TEST MATRIX: this is a test matrix if you wanna get the intuition about these properties measured.
#testmat <-matrix(0,nrow=4,ncol=50)
#testmat[1,1] = 2
#testmat[2,1] = 1
#testmat[2,2] = 3
#testmat[2,3] = 4
#testmat[2,4] = 5
#testmat[3,4] = 10
#testmat[4,4] = 3
#testmat[4,5] = 4
#testmat
#
#ComputeAssymetryValues(testmat)


ComputeAssymetryValues<-function(mat){ 
  #It could be good here to check if there is colnames and rownames and that they are unique if not ALL IS FUCKED UP! 

  #strength_nodes and degree_nodes can be written to the csv file with the nodes properties you already have constructed (if I am not wrong)
  # ,y="strength"However the vectors vA, vP and vAS_abs will have size nrow(mat)xncol(mat). I don't know which will be the best way to store it. 
  #the frequency plots need to be of vA and vP separated, and vAS_abs. 
  # See paper "Asymmetric Coevolutionary Networks Facilitate Biodiversity Maintenance. Bascompte et al 2006"

  matA=ComputematA(mat)
  matP=ComputematP(mat)
  vA=as.vector(t(matA))
  vP=as.vector(t(matP))
  AS=ComputeAS(mat,matP,matA)
  vAS_abs=as.vector(t(abs(AS)))
  strength_nodes = ComputeStrength(mat)  #to plot degree_nodes versus strength_nodes, as it is seen that it does not grow linearly the relationship.
  degreenodes=ComputeDegree(mat)
  return(list("degree"=degreenodes,"strength"=strength_nodes)) 
}

#the three following function could be optimized in only one function i think
#TODO once the optimization is done, integrate to utils.R ?
#Or think about a more robust way to construct full  PACKAGE
ComputematA <- function(mat){
    matA <-matrix(0,nrow=ncol(mat),ncol=nrow(mat))

    col_total= colSums(mat) #this kind of function are really more quick that to do the sum by hand each time
    for (j in 1:ncol(mat)){
	for (i in 1:nrow(mat)){
	    matA[j,i] = mat[i,j]/col_total[j]
	}
    }
    return(matA)
}

ComputematP <- function(mat){
    matP <-matrix(0,nrow=nrow(mat),ncol=ncol(mat))
    row_total= rowSums(mat) 
    for (i in 1:nrow(mat)){
	for (j in 1:ncol(mat)){
	    matP[i,j] = mat[i,j]/row_total[i]
	}
    }
    return(matP)
}

ComputeAS <- function(mat,matP,matA){
  AS <-matrix(0,nrow=nrow(mat),ncol=ncol(mat))
  for (i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      AS[i,j] = (matP[i,j] - matA[j,i])/max(matP[i,j],matA[j,i])
    }
  }
  AS[is.nan(AS)] <- 0
  return(AS)
}

ComputeStrength<-function(mat){
  #Strength_node = s_i = \sum_{j=1}^N a_{ij}w_{ij}. from Barrat et al PNAS 2004 The architecture of complex weighted networks
  strength_rows = numeric(nrow(mat))
  strength_cols = numeric(ncol(mat))
  for(i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      strength_rows[i] = strength_rows[i] + mat[i,j] 
    }
  }
  for(j in 1:ncol(mat)){
    for (i in 1:nrow(mat)){
      strength_cols[j] = strength_cols[j] + mat[i,j] 
    }
  }
  strength_cols = cbind.data.frame("strength"=strength_cols,"node_id"=colnames(mat))
  strength_rows = cbind.data.frame("strength"=strength_rows,"node_id"=rownames(mat))
  strength_merged = rbind(strength_rows,strength_cols)
  return(strength_merged)
}

ComputeDegree<-function(mat){
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
  cols_degree = cbind.data.frame("degree"=cols_degree,"node_id"=colnames(mat))
  rows_degree = cbind.data.frame("degree"=rows_degree,"node_id"=rownames(mat))
  degree_merged = rbind(rows_degree,cols_degree)
  return(degree_merged)
}
