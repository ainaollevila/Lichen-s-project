datafolder="~/Dropbox/Lichen-s-project/dev/data/"
datafolder
filename=paste(datafolder,"data_observed_pairs_t1.txt",sep="")
filename=paste(datafolder,"edgelist.txt",sep="")
tagsnum=7

b <- c("mutualism","parasitism")
ticks <-c(1500, 2000)
ticks[1]
for (i in 1:2){
  for(j in 1:2){
    filename=paste(b[i],"_",ticks[j],sep="")
    filename
  }
}

filename=paste(datafolder,"edgelist.txt",sep="")

edgelisttomatrix <-function(filename,weighted){
  el=read.table(filename)
  m <-matrix(0,nrow=tagsnum,ncol=tagsnum)
  for (i in 1:nrow(el)){
    origin = el[i,1]    
    dest = el[i,2]
    if (weighted == 1) {m[origin,dest] = m[origin,dest] + 1}
    else {m[origin,dest] = 1}
  }
  return(m)
}

removingzeros <-function(salva_matrix){
  bool_zero_rows<-rep(0,tagsnum)
  bool_zero_rows
  length(bool_zero_rows)
  bool_zero_cols<-rep(0,tagsnum)
  bool_zero_cols
  length(bool_zero_cols)
  salva_matrix
  
  for (i in 1:tagsnum)
  {
    bool = 1
    for (j in 1:tagsnum)
    {
      if (salva_matrix[i,j] > 0){
        bool = 0
      }
    }
    if (bool == 1)
    {
      bool_zero_rows[i] = 1
    }
    else
    {
      bool_zero_rows[i] = 0
    }
  }
  
  for (j in 1:tagsnum)
  {
    bool = 1
    for (i in 1:tagsnum)
    {
      if (salva_matrix[i,j] > 0){
        bool = 0
      }
    }
    if (bool == 1)
    {
      bool_zero_cols[j] = 1
    }
    else
    {
      bool_zero_cols[j] = 0
    }
  }
  
  bool_zero_cols
  bool_zero_rows
  count_cols = 0
  count_rows = 0
  for (i in 1:tagsnum){
    if (bool_zero_cols[i] == 0){
      count_cols = count_cols + 1
    }
  }
  count_cols
  for (i in 1:tagsnum){
    if (bool_zero_rows[i] == 0){
      count_rows = count_rows + 1
    }
  }
  count_rows
  
  salva_corrected <- array(0, dim=c(count_rows,count_cols)) #puc ficar-ho manera nova que s??
  salva_corrected
  ip=1
  jp=1
  for (i in 1:tagsnum){
    if (bool_zero_rows[i] == 0){
      jp=1
      for (j in 1:tagsnum){
        if (bool_zero_cols[j]==0){
          salva_corrected[ip,jp] = salva_matrix[i,j]
          jp = jp + 1
        }
      }
      ip = ip + 1
    }
  }
  return(salva_corrected)
}

m = edgelisttomatrix(filename,1)  #1 if you want the weighted matrix, 0 otherwise
removingzeros(m) #removing the rows and cols = 0




