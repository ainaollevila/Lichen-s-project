tagsnum=2048

#I KEEP IT, just in case we want to test specific matrices, as there are A LOT! if we want to test all of them, just 
#R <- c("mutualism","parasitism")
#C <- c("michaelis-menten","sigmoid")
#ticks <-seq(from = 5000, to = 100000, by = 5000)
#sex <- c(1, 5, 10, 50, 100)
#repl <- c("A","B","C","D","E")
#length(repl)
#filename=paste(R[i],"_",C[j],"_",ticks[k],"ticks_",sex[l],"sexualreproduction_replicate",repl[m],sep="")

#for (i in 1:2){
#  for(j in 1:2){
#    for(k in 1:length(ticks)){
#      for(l in 1:length(sex)){
#        for(m in 1:length(repl)){
#          
#        }
#      }
#    }
#  }
#}

datafolder="../data/"
i=1
filename=paste(datafolder,"data_observed_pairs_t",i,".txt",sep="")

for (i in 1:8){
  i=1
  filename=paste(datafolder,"data_observed_pairs_t",i,".txt",sep="")
  m = edgelisttomatrix(filename,0)  #1 if you want the weighted matrix, 0 otherwise
  mat = removingzeros(m) #removing the rows and cols = 0
  fileout = paste(datafolder,"m",i,".txt",sep="")
  write.table(mat, fileout, row.names=FALSE, col.names=FALSE)
}

for (l in list.files(datafolder,pattern="*reproduction*"))
{
  filename=paste(datafolder,l,sep="")
  m = edgelisttomatrix(filename,0)  #1 if you want the weighted matrix, 0 otherwise
  mat = removingzeros(m) #removing the rows and cols = 0
  fileout=paste(datafolder,"Matrix_",l,sep="")
  write.table(mat, fileout, row.names=FALSE, col.names=FALSE)
}


edgelisttomatrix <-function(filename,weighted){
  #el=read.table(filename)
  el=read.csv(filename,header = FALSE, sep = ",")
  m <-matrix(0,nrow=tagsnum,ncol=tagsnum)
  for (i in 1:nrow(el)){
    origin = el[i,1]    
    dest = el[i,2]
    weighted = 0
    if (weighted == 1) {m[origin,dest] = m[origin,dest] + 1}
    else {m[origin,dest] = 1}
  }
  return(m)
}

removingzeros <-function(salva_matrix){
  bool_zero_rows<-rep(0,tagsnum)
  length(bool_zero_rows)
  bool_zero_cols<-rep(0,tagsnum)
  length(bool_zero_cols)
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
  
  count_cols = 0
  count_rows = 0
  for (i in 1:tagsnum){
    if (bool_zero_cols[i] == 0){
      count_cols = count_cols + 1
    }
  }
  for (i in 1:tagsnum){
    if (bool_zero_rows[i] == 0){
      count_rows = count_rows + 1
    }
  }
  salva_corrected <- array(0, dim=c(count_rows,count_cols)) #puc ficar-ho manera nova que s??
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



#SIMON METHOD!!! Infinitely more optimal

for (l in list.files(datafolder,pattern="*reproduction*"))
{
  filename=paste(datafolder,l,sep="")
  data_model=read.csv(filename,header=F)
  colnames(data_model)=c("A","F","x","y")
  data_model
  M=cooccurenceModel(data_model)
  fileout=paste(datafolder,"MatrixS_",l,sep="")
  write.table(mat, fileout, row.names=FALSE, col.names=FALSE)
}

cooccurenceModel <- function(datas){
  print(unique(datas[,1]))
  res=matrix(0,nrow=length(unique(datas$A)),ncol=length(unique(datas$F)))
  
  rownames(res)=unique(datas$A)
  colnames(res)=unique(datas$F)
  
  print (res)
  for(i in 1:nrow(datas)){
    popiMlgF=as.character(datas[i,1])
    popiMlgA=as.character(datas[i,2])
    res[popiMlgF,popiMlgA]=res[popiMlgF,popiMlgA]+1
  }
  res
  return(res)
  
}


#vec_files <- list.files(datafolder,pattern="*reproduction*")

#filename=paste(datafolder,vec_files[60],sep="")
#filename
