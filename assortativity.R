m<-matrix(0,nrow=4,ncol=4)
m[1,2]=1
m[1,3]=1
m[2,1]=1
m[2,2]=1
m[3,3]=1
m[4,4]=1
m[4,3]=1
m[4,1]=1

m<-read.csv("Desktop/LICHEN_PROJECT/mutualism_sigmoid/data_observed_pairs_mutualism2_t2.csv", sep = ",")
m<-read.csv("Downloads/coocurrence_matrix.csv", sep = ";")

assort_A<-vector(length=nrow(m))
assort_num_A<-vector(length=nrow(m))
assort_F<-vector(length=ncol(m))
assort_num_F<-vector(length=nrow(m))
total_cols<-colSums(m)
total_rows<-rowSums(m)

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
