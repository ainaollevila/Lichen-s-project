library(bipartite)
library(vegan)

datafolder="/Users/aina/Dropbox/Lichen-s-project/dev/data/Matrices/"
files <-c()
for (l in list.files(datafolder,pattern="*reproduction*")){
  files <-c(files,l)
}
files
files[1]
filename=paste(datafolder,files[1],sep="")
A=read.table(filename)
#oecusimu
#nullmodel
#commsim()
#nestednodf()


nestednodf(A, order = TRUE, weighted = FALSE, wbinary = FALSE)

oecosimu(A, nestednodf, "r00", nsimul = 100, burnin = 0, thin = 1,
         statistic = "statistic", alternative = "greater",
         batchsize = NA, parallel = 1)

datafolder="/Users/aina/Dropbox/Lichen-s-project/data/cooc_mat/"
write("Nestedness",file="/Users/aina/Desktop/nestedness.txt",append=FALSE)
sink('/Users/aina/Desktop/nestedness.txt')
for (l in list.files(datafolder,pattern="mat_pop*")){
  
  filename=paste(datafolder,l,sep="")
  A=readMatrix(filename)
  N=oecosimu(A, nestednodf, "r00", nsimul = 100, burnin = 0, thin = 1,
           statistic = "statistic", alternative = "greater",
           batchsize = NA, parallel = 1)
  print(l)
  print(N)
}
sink()

l=1
r=2
write(l,file="/Users/aina/Desktop/nestedness.txt",append=TRUE)
write(r,file="/Users/aina/Desktop/nestedness.txt",append=TRUE)
filename=paste(datafolder,"mat_pop-9.csv",sep="")
A=readMatrix(filename)
M
#Take the measure to matrices from real data (small ones)
readMatrix<-function(filename){
  temp=read.csv(filename, header = TRUE, sep = ",") #this relative adress will be good for every body willing to use the script 
  temp1=temp[,2:ncol(temp)]
  return(as.matrix(temp1))
}















#from terminal, OUTPUT
> oecosimu(A, nestednodf, "r00", nsimul = 100, burnin = 0, thin = 1,
           + statistic = "statistic", alternative = "greater",
           + batchsize = NA, parallel = 1)
oecosimu object

Call: oecosimu(comm = A, nestfun = nestednodf, method = "r00", nsimul = 100, burnin = 0, thin = 1, statistic = "statistic", alternative = "greater", batchsize = NA,
               parallel = 1)

nullmodel method 'r00' with 100 simulations

alternative hypothesis: statistic is greater than simulated values

N columns  : 0.5379 
N rows     : 0.6341 
NODF       : 0.5937 
Matrix fill: 0.002961 

statistic  SES  mean   50%  95% Pr(sim.)   
N.columns     0.538 43.0 0.329 0.328 0.34   0.0099 **
  N.rows        0.634 72.9 0.326 0.326 0.33   0.0099 **
  NODF          0.594 67.8 0.327 0.327 0.33   0.0099 **
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Warning message:
  In oecosimu(A, nestednodf, "r00", nsimul = 100, burnin = 0, thin = 1,  :
                nullmodel transformed 'comm' to binary data