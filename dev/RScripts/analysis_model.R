
data_model=read.csv("../../dev/data/mutualism_michaelis-menten_10000ticks_10sexualreproduction_replicateA.dat",header=F)

colnames(data_model)=c("A","F","x","y")

getCorrespLichensModel <- function(rawdata,genotype,type,pop=""){
    print(genotype)
    print(type)
    print(pop)
    res=rawdata
    if(pop!=""){res=res[res$Population == pop,]}

    if(type=="A"){
	res=res[as.character(res[,2]) == as.character(genotype),]
    }

    if(type=="F"){
	res=res[as.character(res[,1]) == as.character(genotype),]
    }
    return(res)
}

cooccurenceModel <- function(datas){
	print(unique(datas[,1]))
    res=matrix(0,nrow=length(unique(datas$V1)),ncol=length(unique(datas$V2)))

    rownames(res)=unique(datas$V1)
    colnames(res)=unique(datas$V2)

    for(i in 1:nrow(datas)){
	popiMlgF=as.character(datas[i,1])
	popiMlgA=as.character(datas[i,2])
	res[popiMlgF,popiMlgA]=res[popiMlgF,popiMlgA]+1
    }
    return(res)

}

#	   fungusId=rownames(mat) 
#	   algaeId=colnames(mat) 
#	   coln=c("node_id","type")
#	   alg=cbind.data.frame(algaeId,"A",stringsAsFactors=F)
#	   colnames(alg)=coln
#	   fung=cbind.data.frame(fungusId,"F",stringsAsFactors=F)
#	   colnames(fung)=coln
#	   join=  rbind(alg,fung)
#	   join$node_id=as.character(join$node_id)
#
#	   web2edges(mat,is.one.mode=F,verbose=TRUE,both.directions=T)
#	   edgelist=read.table("web.pairs",sep="\t")
#	   idkeyTable=read.table("web-names.lut",sep="\t",header=T,as.is=2,numerals="no.loss")
#	   ##The read table here is primordial. Without the no.loss (that somehowe avoid some cut when the integer is computed) lot of errors emerge. The as.is is here jsute to avoid factor and keep number as number.
#
#
#	   
#	   #compute different metrics
#	   betweenness_netw = betweenness_w(edgelist,directed=NULL,alpha=1)
#	   closeness_netw = closeness_w(edgelist, directed = FALSE, gconly=TRUE, alpha=1)
#
#
#	   #compute matrix of spatial distance beetween every similar MLG
#		      
#	   spatial=apply(join,1,function(n){
#			    gen=n[["node_id"]]
#			    subst=getCorrespLichens(rawdata=fullD,pop=ind,type=n[["type"]],genotype=gen)
#			    dist_mat=dist(subst[,c("x","y")])
#			    c(mean(dist_mat), median(dist_mat),mad(dist_mat),min(dist_mat),max(dist_mat),sd(dist_mat))
#	})
#	dist_mat=dist(mat)
#			    c(mean(dist_mat), median(dist_mat),mad(dist_mat),min(dist_mat),max(dist_mat),sd(dist_mat))
#	   spatial=t(spatial)
#	   colnames(spatial)=c("mean_dist","median_dist","mad","min_dist","max_dist","sd_dist")
#	
#	   join=cbind(join,spatial)
#	   #norm_netw=ND(edgelist,normalised=T)
#	   #....
#
#	   #prepare the merging of those metrics
#	   clsn_withid=merge(idkeyTable,closeness_netw,by.x="virtual",by.y="node")
#	   btwn_withid=merge(idkeyTable,betweenness_netw,by.x="virtual",by.y="node")
#
#
#	   #merge it
#	   join=merge(join,btwn_withid[,c("real","betweenness")],by.x="node_id",by.y="real",all.x=T)
#	   join=merge(join,clsn_withid[,c("real","closeness","n.closeness")],by.x="node_id",by.y="real",all.x=T)
#
#	   wholeset<<-rbind(wholeset,join)
