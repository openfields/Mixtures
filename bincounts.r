bincounts<-function(datas, range){

  #if(is.na(range)){
  #min.d<-min(datas)
  #max.d<-max(datas)
  #ob.range<-seq(from=min.d, to=max.d)
  #}
  min.d<-min(range)
  max.d<-max(range)
  ob.range<-seq(from=min.d, to=max.d)
  matrix(0, nrow=length(ob.range), ncol=2)->ob.mat
  ob.mat[,1]<-ob.range                                     

  for (n in 1:length(ob.range)){
    which(datas<ob.mat[n,1]+0.5)->tmpind
    length(which(datas[tmpind]>ob.mat[n,1]-0.5))->ob.mat[n,2]
  }

return(ob.mat)
}