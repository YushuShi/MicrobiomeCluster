switch2Zero<-function(otutable,threshold){
  rown<-rownames(otutable)
  coln<-colnames(otutable)
  otutable<-as.matrix(otutable)
  nrow<-nrow(otutable)
  mani<-ifelse(otutable<rep(threshold,length(otutable)),rep(0,length(otutable)),otutable)
  otutable<-data.frame(matrix(mani,nrow=nrow))
  rownames(otutable)<-rown
  colnames(otutable)<-coln
  otutable
}

