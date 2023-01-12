calNodeAbun<-function(tree,AvgAbun){
  tree$node.label<-paste("CROTU",1:length(tree$node.label))
  tree$root.edge<-0
  tiplist<-listTips(tree)
  crotusum<-rep(0,tree$Nnode)
  names(crotusum)<-tree$node.label
  for(i in tree$node.label){
    temp<-names(unlist(tiplist[i]))
    temp<-gsub(paste(i,".",sep=""),"",temp)
    crotusum[i]<-sum(AvgAbun[temp],na.rm = TRUE)
  }
  crotusum
}