dist2Root<-function(tree){
  tree$node.label<-paste("CROTU",1:length(tree$node.label))
  tree$root.edge<-0
  tiplist<-listTips(tree)
  tier<-list(length(tree$tip.label)+1)
  tier<-c(tier,list(tree$edge[tree$edge[,1]==(length(tree$tip.label)+1),2]))
  for(i in 3:length(tree$node.label)){
    tier<-c(tier,list(tree$edge[tree$edge[,1] %in% tier[[i-1]],2]))
  }
  tier<-tier[lapply(tier,sum)>0]
  distance<-rep(0,max(tree$edge))
  for(i in 1:length(tier)){
    namelist<-tier[[i]]
    namelist2<-tree$edge[tree$edge[,1] %in% namelist,2]
    distance[namelist2]<-tree$edge.length[tree$edge[,1] %in% namelist]
  }
  otudistance<-distance[1:length(tree$tip.label)]
  names(otudistance)<-tree$tip.label
  crotudistance<-distance[(1+length(tree$tip.label)):length(distance)]
  names(crotudistance)<-tree$node.label
  list(crotudistance=crotudistance,otudistance=otudistance)
}
