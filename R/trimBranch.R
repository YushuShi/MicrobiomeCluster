trimBranch<-function(level,tree,otutable){
  otutable<-as.matrix(otutable)
  treelevel<-level2Root(tree)
  otulevel<-treelevel$otulevel
  totallevel<-c(treelevel$otulevel,treelevel$crotulevel)
  maxlevel<-max(totallevel)
  for(layer in maxlevel:level){
    chosen<-which(totallevel==layer)
    whoatthatlevel<-names(chosen)
    numberofit<-which(c(tree$tip.label,tree$node.label) %in% whoatthatlevel)
    whoconnected<-unique(tree$edge[tree$edge[,2] %in% numberofit,1])
    namesconnected<-tree$node.label[whoconnected-length(tree$tip.label)]
    otutable<-rbind(matrix(0,nrow=length(namesconnected),
                           ncol=ncol(otutable)),otutable)
    
    rownames(otutable)[1:length(namesconnected)]<-namesconnected
    for(count in numberofit){
      connected<-tree$node.label[tree$edge[tree$edge[,2]==count,1]
                                 -length(tree$tip.label)]
      if(length(connected)>0){
        otutable[connected,]<-otutable[connected,]+otutable[c(tree$tip.label,tree$node.label)[count],]
        otutable<-otutable[!rownames(otutable) %in% c(tree$tip.label,tree$node.label)[count],]
      }
    }
  }
  selectall<-names(totallevel)[totallevel<layer]
  newtreetip<-c(tree$tip.label[tree$tip.label %in% selectall],rownames(otutable))
  newtreenode<-tree$node.label[tree$node.label %in% selectall]
  keepnumber<-(1:max(tree$edge))[totallevel<=layer]
  newedge<-NULL
  newtotal<-c(newtreetip,newtreenode)
  newlength<-NULL
  for(i in 1:nrow(tree$edge)){
    if(sum(tree$edge[i,] %in% keepnumber)==2){
      newedge1<-names(totallevel)[tree$edge[i,1]]
      newedge2<-names(totallevel)[tree$edge[i,2]]
       newedge<-rbind(newedge,c(which(newtotal==newedge1),which(newtotal==newedge2)))
      newlength<-c(newlength,tree$edge.length[i])
        }
  }
  tree<-list(edge=newedge,
                tip.label=newtreetip,
                node.label=newtreenode,
                edge.length=newlength,
                root.edge=0,
                Nnode=length(newtreenode))
  class(tree)<-"phylo"
  list(otutable=otutable,tree=tree)
}