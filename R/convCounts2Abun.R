#otutable to otu abundance

convCounts2Abun<-function(otucounttable){
colsum<-apply(otucounttable,2,sum)
colmax<-matrix(rep(colsum,each=nrow(otucounttable)),nrow=nrow(otucounttable))
otu.abundance.table<-otucounttable/colmax
}
