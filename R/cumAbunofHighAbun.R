cumAbunofHighAbun<-function(otuabundance,threshold=0.001){
  avgabun<-apply(otuabundance,1,mean)
  avgabun<-avgabun[avgabun>threshold]
  sum(avgabun)
}