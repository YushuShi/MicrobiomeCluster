\name{trimBranch}
\alias{trimBranch}
\docType{package}
\title{
  Trim tree tips and make a new tree.
}
\description{
 Trim the tree tips to the given value. Sequences from trimmed OTUs are combined to the upper level.  
 }
\usage{
  trimBranch(level, tree, otutable)  
}

\arguments{
  \item{level}{Desired maximum hierarchy level of the output tree.}
  \item{tree}{A tree object.}
  \item{otutable}{An OTU table.}
}

\source{
Yushu Shi, Liangliang Zhang, Christine B. Peterson, Kim-Anh Do, Robert R. Jenq \emph{Performance determinants of
unsupervised clustering methods for microbiome data}  \emph{Accepted by Microbiome}}
\value{
  \item{tree}{A new tree created from trimming distant tips and nodes.}
  \item{otutable}{A new OTU table after combining sequences that are trimmed to the upper level.}
}

\examples{
library(MicrobiomeCluster)
library(mclust)
library(cluster)
library(vegan)
library(ape)

data("Schnorr")

#High abundance OTUs control the performance of the Bray Curtis distance.

SchnorrAbundance <- convCounts2Abun(Schnorr$otutable)
cumAbunofHighAbun(SchnorrAbundance)

originalbray <- vegdist(t(Schnorr$otutable), method="bray")
SchnorrTrimmed <- trimBranch(37,Schnorr$tree,Schnorr$otutable)

SchnorrTrimmedAbundance <- convCounts2Abun(SchnorrTrimmed$otutable)
cumAbunofHighAbun(SchnorrTrimmedAbundance)

trimmedbray <- vegdist(t(SchnorrTrimmed$otutable), method="bray")

#The performance of Bray Curtis improves after trimming distant branches.

adjustedRandIndex(pam(originalbray,2,diss=TRUE)$cluster, Schnorr$sampleinfo)
adjustedRandIndex(pam(trimmedbray,2,diss=TRUE)$cluster, Schnorr$sampleinfo)

#plot the PCOA plots of the original data and the modified data

pcoaorig <- pcoa(originalbray)
pcoatrimmed <- pcoa(trimmedbray)

col <- ifelse(Schnorr$sampleinfo=="Italy", "red", "blue")
par(mfrow=c(1,2))
plot(pcoaorig$vectors[ ,1],pcoaorig$vectors[ ,2], col=col,
     xlim=c(-0.4,0.45), ylim=c(-0.4,0.45))
plot(pcoatrimmed$vectors[ ,1],pcoatrimmed$vectors[ ,2], col=col,
     xlim=c(-0.4,0.45), ylim=c(-0.4,0.45))
}

