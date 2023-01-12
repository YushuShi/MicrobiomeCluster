\name{growBranch}
\alias{growBranch}
\docType{package}
\title{
  Bifurcate each tree tip and make a new tree.
}
\description{
 Bifurcate each tree tip and make a new tree. Sequences of each OTU are randomly assigned to either of the two newly formed tips.

}
\usage{
  growBranch(tree, otutable)  
}

\arguments{
  \item{tree}{A tree object.}
  \item{otutable}{An OTU table}

}

\source{
Yushu Shi, Liangliang Zhang, Christine B. Peterson, Kim-Anh Do, Robert R. Jenq \emph{Performance determinants of
unsupervised clustering methods for microbiome data}  \emph{Accepted by Microbiome}}
\value{
  \item{tree}{A new tree created from bifurcating the old tree tips.}
  \item{otutable}{A new OTU table after assigning sequences to the new tree tips.}
}

\examples{
library(MicrobiomeCluster)
library(vegan)
library(ape)
library(mclust)
library(cluster)
data("Martinez")

# martinez1 and martinez2 are the OTU tables of the first and second generation descendants
  
originalbray <- vegdist(t(Martinez$otutable), method="bray")
martinez1 <- growBranch(Martinez$tree, Martinez$otutable)
bray1 <- vegdist(t(martinez1$otutable), method="bray")
martinez2 <- growBranch(martinez1$tree, martinez1$otutable)
bray2 <- vegdist(t(martinez2$otutable), method="bray")

# The performance of Bray Curtis deteriorates as tree tips bifurcate. 
 
adjustedRandIndex(pam(originalbray, 2, diss=TRUE)$cluster, Martinez$sampleinfo)
adjustedRandIndex(pam(bray1, 2, diss=TRUE)$cluster, Martinez$sampleinfo)
adjustedRandIndex(pam(bray2, 2, diss=TRUE)$cluster, Martinez$sampleinfo)

# plot the PCOA plots of the original data, 
# the first generation descendants and the second generation descendants
pcoaorig <- pcoa(originalbray)
pcoa1 <- pcoa(bray1)
pcoa2 <- pcoa(bray2)

col <- ifelse(Martinez$sampleinfo=="USA", "red", "blue")
par(mfrow=c(1, 3))
plot(pcoaorig$vectors[,1], pcoaorig$vectors[,2], col=col)
plot(pcoa1$vectors[,1], pcoa1$vectors[,2], col=col,
xlim=c(-0.25,0.45), ylim=c(-0.4,0.35))
plot(pcoa2$vectors[,1], pcoa2$vectors[,2], col=col, 
xlim=c(-0.25,0.45), ylim=c(-0.4,0.35))

}