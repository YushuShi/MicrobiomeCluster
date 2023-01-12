\name{switch2Zero}
\alias{switch2Zero}
\docType{package}
\title{
    Convert OTU counts below the threshold to 0.
}
\description{
  Convert OTU counts below the threshold to 0. This helps recovering the true clustering for unweighted Unifrac distance.
}
\usage{
switch2Zero(otutable,threshold) 
}

\arguments{
  \item{otutable}{An OTU table}
  \item{threshold}{Desired threshold.}
}

\source{
Yushu Shi, Liangliang Zhang, Christine B. Peterson, Kim-Anh Do, Robert R. Jenq \emph{Performance determinants of
unsupervised clustering methods for microbiome data}  \emph{Accepted by Microbiome}}
\value{
  \item{otutable}{A new OTU table after coverting counts below threshold to 0}
}

\examples{
library(MicrobiomeCluster)
library(ape)
library(GUniFrac)
library(mclust)
library(cluster)

data("Smits")

uuorig <- as.dist(GUniFrac(t(Smits$otutable), Smits$tree)$unifracs[ , ,"d_UW"])
newsmits <- switch2Zero(Smits$otutable, 30)
uu1 <- as.dist(GUniFrac(t(newsmits), Smits$tree)$unifracs[ , ,"d_UW"])

# The performance of unweighted Unifrac improves after we convert counts below 30 to 0

adjustedRandIndex(pam(uuorig, 2, diss=TRUE)$cluster, Smits$sampleinfo)
adjustedRandIndex(pam(uu1, 2, diss=TRUE)$cluster, Smits$sampleinfo)

# Plot the PCOA plot for the original data and the modified data.
pcoaorig <- pcoa(uuorig)
pcoa1 <- pcoa(uu1)

col <- ifelse(Smits$sampleinfo=="Early Wet", "red", "blue")
par(mfrow=c(1,2))
plot(pcoaorig$vectors[ ,1], pcoaorig$vectors[ ,2], col=col,
xlim=c(-0.4,0.4), ylim=c(-0.35,0.35))
plot(pcoa1$vectors[ ,1], pcoa1$vectors[ ,2], col=col,
xlim=c(-0.4,0.4), ylim=c(-0.35,0.35))


}
