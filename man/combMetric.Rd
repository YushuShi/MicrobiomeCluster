
\name{combMetric}
\alias{combMetric}
\docType{package}
\title{
  Calculate combined metrics of the Bray Curtis dissimilarity and the unweighted UniFrac dissimilarity
}
\description{
  Calculate combined metrics, which inherits complimentary insights from the Bray Curtis and the Unweighted UniFrac dissimilarities
}
\usage{
  combMetric(bray,uu,alpha)  
}

\arguments{
  \item{bray}{Bray Curtis distance matrix, with row names and column names correspond to the sample ID.}
  \item{uu}{Unweighted UniFrac distance matrix, with row names and column names correspond to the sample ID.}
  \item{alpha}{Relative contribution of the unweighted Unifrac distance.}
}
\details{
\deqn{d^{combined}=alpha*d^{UU}_{normalized}+(1-alpha)*d^{BC}_{normalized}}
}
\source{
Yushu Shi, Liangliang Zhang, Christine B. Peterson, Kim-Anh Do, Robert R. Jenq \emph{Performance determinants of
unsupervised clustering methods for microbiome data}  \emph{Accepted by Microbiome}}

\value{
 combined metric distance matrix
}

\examples{

library(MicrobiomeCluster)
data("Schnorr")

temp<-GUniFrac(t(Schnorr$otutable),Schnorr$tree)
uu<-as.dist(temp$unifracs[,,"d_UW"])  
bray<-as.matrix(vegdist(t(Schnorr$otutable),"bray"))
combM<-combMetric(uu,bray)
}
