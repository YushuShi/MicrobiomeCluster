\name{cumAbunofHighAbun}
\alias{cumAbunofHighAbun}
\docType{package}
\title{
  Cumulative Abundance of High Abundance OTUs.
}
\description{
  Cumulative Abundance of High Abundance OTUs. Here the threshold of the high abundance OTU is set to be 0.001. A low function output suggests Bray Curtis may perform poorly in recovering the true sample clusters.}
\usage{
  cumAbunofHighAbun(otuabundance,threshold=0.001)  
}

\arguments{
  \item{otuabundance}{An OTU abundance table}
  \item{threshold}{The threshold of the high abundance OTU. The default is 0.001.}
}

\source{
Yushu Shi,Liangliang Zhang,Christine Peterson, Kim-Anh Do, Robert Jenq \emph{Combined Beta Metric for Unsupervised Clustering of Microbiome Data} \emph{Manuscript submitted}}

\value{
Sum of the abundances of OTUs with average abundance greater than the threshold.
}

