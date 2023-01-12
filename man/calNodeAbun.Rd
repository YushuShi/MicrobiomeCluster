\name{calNodeAbun}
\alias{calNodeAbun}
\title{
 Calculate average abundance for phylogenetic tree nodes
}
\description{
 Calculate average abundance for phylogenetic tree nodes
}
\usage{
calNodeAbun(tree, AvgAbun)
}
\arguments{
  \item{tree}{A tree object}
  \item{AvgAbun}{A vector of average OTU abundance. Its names are OTU names.}
}

\value{
A vector of the average abundance for each Tree Node.
}
\details{
  Here we adopt the name "CROTU" for tree node following the convention of 
  Peled et al.
}
\source{
Jonathan U. Peled, Sean M. Devlin, Anna Staffas, Melissa Lumish, Raya Khanin, Eric R. Littmann, Lilan Ling, Satyajit Kosuri, Molly Maloy, John B. Slingerland, Katya F. Ahr, Kori A. Porosnicu Rodriguez, Yusuke Shono, Ann E. Slingerland, Melissa D. Docampo, Anthony D. Sung, Daniela Weber,  Amin M. Alousi, Boglarka Gyurkocza, Doris M. Ponce, Juliet N. Barker, Miguel-Angel Perales, Sergio A. Giralt, Ying Taur, Eric G. Pamer, Robert R. Jenq, and Marcel R.M. van den Brink (2017) Intestinal Microbiota and Relapse After Hematopoietic-Cell Transplantation, \emph{Journal of Clinical Oncology}, vol 35, num 15, 1650-1659

Yushu Shi, Liangliang Zhang, Christine B. Peterson, Kim-Anh Do, Robert R. Jenq \emph{Performance determinants of
unsupervised clustering methods for microbiome data}  \emph{Accepted by Microbiome}}

