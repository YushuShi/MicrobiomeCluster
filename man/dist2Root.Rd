
\name{dist2Root}
\alias{dist2Root}
\docType{package}
\title{
  Calculate distance from tree tips/nodes to root
}
\description{
  Calculate distance from tree tips/nodes to root
}
\usage{
  dist2Root(tree)  
}

\arguments{
  \item{tree}{A tree object.}
}
\details{
  Here we adopt the name "CROTU" for tree node following the convention of Peled et al. Distance refers to the sum of branch lengths.
}
\source{
Jonathan U. Peled, Sean M. Devlin, Anna Staffas, Melissa Lumish, Raya Khanin, Eric R. Littmann, Lilan Ling, Satyajit Kosuri, Molly Maloy, John B. Slingerland, Katya F. Ahr, Kori A. Porosnicu Rodriguez, Yusuke Shono, Ann E. Slingerland, Melissa D. Docampo, Anthony D. Sung, Daniela Weber, Amin M. Alousi, Boglarka Gyurkocza, Doris M. Ponce, Juliet N. Barker, Miguel-Angel Perales, Sergio A. Giralt, Ying Taur, Eric G. Pamer, Robert R. Jenq, and Marcel R.M. van den Brink (2017) Intestinal Microbiota and Relapse After Hematopoietic-Cell Transplantation, \emph{Journal of Clinical Oncology}, 1650-1659}
  
\value{
  \item{otudistance}{A vector of distance from OTUs to the root}
  \item{crotudistance}{A vector of distance from phylogenetic tree nodes (CROTUs) to the root}
}

