\name{level2Root}
\alias{level2Root}
\docType{package}
\title{
  Clculate level of tree tips/nodes to the root.
}
\description{
  Calculate level of tree tips/nodes to the root.
}
\usage{
  level2Root(tree)  
}

\arguments{
  \item{tree}{A tree object.}
}
\details{
  Here we adopt the name "CROTU" for tree node following the convention of Peled et al. The level refers to the number of branches from the root to the CROTU or OTU.
}
\source{
Jonathan U. Peled, Sean M. Devlin, Anna Staffas, Melissa Lumish, Raya Khanin, Eric R. Littmann, Lilan Ling, Satyajit Kosuri, Molly Maloy, John B. Slingerland, Katya F. Ahr, Kori A. Porosnicu Rodriguez, Yusuke Shono, Ann E. Slingerland, Melissa D. Docampo, Anthony D. Sung, Daniela Weber, Amin M. Alousi, Boglarka Gyurkocza, Doris M. Ponce, Juliet N. Barker, Miguel-Angel Perales, Sergio A. Giralt, Ying Taur, Eric G. Pamer, Robert R. Jenq, and Marcel R.M. van den Brink (2017) Intestinal Microbiota and Relapse After Hematopoietic-Cell Transplantation, \emph{Journal of Clinical Oncology}, 1650-1659
}
\value{
  \item{otulevel}{Level of each OTU}
  \item{crotulevel}{Level for each phylogenetic tree node (CROTU)}
}

