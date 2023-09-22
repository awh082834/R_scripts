library(ape)
library(data.table)

data <- read.table("path",header = TRUE , sep = "\t")
data2 <- setNames(data.frame(t(data[ , - 1])), data[ , 1])
data3 <- t(data2)
data4 <- as.matrix(data3)
tr <- nj(data4)
fix_negative_edge_length <- function(nj.tree) {
  edge_infos <- as.data.table(cbind(nj.tree$edge, nj.tree$edge.length)) 
  colnames(edge_infos) <- c('from', 'to', 'length')
  nega_froms <- edge_infos[length < 0, sort(unique(from))]
  nega_froms
  for (nega_from in nega_froms) {
    minus_length <- edge_infos[from == nega_from, ][order(length)][1, length]
    edge_infos[from == nega_from, length := length - minus_length]
    edge_infos[to == nega_from, length := length + minus_length]
  }
  nj.tree$edge.length <- edge_infos$length
  nj.tree
}
tr <- fix_negative_edge_length(tr)
tr$edge.length[tr$edge.length<0]<-0
write.tree(tr,file = "out")
