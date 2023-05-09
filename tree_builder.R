library(ape)
dir <- getwd()
setwd(dir)

args = commandArgs(trailingOnly = TRUE)
tree <- read.tree(args[1])

el <- tree$edge.length
branches <- sum(el > mean(el) + 2 *sd(el))

pdf(file = args[2])
plotBreakLongEdges(tree, n= branches, font = 3, cex = 0.75)
add.scale.bar(0,0.125)
dev.off()
