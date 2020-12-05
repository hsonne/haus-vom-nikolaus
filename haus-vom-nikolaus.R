#     (3)
#     / \
#    /   \
#   /     \
# (2)-----(4)
#  | \   / |
#  |  \ /  |
#  |   x   |
#  |  / \  |
#  | /   \ |
# (1)-----(5)

edges <- c("12", "14", "15", "23", "24", "25", "34", "45")

go <- function(path, seen = logical(length(edges))) {  
  
  if (length(path) == 9L) {
    cat(paste(path, collapse = "-"), "\n")
    return()
  }
  
  node <- path[length(path)]
  
  for (edge in grep(node, edges[! seen], value = TRUE)) {
    go(c(path, gsub(node, "", edge)), `[<-`(seen, edges == edge, TRUE))
  }
}

result <- capture.output(for (i in 1:5) go(i))
writeLines(sprintf("%02d: %s", seq_along(result), result))
