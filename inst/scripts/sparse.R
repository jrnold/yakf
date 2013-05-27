# slam

library(slam)

x <- array(c(1, 0, 0, 2, 0, 0, 1, 3), dim = c(2, 2, 2))
s <- as.simple_sparse_array(x)

## Extract part
s[ , , 1]

## set part
s[ , , 1] <- array(c(1, 0, 0, 1), c(2, 2))

setOldClass("simple_sparse_array")

setAs("simple_triplet_matrix", "CsparseMatrix",
      function(from, to) {
        sparseMatrix(i = as.integer(from$i),
                     j = as.integer(from$j),
                     x = from$v,
                     dims = c(from$nrow, from$ncol))
      })

simple_sparse_array_to_mat <- function(from, to) {
  as(as.simple_triplet_matrix(from), to)  
}


setAs("simple_sparse_array", "CsparseMatrix",
      simple_sparse_array_to_mat)

setAs("simple_triplet_matrix", "dgTMatrix",
      function(from, to) {
        new(to,
            i = as.integer(from$i) - 1L,
            j = as.integer(from$j) - 1L,
            x = as.numeric(from$v),
            Dim = c(from$nrow, from$ncol))
      })

setAs("simple_sparse_array", "dgTMatrix",
      simple_sparse_array_to_mat)
      
s1 <- s[ , , 1]
dim(s1) <- dim(s1)[1:2]

i <- s1$i[1, ]
j <- s1$i[2, ]

as(s[ , , 1], "CsparseMatrix")
as(s[ , , 1], "dgTMatrix")

# SparseM and Matrix do not have array formats


