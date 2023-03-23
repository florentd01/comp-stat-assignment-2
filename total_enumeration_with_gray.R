
data(law)
n <- 15

mat <- gray_codes(n)
rows <- dim(mat)[1]

indices <- matrix(NA, nrow = rows, ncol = n)

for (i in 1:rows) {

  vec <- c()
  for (j in 1:n) {
    if (mat[i,j] != 0) {
      for (count in 1:mat[i,j]) {
        vec <- append(vec, j)
      }
    }
  }
  indices[i, 1:n] <- vec
}

func <- function(x, law){cor(law$LSAT[x], law$GPA[x], method = "pearson")}
correlations <- rep(0, rows)
for (i in 1:rows) {
  correlations[i] <- func(indices[i,1:n], law)
}
