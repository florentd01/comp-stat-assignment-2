gray_codes <- function(n) {
  total <- choose(2*n - 1, n - 1)
  gray_codes <- matrix(0L, nrow = total, ncol = n)
  r <- integer(n)
  r[1] <- n
  t <- n
  h <- 0
  gray_codes[1, ] <- r
  i <- 2
  while (r[n] != n) {
    if (t != 1) {
      h <- 0
    }
    h <- h + 1
    t <- r[h]
    r[h] <- 0
    r[1] <- t - 1
    r[h+1] <- r[h+1] + 1
    gray_codes[i, ] <- r
    i <- i + 1
  }
  return(gray_codes)
}
