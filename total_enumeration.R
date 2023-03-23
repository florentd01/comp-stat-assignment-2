library("ggplot2")
library("tibble")
library("bootstrap")
library("magrittr")
data(law)
law %<>% add_column(observation = 1:nrow(law), .before = 1)
ggplot(law, aes(x = LSAT, y = GPA)) +
  geom_text(aes(label = observation),
            hjust = 0, vjust = 0)

n_obs <- length(law$observation)
enum <- 1:n_obs

which_bin <- function(x) {
  return(floor(x*100)+101)
}
corr_bin_count <- vector("double", 200)

for (a in enum) {
  for (b in enum) {
    for (c in enum) {
      for (d in enum) {
        for (e in enum) {
          for (f in enum) {
            for (g in enum) {
              for (h in enum) {
                for (i in enum) {
                  for (j in enum) {
                    for (k in enum) {
                      for (l in enum) {
                        for (m in enum) {
                          for (n in enum) {
                            for (o in enum) {
                              indices <- c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
                              correl <- cor(law$LSAT[indices], law$GPA[indices])
                              index <- which_bin(correl)
                              corr_bin_count[index] = corr_bin_count[index] + 1
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

load('myEnvironment.RData')


