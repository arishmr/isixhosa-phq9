## Create dataframe
pcadata <- testdata[, 2:10]


# Create function to obtain factor loadings
get.loadings <- function(data, i){pca <- principal(data[i,], nfactors = 1, rotate = "varimax")
                            loadings <- pca$loadings
                            c(loadings[1:9])}
get.loadings(pcadata)


# Run bootstrapping
set.seed(1)
boot.out <- boot(pcadata, get.loadings, R = 1000)
#head(boot.out$t)              # bootstrap realisations of t i.e. factor loadings
#boot.out$t0                   # original values of t i.e. factor loadings
boot.out

# Calculate CIs of type bca and obtain vector with CIs
getCI <- function(x,w) {
  b1 <- boot.ci(x, index = w, type = 'bca')
  b1$bca[,4:5]
  }
# Calculate CIs for all 9 items
b2 <- t(sapply(1:9, getCI, x = boot.out))
bootstrap <- data.frame(CI.min = b2[,1],
                        CI.max = b2[,2])
bootstrap
rm(b2, boot.out, get.loadings, getCI)


#######################################################
## Run only until here to see bootstrap outputs and CIs

loadings.cumbe <- c(0.722, 0.717, 0.582, 0.701, 0.564, 0.747, 0.728, 0.753, 0.737)
loadings.monah <- c(0.65, 0.63, 0.58, 0.66, 0.57, 0.61, 0.64, 0.59, 0.52)

bootstrap.comparisons <- data.frame(CI.min = bootstrap$CI.min,
                        CI.max = bootstrap$CI.max,
                        loadings.monah,
                        loadings.cumbe)
in.ci.m <- bootstrap.comparisons$loadings.monah >= bootstrap.comparisons$CI.min & loadings.monah <= bootstrap.comparisons$CI.max
in.ci.c <- bootstrap.comparisons$loadings.cumbe >= bootstrap.comparisons$CI.min & loadings.cumbe <= bootstrap.comparisons$CI.max
bootstrap.comparisons <- cbind(bootstrap.comparisons, in.ci.m, in.ci.c)
names(bootstrap.comparisons)[c(5,6)] <- "in.ci"
bootstrap.comparisons <- bootstrap.comparisons[c(1,2,3,5,4,6)]

write.csv(bootstrap.comparisons, "Results/Bootstrap.csv")

rm(pcadata, loadings.cumbe, loadings.monah, bootstrap, bootstrap.comparisons, in.ci.m, in.ci.c)
