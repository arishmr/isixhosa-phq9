## Create dataframe
pcadata <- testdata[, 2:10]

## Determine eigenvalues and scree plot
pca <- principal(pcadata, nfactors = 1, rotate = "varimax")
eigenvalues <- pca$values
loadings <- as.numeric(pca$loadings)
var_exp = eigenvalues / sum(eigenvalues)

scree.plot <- qplot(c(1:9), var_exp) +
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ylim(0, 1) +
  theme_classic()
scree.plot
ggsave("Figures/Scree Plot.tiff", plot = scree.plot, width = 8, height = 4)


## Export results of PCA to CSV

df.pca <- data.frame (eigenvalues,
                      var_exp,
                      loadings)
names(df.pca)[1] <- "Eigenvalues"
names(df.pca)[2] <- "Variance Explained"
names(df.pca)[3] <- "Factor Loadings"

write.csv(df.pca, "Results/Principal Component Analysis.csv")


## Comparisons of eigenvalues and factor loadings with Monahan et al (2008)
eigen.monah <- c(3.3, 1.0, 0.9, 0.8, 0.7, 0.7, 0.6, 0.5, 0.5)
var_exp.monah <- eigen.monah / sum(eigen.monah)
var_exp.comp <- data.frame(datasource = rep(c("isiXhosa, South Africa", "English, Kenya"), each = 9),
                            item = rep(c(1:9)),
                            var = c(var_exp, var_exp.monah))
var_exp.comp

scree.plot <- ggplot(var_exp.comp, aes(x = item, y = var, group = fct_rev(datasource))) +
  geom_line(mapping = aes(linetype = fct_rev(datasource))) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  labs(x ="Principal Component",
       y = "Variance Explained",
       linetype = "") +
  ylim(0, 1) +
  theme_classic() +
  theme(legend.position="top")
scree.plot
ggsave("Figures/Scree Plot Comparisons.tiff", plot = scree.plot, width = 8, height = 4)


loadings.cumbe <- c(0.722, 0.717, 0.582, 0.701, 0.564, 0.747, 0.728, 0.753, 0.737)
loadings.monah <- c(0.65, 0.63, 0.58, 0.66, 0.57, 0.61, 0.64, 0.59, 0.52)
factors = c(loadings, loadings.cumbe, loadings.monah)

factor.congruence(loadings, loadings.cumbe, digits=2)
factor.congruence(loadings, loadings.monah, digits=2)


df.group.plot <- data.frame(datasource = rep(c("isiXhosa, South Africa", "English, Kenya", "Portuguese, Mozambique"), each = 9),
                            item = item,
                            factors)
df.group.plot

group.plot <- ggplot(df.group.plot, aes(x = item, y = factors, fill = factor(datasource, level = c("isiXhosa, South Africa", "English, Kenya", "Portuguese, Mozambique")))) +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  ylim(0, 1.0) +
  scale_fill_grey() +
  scale_color_manual("black") +
  theme_classic() +
  labs(x = "PHQ-9 Items", y = "Factor Loading", fill = "") +
  scale_x_discrete(limits = item) +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
group.plot
ggsave("Figures/Factor Loading Comparisons.tiff", plot = group.plot, width = 10, height = 5)


rm(df.pca, pca, pcadata, scree.plot, eigenvalues, eigen.monah, loadings, var_exp, var_exp.comp, var_exp.monah, factors, group.plot, df.group.plot, loadings.cumbe, loadings.monah)

