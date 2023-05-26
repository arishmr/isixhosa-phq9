##################### SENSITIVITY ANALYSES:
##################### for only participants with HIV
## Reliability (cronbach's alpha) and inter-item/item-total correlations
## Factor analysis - scree plot and eigenvalues/variances/factor loadings

hivtestdata <- alldata %>%
  filter(alldata$HIV.status == "Participants with HIV")
hivtestdata <- hivtestdata[, c(1,14:23)]

########## RELIABILITY ONLY FOR PARTICIPANTS WITH HIV

## Item means
means <- hivtestdata %>% 
  dplyr::select(2:10) %>% 
  summarise_all(mean)

SDs <- hivtestdata %>% 
  dplyr::select(2:10) %>% 
  summarise_all(sd)

means
SDs

## Extract data frames with just item scores and with item scores and total score
cor.total <- hivtestdata[, 2:11]
cor <- hivtestdata[, 2:10]

## Calculate cronbach's alpha
alpha(cor)
cronbach.hiv <- alpha(cor)$total$raw_alpha


## Generate correlation matrix for PHQ-9 scores
corrmatrix <- cor(cor.total, method = "spearman")
write.csv(corrmatrix, "Results/Sensitivity - PHQ-9 Correlations HIV Only.csv", row.names = T)

## Produce heatmap of inter-item correlations
cor <- read.csv("Results/Sensitivity - PHQ-9 Correlations HIV Only.csv", row.names = 1)
cor <- as.matrix(cor)
cor[upper.tri(cor)] <- NA
melted.cor <- melt(cor)
range(na.omit(melted.cor$value))

gplot <- ggplot(data = melted.cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "gray") +
  geom_text(aes(label=round(value, digits=2)), size = 3, alpha = 0.5, na.rm = T) +
  scale_fill_gradient2(low = "brown2", high = "blue2", mid = "cornsilk", na.value = "transparent",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  labs(x = "", y = "") +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

gplot
ggsave("Figures/Sensitivity - PHQ-9 Correlations HIV Only.tiff", gplot, scale = 1.5)
rm(melted.cor, cor, gplot)


## Calculate average inter-item and item-total correlations
corrmatrix[corrmatrix==1] <- NA
range(corrmatrix, na.rm = T)
interitem_avg.hiv <- colMeans(corrmatrix[1:9,1:9], na.rm = T)
range(interitem_avg.hiv)
itemtotal_avg.hiv <- mean(corrmatrix[10], na.rm = T)
itemtotal_avg.hiv


## Create dataframe and write output CSV

df.means <- data.frame (item = item,
                        ourmeans = as.numeric(means),
                        ourSDs = as.numeric(SDs),
                        inter_item = as.numeric(interitem_avg.hiv),
                        item_total = as.numeric(corrmatrix[1:9,10])
)
names(df.means)[1] <- 'Item'
names(df.means)[2] <- 'Mean'
names(df.means)[3] <- 'SD'
names(df.means)[4] <- 'Inter-Item Correlations'
names(df.means)[5] <- 'Item-Total Correlations'
df.means
write.csv(df.means, "Results/Sensitivity - Means and SDs HIV Only.csv")

rm(means, SDs)
#rm(interitem_avg.hiv, itemtotal_avg.hiv)
rm(cor.total, corrmatrix)



########## FACTOR STRUCTURE ONLY FOR PARTICIPANTS WITH HIV


## Create dataframe
pcadata <- hivtestdata[, 2:10]

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
ggsave("Figures/Sensitivity - Scree Plot HIV Only.tiff", plot = scree.plot, width = 8, height = 4)


## Export results of PCA to CSV

df.pca <- data.frame (eigenvalues,
                      var_exp,
                      loadings)
names(df.pca)[1] <- "Eigenvalues"
names(df.pca)[2] <- "Variance Explained"
names(df.pca)[3] <- "Factor Loadings"

write.csv(df.pca, "Results/Sensitivity - Principal Component Analysis HIV Only.csv")

rm(pcadata, df.pca, df.means, pca, scree.plot, eigenvalues, loadings, var_exp)

rm(hivtestdata)
