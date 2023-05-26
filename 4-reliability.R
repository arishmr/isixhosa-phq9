## 3 - RELIABILITY
## Descriptive stats for each question
## Cronbach's alpha as a measure of internal consistency for the translated version
## Inter-item (matrix) and item-total score correlations, plus averages of both

## Item means
means <- testdata %>% 
  dplyr::select(2:10) %>% 
  summarise_all(mean)

SDs <- testdata %>% 
  dplyr::select(2:10) %>% 
  summarise_all(sd)

means
SDs

## Extract data frames with just item scores and with item scores and total score
cor.total <- testdata[, 2:11]
cor <- testdata[, 2:10]

## Calculate cronbach's alpha
alpha(cor)
cronbach <- alpha(cor)$total$raw_alpha


## Generate correlation matrix for PHQ-9 scores
corrmatrix <- cor(cor.total, method = "spearman")
write.csv(corrmatrix, "Results/PHQ-9 Correlations.csv", row.names = T)

## Produce heatmap of inter-item correlations
cor <- read.csv("Results/PHQ-9 Correlations.csv", row.names = 1)
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
ggsave("Figures/PHQ-9 Correlations.tiff", gplot, scale = 1.5)
rm(melted.cor, cor, gplot)


## Calculate average inter-item and item-total correlations
corrmatrix[corrmatrix==1] <- NA
interitem_range <- range(corrmatrix[1:9,1:9], na.rm = T)
interitem_avg <- colMeans(corrmatrix[1:9,1:9], na.rm = T)
range(interitem_avg)
itemtotal_avg <- mean(corrmatrix[10], na.rm = T)
itemtotal_avg


## Create dataframe and write output CSV

df.means <- data.frame (item = item,
                        ourmeans = as.numeric(means),
                        ourSDs = as.numeric(SDs),
                        inter_item = as.numeric(interitem_avg),
                        item_total = as.numeric(corrmatrix[1:9,10])
                        )
names(df.means)[1] <- 'Item'
names(df.means)[2] <- 'Mean'
names(df.means)[3] <- 'SD'
names(df.means)[4] <- 'Inter-Item Correlations'
names(df.means)[5] <- 'Item-Total Correlations'
df.means
write.csv(df.means, "Results/Means and SDs.csv")

rm(means, SDs)
#rm(interitem_avg, itemtotal_avg)
rm(cor.total, corrmatrix)

