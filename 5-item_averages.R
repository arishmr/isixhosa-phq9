## Goal: produce a clustered bar graph comparing item means and SDs in our sample and a comparable sample


## Create data frame with comparison of means and SDs
means <- c(as.numeric(unlist(df.means[2])),
           #c(0.67, 0.61, 0.92, 0.88, 0.67, 0.30, 0.48, 0.30, 0.09)    # Lotrakul et al
           #c(1.22, 0.93, 1.02, 1.18, 0.90, 0.68, 0.85, 0.60, 0.46)     # Makhubela et al
           c(0.621, 0.539, 0.895, 1.129, 0.687, 0.548, 0.496, 0.270, 0.104))      # Bianchi et al
SDs <- c(as.numeric(unlist(df.means[3])),
         #c(0.66, 0.69, 0.87, 0.83, 0.78, 0.58, 0.68, 0.55, 0.34)                 # Lotrakul et al
         #c(1.043, 0.949, 1.081, 1.004, 1.041, 0.969, 0.996, 0.897, 0.861)         # Makhubela et al
         c(0.831, 0.784, 1.030, 0.991, 0.966, 0.853, 0.802, 0.627, 0.401))        # Bianchi et al
means
SDs

df.group.plot <- data.frame(datasource = rep(c("isiXhosa, South Africa", "Pooled, N = 58,472"), each = 9),
                            item = c(df.means[1], row.names = NULL),
                            mean = means,
                            sd = SDs,
                            n = rep(c(30, 58272), each = 9)
                            )
df.group.plot


t.testFunc <- function(i){
  test <- tsum.test(mean.x = df.group.plot[i,3],
                    mean.y = df.group.plot[i+9,3],
                    s.x = df.group.plot[i,4],
                    s.y = df.group.plot[i+9,4],
                    n.x = df.group.plot[i,5],
                    n.y = df.group.plot[i+9,5],
                    alternative = "two.sided")
  df.t.test <- data.frame(item = item, t = as.numeric(test$statistic),
                          df = as.numeric(test$parameters), p = test$p.value,
                          p.fdr = p.adjust(as.numeric(test$p.value), method = "fdr"))
  df.t.test
}

comp.t <- t.testFunc(1:9)
write.csv(comp.t, "Results/Mean Score Comparisons.csv")



group.plot <- ggplot(df.group.plot, aes(x = Item, y = mean, fill = datasource)) +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=0.7, position=position_dodge(width=0.8)) +
  #scale_y_continuous(breaks = seq(0, 2, by = 0.10)) +
  scale_fill_grey() +
  scale_color_manual("black") +
  theme_classic() +
  labs(x = "PHQ-9 Items", y = "Mean Score", fill = "") +
  scale_x_discrete(limits = item) +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
group.plot

ggsave("Figures/Mean Score Comparisons.tiff", plot = group.plot, width = 10, height = 5)

rm(df.group.plot, group.plot, means, SDs, comp.t, t.testFunc, df.means)
