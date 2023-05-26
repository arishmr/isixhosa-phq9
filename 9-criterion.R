#################### AIM
## Is participant: "clinically depressed", "borderline", or "non-depressed"
## Then calculate:  diagnostic sensitivity and specificity,
##                  predictive values (PPV, NPV)
##                  likelihood ratios (PLR, NLR)
##                  Youden's index
##                  ROC curve and area under the curve
## Finally, compare mean scores on each item of the PHQ-9 between participants classified as depressed, borderline, and non-depressed

criterion <- merge(testdata, ysrdata, all.x = T, all.y = F)
criterion <- criterion %>% na.omit() %>%
  dplyr::select(PID, total, WithDep.t, WithDep.class) %>%
  mutate(WithDep.class = as.factor(ifelse(WithDep.class=="None", 0, 1)))
summary(criterion$WithDep.class)

## Get validity stats at all thresholds from ROC

roc_df <- roc(criterion, WithDep.class, total, ret = "all_coords")
roc_df <- roc_df %>%
  dplyr::select(threshold, sensitivity, specificity, ppv, npv, '1-sensitivity', '1-specificity', youden)
roc_df$plr <- (roc_df$sensitivity/roc_df$'1-specificity')
roc_df$nlr <- (roc_df$'1-sensitivity'/roc_df$specificity)

roc_df <- roc_df %>%
  mutate(sensitivity = sensitivity*100,
         specificity = specificity*100) %>%
  rename(
  "Threshold" = threshold,
  "Sensitivity %" = sensitivity,
  "Specificity %" = specificity,
  "PPV" = ppv,
  "NPV" = npv,
  "1-Sensitivity" = '1-sensitivity',
  "1-Specificity" = '1-specificity',
  "Youden's Index" = youden,
  "PLR" = plr,
  "NLR" = nlr) %>%
  relocate(Threshold:NPV,PLR:NLR)

write.csv(roc_df, "Results/ROC Analyses.csv", row.names = F)

## Plot ROC and get AUC, plus power analysis

roc <- roc(criterion, WithDep.class, total, plot = T, print.auc = T, auc.polygon= T)

auc <- auc(roc)
auc.ci <- ci.auc(roc)

plot.roc(roc, print.auc = T, auc.polygon = T)

ggroc(roc) +
  theme_minimal() +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype="dashed") +
  xlab("Specificity") +
  ylab("Sensitivity")
ggsave("Figures/AUC.tiff")


power.roc.test(auc = 0.706, sig.level = 0.05, ncontrols = 38, ncases = 9)

rm(roc, roc_df, criterion)


## Compare mean scores on each item of PHQ-9 between participants classified as healthy, borderline, and clinical
criterion <- merge(testdata, ysrdata, all.x = T, all.y = T) %>% na.omit() %>%
  dplyr::select(PID:total, WithDep.t, WithDep.class)
means <- criterion %>%
  dplyr::select(q1:q9, WithDep.class) %>%
  mutate(WithDep.class = as.factor(ifelse(WithDep.class=="None", "No Depression", "Borderline or Clinical Depression"))) %>%
  group_by(WithDep.class) %>%
  summarise_at(.vars = vars(q1:q9), .funs = mean) %>%
  as.data.frame() %>%
  melt() %>%
  rename("mean" = value)
sds <- criterion %>%
  dplyr::select(q1:q9, WithDep.class) %>%
  mutate(WithDep.class = as.factor(ifelse(WithDep.class=="None", "No Depression", "Borderline or Clinical Depression"))) %>%
  group_by(WithDep.class) %>%
  summarise_at(.vars = vars(q1:q9), .funs = sd) %>%
  as.data.frame() %>%
  melt() %>%
  rename("sd" = value)

df.group.plot <- cbind(means, sd = sds$sd) %>%
  mutate(n = rep(47)) %>%
  arrange(WithDep.class)
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
write.csv(comp.t, "Results/PHQ9 Mean Score by YSR Classification.csv")

gplot <- ggplot(df.group.plot, aes(x = variable, y = mean, fill = WithDep.class)) +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=0.7, position=position_dodge(width=0.8)) +
  scale_fill_grey() +
  scale_color_manual("black") +
  theme_classic() +
  labs(x = "PHQ-9 Items", y = "Mean Score", fill = "YSR Classification") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels=item) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gplot

ggsave("Figures/PHQ9 Mean Scores by YSR Classification.tiff", gplot)

rm(t.testFunc, comp.t, df.group.plot, gplot, means, sds, criterion)

