##################### SENSITIVITY ANALYSES:
##################### convergent validity
## Control Spearman's correlation coefficient for time difference between PHQ-9 and YSR

convergent <- merge(testdata, ysrdata, all.x = T, all.y = T)
convergent <- convergent %>% dplyr::select(PID, total, AnxDep.t, AnxDep.class, WithDep.t, WithDep.class, AffDis.t, AffDis.class) %>% 
  mutate(AnxDep.class = as.factor(AnxDep.class),
         WithDep.class = as.factor(WithDep.class),
         AffDis.class = as.factor(AffDis.class)) %>%
  mutate(diffdate = abs(as.numeric(difftime(alldata$YSR.date, alldata$PHQ9.date, units = "days")))) %>%na.omit()

summary(convergent$AnxDep.class)
summary(convergent$WithDep.class)
summary(convergent$AffDis.class)

plot(convergent$total, convergent$AnxDep.t)
pcor.test(convergent$total, convergent$AnxDep.t, convergent$diffdate, method = "spearman")

plot(convergent$total, convergent$WithDep.t)
pcor.test(convergent$total, convergent$WithDep.t, convergent$diffdate, method = "spearman")

plot(convergent$total, convergent$AffDis.t)
pcor.test(convergent$total, convergent$AffDis.t, convergent$diffdate, method = "spearman")

rm(convergent)



##################### criterion validity
## AUC and ROC for only subset of participants with less than 100 days between PHQ-9 and YSR

criterion <- alldata %>%
  mutate(diffdate = abs(as.numeric(difftime(alldata$YSR.date, alldata$PHQ9.date, units = "days")))) %>%
  filter(diffdate < 100) %>%
  mutate(WithDep.class = as.factor(ifelse(WithDep.class=="None", 0, 1))) %>%
  dplyr::select(PID, total, WithDep.t, WithDep.class, diffdate)

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

write.csv(roc_df, "Results/Sensitivity - ROC Analyses 100 Days.csv", row.names = F)

## Plot ROC and get AUC, plus power analysis

roc <- roc(criterion, WithDep.class, total, plot = T, print.auc = T, auc.polygon= T)

auc.100 <- auc(roc)
auc.100.ci <- ci.auc(roc)

plot.roc(roc, print.auc = T, auc.polygon = T)

ggroc(roc) +
  theme_minimal() +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype="dashed") +
  xlab("Specificity") +
  ylab("Sensitivity")
ggsave("Figures/Sensitivity - AUC 100 Days.tiff")


power.roc.test(auc = 0.794, sig.level = 0.05, ncontrols = 19, ncases = 6)

rm(roc, roc_df, criterion)






