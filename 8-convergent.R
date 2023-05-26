#################### AIM
## Calculate correlation coefficients for the total PHQ-9 score and T score for Anxious/Depressed, Withdrawn/Depressed components of the YSR for participants

convergent <- merge(testdata, ysrdata, all.x = T, all.y = T)
convergent <- convergent %>% dplyr::select(PID, total, AnxDep.t, AnxDep.class, WithDep.t, WithDep.class, AffDis.t, AffDis.class) %>% na.omit() %>%
  mutate(AnxDep.class = as.factor(AnxDep.class),
         WithDep.class = as.factor(WithDep.class),
         AffDis.class = as.factor(AffDis.class))

summary(convergent$AnxDep.class)
summary(convergent$WithDep.class)
summary(convergent$AffDis.class)

plot(convergent$total, convergent$AnxDep.t)
cor.test(convergent$total, convergent$AnxDep.t, method = "spearman")

plot(convergent$total, convergent$WithDep.t)
cor.test(convergent$total, convergent$WithDep.t, method = "spearman")

plot(convergent$total, convergent$AffDis.t)
cor.test(convergent$total, convergent$AffDis.t, method = "spearman")

rm(convergent)
