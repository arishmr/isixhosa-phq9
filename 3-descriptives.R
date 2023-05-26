## Summarise key demographic info

## Extract demographic data
colnames(alldata)
demodata <- data.frame(alldata[c(1,3:9,11,13,24:26,39)])
colnames(demodata)

gender <- table(demodata$gender)
sexuality <- table(demodata$sexual.orient)
chisq.test(gender)
chisq.test(sexuality)
rm(gender, sexuality)

median(alldata$total)
quantile(alldata$total, 0.25)
quantile(alldata$total, 0.75)
range(alldata$total)

tab1 <- tbl_summary(demodata,
            by = HIV.status,
            type = list(age ~ "continuous", viral.load.copies ~ "continuous", CD4.abs ~ "continuous", regimen ~ "categorical"),
            statistic = list(
              all_continuous() ~ "{median} ({p25}, {p75})",
              all_categorical() ~ "{n} ({p}%)"
            ),
            digits = all_continuous() ~ 1,
            label = list(
              grade ~ "Grade in School",
              age ~ "Age (years)",
              gender ~ "Gender",
              ethnicity ~ "Ethnicity",
              sexual.orient ~ "Sexual orientation",
              alcohol ~ "Alcohol use",
              smoking ~ "Cigarette smoking",
              recdrugs ~ "Recreational drug use",
              viral.load.copies ~ "Viral Load (copies/\u03BCL)",
              CD4.abs ~ "Absolute CD4 Count",
              regimen ~ "Antiretroviral Regimen"
            ),
            missing_text = "(Missing)"
            ) %>%
  add_p() %>%
  #add_n() %>%
  add_overall() %>%
  add_stat_label()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - Demographics by HIV Status.docx")
  
rm(tab1, tab1.print)

## Differences in demographic characteristics by depression status

tab1 <- tbl_summary(demodata,
                    by = PHQ9.class,
                    type = list(age ~ "continuous", viral.load.copies ~ "continuous", CD4.abs ~ "continuous", regimen ~ "categorical"),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    label = list(
                      grade ~ "Grade in School",
                      age ~ "Age (years)",
                      gender ~ "Gender",
                      ethnicity ~ "Ethnicity",
                      sexual.orient ~ "Sexual orientation",
                      alcohol ~ "Alcohol use",
                      smoking ~ "Cigarette smoking",
                      recdrugs ~ "Recreational drug use",
                      viral.load.copies ~ "Viral Load (copies/\u03BCL)",
                      CD4.abs ~ "Absolute CD4 Count",
                      regimen ~ "Antiretroviral Regimen"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p() %>%
  #add_n() %>%
  add_overall() %>%
  add_stat_label()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - Demographics by Depression Status.docx")

rm(tab1, tab1.print)


#rm(demodata)
