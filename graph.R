library(tidyverse)
library(readxl)

stroke <- read_excel("stroke.xlsx")

glimpse(stroke)

#将数值型的sex, status, stroke_type转换为factor
stroke <- stroke %>% 
  mutate(sex = factor(sex, 
                      levels = c(1,2),
                      labels = c("male", "female")),
         status = factor(status, 
                         levels = c(1,2),
                         labels = c("alive", "dead")),
         stroke_type = factor(stroke_type, 
                              levels = c(0,1),
                              labels = c("Ischaemic",
                                         "Haemorrhagic")))

glimpse(stroke)

library(gtsummary)

stroke %>% 
  tbl_summary()

stroke %>% 
  tbl_summary(by = status) 


model1 <- stroke %>% 
  glm(status ~ gcs + stroke_type + sex + dm + sbp + age, 
      data = ., 
      family = binomial(link = 'logit'))


# 报告bi
model1 %>% 
  tbl_regression(intercept = TRUE,
                 estimate_fun = function(x) style_number(x, digits = 3),
                 pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
  as_gt()

library(ggstatsplot)

stroke %>% 
  gghistostats(age,
              test.value = 58)


stroke %>% 
  ggbetweenstats(stroke_type,
                 age)
    
               age, 
               test.value = 58,
  )

    