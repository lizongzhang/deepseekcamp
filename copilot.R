library(tidyverse)

data(mpg)

mpg %>% 
  ggplot(aes(x = displ, y = hwy, 
             color = drv, size = displ)) + #aes() maps the variables to the axes
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Engine Displacement vs. Highway MPG",
    x = "Engine Displacement (L)",
    y = "Highway MPG"
  ) 

#利用mpg, 进行回归分析
# 1. 线性回归
lm_model <- lm(hwy ~ displ + drv, data = mpg)
summary(lm_model)

# 2. 逻辑回归
log_model <- glm(drv ~ displ + hwy, data = mpg, family = binomial)
summary(log_model)

# 3. 多项式回归
poly_model <- lm(hwy ~ poly(displ, 2) + drv, data = mpg)
summary(poly_model)

# 4. 岭回归
library(MASS)
ridge_model <- lm.ridge(hwy ~ displ + drv, data = mpg, lambda = seq(0, 10, by = 0.1))
plot(ridge_model)

# 5. Lasso回归
library(glmnet)
x <- model.matrix(hwy ~ displ + drv, data = mpg)[, -1]
y <- mpg$hwy
lasso_model <- glmnet(x, y, alpha = 1)
