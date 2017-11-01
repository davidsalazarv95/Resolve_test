################ Regresión ################ 

library(tidyverse)
library(broom)
library(forcats)
library(randomForest)
library(caret)
library(ggalt)
library(hrbrthemes)

standardize <- function(variable) {
  stan <- (variable - mean(variable))/sd(variable)
}



regresion <- function(data) {
  data <- data %>% 
    select(-c(index, valor, rio)) %>% 
    mutate_all(standardize) %>% 
    cbind(base_datos$valor, base_datos$rio) %>% 
    rename(valor = `base_datos$valor`, rio = `base_datos$rio`) %>% 
    as_tibble()
  reg <- lm(valor ~., data = data)
  coef <- tidy(reg, conf.int = TRUE)
  coef %>% 
    filter(term != "(Intercept)") %>% 
    ggplot(aes(x = fct_reorder(factor(term), estimate), y = estimate)) +
    geom_point() +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
    theme_ipsum_rc() +
    coord_flip() +
    labs(y = "Coeficiente Estimado", 
         subtitle = "Barras cubren intervalo de confianza", 
         x = "Variable", 
         title = "Coeficientes de regresion vs. Valor")
}

################ Random Forest ################ 

random_forest_importance <- function(data) {
  set.seed(22)
  data <- data %>% 
    select(-c(index, valor, rio)) %>% 
    mutate_all(standardize) %>% 
    cbind(base_datos$valor, base_datos$rio) %>% 
    rename(valor = `base_datos$valor`, rio = `base_datos$rio`) %>% 
    as_tibble()
  fit <- randomForest(valor ~., data = data, importance = TRUE)
  imp <- varImp(fit)
  as_tibble(imp) %>% rownames_to_column(var = "Variable") %>% 
    mutate(Overall = Overall/100) %>% 
    ggplot(aes(y = fct_reorder(factor(Variable), Overall), x = Overall)) +
      geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE) +
      theme_ipsum_rc() +
      scale_x_percent(limits = c(0, 0.45)) +
      labs(y = "Variable",
           title = "Importancia predictiva de las variables",
           x = "")
    
}