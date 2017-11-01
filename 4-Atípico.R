################ Multivariada #################


library(tidyverse)   
library(MASS)      
library(caret) 

multivariate_normal <- function(base_datos) {
  
  x <- base_datos %>% 
    mutate_all(as.numeric) %>% 
    dplyr::select(-c(index)) %>% 
    prcomp(scale. = TRUE)
  
  x <- x$x %>% tibble::as_tibble() %>% dplyr::select(c(PC1, PC2))
  
  preObj <- preProcess(x, method = "center")
  X2 <- predict(preObj,x)
  
  X2= as.matrix(X2)
  sigma2=diag(var(X2))
  
  sigma2=diag(sigma2)
  
  A=(2*pi)^(-ncol(X2)/2)*det(sigma2)^(-0.5)
  
  B = exp(-0.5 *rowSums((X2%*%ginv(sigma2))*X2))
  p=as.vector(A*B)
  
  tib <- tibble::tibble(probabilities = p, index = base_datos$index, valor = base_datos$valor)
  tib2 <- cbind(x, tib)
}


