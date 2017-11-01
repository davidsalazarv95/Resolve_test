################ Univariado Valor ################ 

library(tidyverse)
library(corrplot)
library(hrbrthemes)
library(corrr)
library(viridis)
library(gridExtra)
library(grid)

histograma_valor <- function(datos, binwidth = 50) {
  
  medidas_centrales <- datos %>% select(valor) %>% summarise(mediana = median(valor), 
                                                             media = mean(valor))
  
  datos %>% 
    ggplot(aes(x = valor)) +
    geom_histogram(binwidth = binwidth, fill = "blue4", alpha = 0.5, color = "black") +
    theme_ipsum_rc() +
    scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = "$")) +
    geom_vline(xintercept = medidas_centrales[['mediana']], linetype = 4, show.legend = TRUE) +
    labs(y = "Conteo",
         title = "Histograma de valor de inmueble",
         subtitle = paste0("Cada barra representa intervalos de ", binwidth ,". Mediana como línea: ", "$",round(medidas_centrales[['mediana']], 1)))
  
}

################ Location ################ 

ubicación <- function(datos) {
  
  datos %>% 
    ggplot(aes(x = coordx, y = coordy, color = valor, size = valor)) +
      geom_point() +
      scale_color_viridis() +
      theme_ipsum_rc()
      
}






################ Variación conjunta ################ 

matriz_correlaciones <- function(datos) {
  datos <- datos %>% select(-index) %>% mutate(rio = as.numeric(rio))
  corrplot(cor(datos), order = "hclust",  tl.col = "black")
}

big_three <- function(datos, num = 3) {

  datos <- datos %>% select(-index) %>% mutate(rio = as.numeric(rio))
  correlate(datos) %>% stretch() %>% filter(x == "valor") %>% arrange(desc(abs(r))) %>% head(num) %>% 
    select(y, r) %>% rename(Variable = y, Correlación = r)
  
}


scatterplots <- function(datos) {
  
  
  g1 <- datos %>% 
    ggplot(aes(x = pobreza, y = valor)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_ipsum_rc() +
    labs(title = "Valor vs. Pobreza")
  
  g2 <- datos %>% 
    ggplot(aes(x = cuartos, y = valor)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_ipsum_rc() +
    labs(title = "Valor vs. # de Cuartos")
  
   g3 <- datos %>% 
    ggplot(aes(x = tasaeducativa, y = valor)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    theme_ipsum_rc() +
    labs(title = "Valor vs. # profesores por alumno")
  
  grid.arrange(g1, g2, g3, ncol = 2, widths = c(1.3, 1.5))
  
}

    