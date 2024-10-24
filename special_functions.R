## Esse arquivo algumas funções com o propósito especial
## sobre amostragem que usamos na disciplina de Métodos Quantitativos.


## Esta função desenha uma amostra de linhas de tamanho n de um dataframe,
## com studentID como a semente.
minha.amostra <- function(matricula, n, dados) {
  suppressWarnings(RNGversion("3.2.1"))
  set.seed(matricula)
  valores_amostrados <- sample(size = n, c(1:nrow(dados)))
  minha_amostra <- dados[dados$ID %in% valores_amostrados,]
  return(minha_amostra)
}


## Esta função produz uma série de amostras com ICs de 95% para a média populacional, para
## uma variável de sua escolha.
dist.amostral.ic <- function(matricula, n.indiv, dados, n.amostras, variavel, nivel.conf) {
  crit.t = qt(p = nivel.conf + ((1-nivel.conf)/2), df = n.indiv - 1)
  medias_amostrais <- 0
  limites_inferiores <- 0
  limites_superiores <- 0
  for (n in 1:n.amostras) {
    amostra.num <- (matricula + (n-1))
    minha_amostra <- suppressWarnings(fazer.minha.amostra(matricula = amostra.num, n = n.indiv, dados))
    media_amostral <- mean(minha_amostra[,variavel])
    erro_padrao_amostral <- sd(minha_amostra[,variavel])/sqrt(n.indiv)
    limite_inferior <- media_amostral - crit.t*erro_padrao_amostral
    limite_superior <- media_amostral + crit.t*erro_padrao_amostral
    medias_amostrais[n] <- media_amostral
    limites_inferiores[n] <- limite_inferior
    limites_superiores[n] <- limite_superior
  }
  combinado <- data.frame(medias_amostrais, limites_inferiores, limites_superiores)
  colnames(combinado) = c("media.amostral", "IC.inferior", "IC.superior")
  return(combinado)
}

## Originals -------------------------------------------------------------------

# ## This one draws a sample of rows of size n from a dataframe,
# ## with studentID as the seed.
# make.my.sample <- function(studentID, n, data) {
#   suppressWarnings(RNGversion("3.2.1"))
#   set.seed(studentID)
#   sample_values <- sample(size = n, c(1:nrow(data)))
#   my_sample <- data[data$ID %in% sample_values,]
#   return(my_sample)
# }
# 
# ## This one produces a bunch of samples with 95% CIs for the population mean, for
# ## a variable of your choice.
# make.sampling.dist.ci <- function(studentID, n.indiv, data, n.samples, variable, conf.level) {
#   crit.t = qt(p = conf.level + ((1-conf.level)/2), df = n.indiv - 1)
#   sample.means <- 0
#   lower.bounds <- 0
#   upper.bounds <- 0
#   for (n in 1:n.samples) {
#     sample.num <- (studentID + (n-1))
#     my_sample <- suppressWarnings(make.my.sample(studentID = sample.num, n = n.indiv, data))
#     sample.mean <- mean(my_sample[,variable])
#     sample.se <- sd(my_sample[,variable])/sqrt(n.indiv)
#     lower.bound <- sample.mean - crit.t*sample.se
#     upper.bound <- sample.mean + crit.t*sample.se
#     sample.means[n] <- sample.mean
#     lower.bounds[n] <- lower.bound
#     upper.bounds[n] <- upper.bound
#   }
#   combined <- data.frame(sample.means, lower.bounds, upper.bounds)
#   colnames(combined) = c("sample.mean", "CI.lower", "CI.upper")
#   return(combined)
# }

