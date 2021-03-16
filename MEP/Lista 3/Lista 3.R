
# Letra C - Ex 10

# Valores dados

zts <- c(0.66,0.57,0.66,-1.47,-1.38,-1.9,-0.7)

# Modelo Arima

fit <- arima0(x = zts, order = c(1,1,0))

fit

# Previsão

pred <- predict(fit,n.ahead = 4)
pred

# Intervalo de Confiança da Previsão

ic_y8 <- c(pred$pred[1] - qnorm(0.975)*pred$se[1],pred$pred[1] + qnorm(0.975)*pred$se[1])
ic_y9 <- c(pred$pred[2] - qnorm(0.975)*pred$se[2],pred$pred[2] + qnorm(0.975)*pred$se[2])

ic_y8
ic_y9

# Exercício 14

# SARIMA

# Abrindo base

base <- read.csv("C:/Users/Paulo/Downloads/series.csv")

questao14 <- function(base){

# Calculando para os primeiros dados

fit <- arima0(x = base[1:988], order = c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))

# Valor dos coeficientes

coeficientes <- fit$coef

# variância dos coeficientes

variancia_coeficientes <- fit$var.coef

#Padronizando

z <- c()

z[1] <- fit$coef[[1]]/sqrt(fit$var.coef[1,1])
z[2] <- fit$coef[[2]]/sqrt(fit$var.coef[2,2])


variaveis_padronizadas <- z

# Calculando o P-valor

pvalor <- 2*(1-pnorm(abs(z)))

# Normalidade dos resíduos

residuos <- residuals(fit)

teste_normalidade <- shapiro.test(residuals(fit))

# Independência para valores diferentes de lag

box_lag6 <- Box.test(residuals(fit), lag = 6, fitdf = 2)
box_lag12 <- Box.test(residuals(fit), lag = 12, fitdf = 2)
box_lag24 <- Box.test(residuals(fit), lag = 24, fitdf = 2)
box_lag36 <- Box.test(residuals(fit), lag = 36, fitdf = 2)

# Valores preditos

preditos <- predict(fit,n.ahead = 12)

# comparação com os valores reais

resultado <- list("Coeficientes" = coeficientes,
                  "Variância Coeficientes" = variancia_coeficientes, "Resíduos" = residuos,
                  "Variaveis Padronizadas" = variaveis_padronizadas,"P-valor Variáveis Padronizadas" = pvalor,
                  "Teste de Normaliddade" = teste_normalidade, "Box Teste Lag 6" = box_lag6,
                  "Box Teste Lag 12" = box_lag12,"Box Teste Lag 24" = box_lag24,"Box Teste Lag 36" = box_lag36,
                  "Previsão" = preditos)

}

resposta <- questao14(base$V1)

