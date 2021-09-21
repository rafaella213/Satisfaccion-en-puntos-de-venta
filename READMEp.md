# Satisfaccion-en-puntos-de-venta
La base contiene información de 70 puntos de venta que se utilizan para determinar el impacto de las características de los puntos de venta en su probabilidad de ser clasificados de acuerdo a los niveles de satisfacción posibles

base2$satisf <- factor(base2$satisf, ordered=TRUE)

# estimar un modelo logit ordenado y guardar los resultados en "ml"

mlog <- polr(satisf ~ ofertas + precios + variedad , data = base2, Hess=TRUE, method ="logistic")
summary(mlog)

# para estimar un probit escribo -method ="probit", y guardar los resultados en "mp"

mprob <- polr(satisf ~ ofertas + precios + variedad , data = base2, Hess=TRUE, method ="probit")
summary(mprob)

##guardo las tablas con las salidas
(ctablemlog <- coef(summary(mlog)))
(ctablempro <- coef(summary(mprob)))

## calculo y guardo los valores p
plog <- pnorm(abs(ctablemlog[, "t value"]), lower.tail = FALSE) * 2
pprob <- pnorm(abs(ctablempro[, "t value"]), lower.tail = FALSE) * 2


## incluyo los valores p a la tabla anterior
(ctablemlog <- cbind(ctablemlog, "p value" = plog))

(ctablemprob <- cbind(ctablempro, "p value" = pprob))

#Obtenemos los intervalos de confianza de acuerdo a dos métodos alternativos

(cilog <- confint(mlog)) # basados en las características de la FV
confint.default(mlog) # asumiendo la normalidad de los errores

(ciprob <- confint(mprob)) 
confint.default(mprob)

#Recordar que la interpretación de los coeficientes de los modelos logit y probit no es directa. 
#Se pueden transformar los coeficientes del logit en odds ratios o razones de probabilidades.

#odds ratios
exp(coef(mlog))

#OR e intervalos de confianza
exp(cbind(OR = coef(mlog), cilog))


#Predigo la probabilidad de cada alternativa de acuerdo al modelo logit o probit

probsmlog<- cbind(base2, predict(mlog, base2, type = "probs"))

probsmpro<- cbind(base2, predict(mprob, base2, type = "probs"))

##

options(scipen=999)


newdatlog <- cbind(base2, predict(mlog, base2, type = "probs"))

newdatpro <- cbind(base2, predict(mprob, base2, type = "probs"))
