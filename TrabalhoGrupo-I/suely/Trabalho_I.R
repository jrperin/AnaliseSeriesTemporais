# Trabalho I

trab<-retornos
attach(trab)
head(trab)

###############################################################

# Questao 1b: Histograma de cada a��o

# a) e b)

par(mfrow=c(2,2))
hist(VALE5, main="Histograma VALE5", col="skyblue")
hist(GOLL4, main="Histograma GOLL4", col="steelblue")
hist(AMBV4, main="Histograma AMBV4", col="dodgerblue")
hist(ITUB4, main="Histograma ITUB4", col="navy")

par(mfrow=c(2,2))
hist(BBDC4, main="Histograma BBDC4", col="indianred")
hist(BVMF3, main="Histograma BVMF3", col="rosybrown")
hist(RAPT4, main="Histograma RAPT4", col="salmon")
hist(MYPK3, main="Histograma MYPK3", col="lavenderblush3")

par(mfrow=c(2,2))
hist(GOAU4, main="Histograma GOAU4", col="powderblue")
hist(LLXL3, main="Histograma LLXL3", col="turquoise")
hist(CSAN3, main="Histograma CSAN3", col="palegreen")
hist(DOLAR, main="Histograma DOLAR", col="springgreen4")

mean(VALE5)
mean(GOLL4)
mean(AMBV4)
mean(ITUB4)
mean(BBDC4)
mean(BVMF3)
mean(RAPT4)
mean(MYPK3)
mean(GOAU4)
mean(LLXL3)
mean(CSAN3)
mean(DOLAR)

sd(VALE5)
sd(GOLL4)
sd(AMBV4)
sd(ITUB4)
sd(BBDC4)
sd(BVMF3)
sd(RAPT4)
sd(MYPK3)
sd(GOAU4)
sd(LLXL3)
sd(CSAN3)
sd(DOLAR)

# c)

par(mfrow=c(2,2))
acf(VALE5, main="S�rie VALE5", col="skyblue")
pacf(VALE5, main="S�rie VALE5", col="skyblue")
acf(GOLL4, main="S�rie GOLL4", col="steelblue")
pacf(GOLL4, main="S�rie GOLL4", col="steelblue")

par(mfrow=c(2,2))
acf(AMBV4, main="S�rie AMBV4", col="dodgerblue")
pacf(AMBV4, main="S�rie AMBV4", col="dodgerblue")
acf(ITUB4, main="S�rie ITUB4", col="navy")
pacf(ITUB4, main="S�rie ITUB4", col="navy")

par(mfrow=c(2,2))
acf(BBDC4, main="S�rie BBDC4", col="indianred")
pacf(BBDC4, main="S�rie BBDC4", col="indianred")
acf(BVMF3, main="S�rie BVMF3", col="rosybrown")
pacf(BVMF3, main="S�rie BVMF3", col="rosybrown")

par(mfrow=c(2,2))
acf(RAPT4, main="S�rie RAPT4", col="salmon")
pacf(RAPT4, main="S�rie RAPT4", col="salmon")
acf(MYPK3, main="S�rie MYPK3", col="lavenderblush3")
pacf(MYPK3, main="S�rie MYPK3", col="lavenderblush3")

par(mfrow=c(2,2))
acf(GOAU4, main="S�rie GOAU4", col="powderblue")
pacf(GOAU4, main="S�rie GOAU4", col="powderblue")
acf(LLXL3, main="S�rie LLXL3", col="turquoise")
pacf(LLXL3, main="S�rie LLXL3", col="turquoise")

par(mfrow=c(2,2))
acf(CSAN3, main="S�rie CSAN3", col="palegreen")
pacf(CSAN3, main="S�rie CSAN3", col="palegreen")
acf(DOLAR, main="S�rie DOLAR", col="springgreen4")
pacf(DOLAR, main="S�rie DOLAR", col="springgreen4")

###############################################################

# Questao 2: grafico ACF e PACF das series abaixo

## d) N(0,1)

x1<-rnorm(200,0,1)
plot.ts(x1, main="S�rie Temporal X1", xlab='tempo',ylab='observa��es', col=6)
par(mfrow=c(1,2))
acf(x1, main="Gr�fico N(0,1)", xlab="defasagem lags", ylab="ACF (autocorrela��o)", col=6)
pacf(x1, main="Gr�fico N(0,1)", xlab="defasagem lags", ylab="PACF (autocorrela��o parcial)", col=6)

## e) xt= xt-1 + N(1,5^2)

e<-rnorm(200,1,25)
x2<-cumsum(e)
plot.ts(x1, main="S�rie Temporal X2", xlab='tempo',ylab='observa��es', col=6)
par(mfrow=c(1,2))
acf(x2,main='Gr�fico xt = xt-1 + N(1,5^2)',xlab='defasagem lags',ylab='ACF (autocorrela��o)', col=6)
pacf(x2,main='Gr�fico xt = xt-1 + N(1,5^2)',xlab='defasagem lags',ylab='PACF (autocorrela��o parcial)', col=6)

## f) xt= 0,7xt-1 + N(0,1)

x3<-arima.sim(n = 200, list(ar = 0.7))
plot.ts(x3, main="S�rie Temporal X3", xlab='tempo',ylab='observa��es', col=6)
par(mfrow=c(1,2))
acf(x3, main="Gr�fico xt = 0,7xt-1 + N(0,1)", xlab="defasagem lags", ylab="ACF (autocorrela��o)", col=6)
pacf(x3, main="Gr�fico xt = 0,7xt-1 + N(0,1)", xlab="defasagem lags", ylab="PACF (autocorrela��o parcial)", col=6)

## g) xt= -0,8xt-1 + N(0,1)

x4<-arima.sim(n = 200, list(ar = -0.8))
plot.ts(x4, main="S�rie Temporal X4", xlab='tempo',ylab='observa��es', col=6)
par(mfrow=c(1,2))
acf(x4, main="Gr�fico xt = -0,8xt-1 + N(0,1)", xlab="defasagem lags", ylab="ACF (autocorrela��o)", col=6)
pacf(x4, main="Gr�fico xt = -0,8xt-1 + N(0,1)", xlab="defasagem lags", ylab="PACF (autocorrela��o parcial)", col=6)

## h) xt= Et + 0,6Et-1, Et~N(0,1)

x5<-arima.sim(list(ar=0.6), n=200)
plot.ts(x5, main="S�rie Temporal X5", xlab='tempo',ylab='observa��es', col=6)
par(mfrow=c(1,2))
acf(x5, main="Gr�fico xt= Et + 0,6Et-1, Et~N(0,1)", xlab="defasagem lags", ylab="ACF (autocorrela��o)", col=6)
pacf(x5, main="Gr�fico xt= Et + 0,6Et-1, Et~N(0,1)", xlab="defasagem lags", ylab="PACF (autocorrela��o parcial)", col=6)


###############################################################

# Questao 3: importar base de dados "skirt"


## a) fazer a leitura da s�rie de dados e tratamentos necess�rios para consider�-la uma s�rie temporal 

# importar arquivo "skirt"
diam.ts<-ts(skirt, frequency=1, start=c(1866))
diam.ts
plot(diam.ts, col=4, main="Di�metro da Saia Feminina: 1866 a 1911")
d.diam.ts<-diff(diam.ts)
plot(d.diam.ts, col=4, main="Diferen�a do Di�metro da Saia Feminina: 1866 a 1911")

## b) decomposi��o da s�rie

# N�o precisa decompor, pois � apenas em anos, n�o tem sazonalidade anual !!!!!

## c) ACF e PACF da s�rie e da diferen�a

par(mfrow=c(1,2))
acf(diam.ts, col=4) # a s�rie n�o � estacion�ria, pois apresenta tend�ncia e n�o converge
pacf(diam.ts, col=4) # n�o sugere nenhuma lag, pois a s�rie n�o � estacion�ria

d.diam.ts<-diff(diam.ts)
par(mfrow=c(1,2))
acf(d.diam.ts, col=4) # a s�rie � estacion�ria, pois converge
pacf(d.diam.ts, col=4) # pacf sugere a lag 1 => AR(1). A lag 6 est� muito pr�xima do zero!

###############################################################

# Questao 4

# 4.a) Processo AR(1) teta0=0, teta1=0.7

Yt1<-arima.sim(model=list(ar=0.7), n=300)
plot(Yt1, lty=1, bty='l', col=8, main="Processo AR(1): teta0=0 e teta1=0.7", ylab=expression(paste(Y[t1])))
par(mfrow=c(1,2))
acf(Yt1, col=2) 
pacf(Yt1, col=2) 

# 4.b) Processo Ar(1) teta0=0, teta1=-0.7
Yt2<-arima.sim(model=list(ar=-0.7), n=300)
plot(Yt2, lty=1, bty='l',col=8, main="Processo AR(1): teta0=0 e teta1=-0.7", ylab=expression(paste(Y[t2])))
par(mfrow=c(1,2))
acf(Yt2, col=2) 
pacf(Yt2, col=2) 

# 4.c) Processo Ar(2) teta0=0, teta1=0.3 e teta2=0.5
Yt3<-arima.sim(model=list(ar=c(0.3,0.5)), n=300)
plot(Yt3, lty=1, bty='l',col=8, main="Processo AR(2): teta0=0, teta1=0.3 e teta2=0.5", ylab=expression(paste(Y[t3])))
par(mfrow=c(1,2))
acf(Yt3, col=2)
pacf(Yt3, col=2)

# 4.d) Processo MA(1) teta0=0, teta1=0.6 
Yt4<-arima.sim(model=list(am=0.6), n=300)
plot(Yt4, lty=1, bty='l',col=8, main="Processo MA(1): teta0=0 e teta1=0.6", ylab=expression(paste(Y[t4])))
par(mfrow=c(1,2))
acf(Yt4, col=2)
pacf(Yt4, col=2)

# 4.e) Processo Ma(1) teta0=0, teta1=-0.6 
Yt5<-arima.sim(model=list(am=-0.6), n=300)
plot(Yt5, lty=1, bty='l',col=8, main="Processo MA(1): teta0=0 e teta1=-0.6", ylab=expression(paste(Y[t5])))
par(mfrow=c(1,2))
acf(Yt5, col=2)
pacf(Yt5, col=2)


###############################################################

# Questao 5: importar base de dados "PIB Brasil"

pib<-scan("http://www.bcb.gov.br/pre/portalCidadao/cadsis/series.asp?idpai=PORTALBCB", skip=1)
PIB.Brasil<-PIB_trabalho

# a) 
attach(PIB.Brasil)
pib.ts<-ts(pib, frequency=4, start=c(1991,1))
pib.ts
plot(pib.ts, col=3, main="PIB Brasil (1� Trim. 1991 - 3� Trim. 2014)")

# b)
decomp.pib.ts<-decompose(pib.ts)
plot(decomp.pib.ts, col=3)

# c)
# importar arquivo PIB_index12
head(PIB_index12)
attach(PIB_index12)
plot(index12, col=3, main="PIB �ltimos 12 anos: 2002 -2014")
dif<-diff(index12)
plot(dif, col=3, main="Diferen�a do PIB �ltimos 12 anos: 2002 -2014")
par(mfrow=c(1,2))
acf(dif, col=3) # sugere que a s�rie � estacion�ria, pois converge
pacf(dif, col=3) # sugere 2 lags (3 e 4)

pib12.arima<-arima(dif, order=c(0,1,2)) 
# order: usar o 0 na primeira casa, pois n�o foi usado o modelo sem diferenca��o
# order: usar o 1 na segunda casa, pois foi aplicada uma diferen�a dos dados
# order: usar 0 2 na terceira casa, pois foram sugeridas 2 lags (3 e 4), com uso de diferencia��o

pib12.arima
pib12.pred<-predict(pib12.arima, n.ahead=2) # n.ahead=2, pois se trata de 2 trimestres adiante
pib12.pred

index12[40:51]
prev.1.trim<-index12[51]+(2.179788)
prev.1.trim
prev.2.trim<-prev.1.trim +(1.171699)
prev.2.trim

############### OP��O ALTERNATIVA USANDO AUTO.ARIMA DIRETAMENTE ####################

# c)

# importar arquivo PIB_index12
head(PIB_index12)
attach(PIB_index12)

auto.arima(index12)
pib12.arima<-arima(dif, order=c(0,1,2)) 
pib12.arima
pib12.pred<-predict(pib12.arima, n.ahead=2) # n.ahead=2, pois se trata de 2 trimestres adiante
pib12.pred

index12[40:51]
prev.1.trim<-index12[51]+(2.179788)
prev.1.trim
prev.2.trim<-prev.1.trim +(1.171699)
prev.2.trim


