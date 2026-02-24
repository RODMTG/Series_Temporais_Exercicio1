


setwd("C:\Users\Labest III\Desktop\Disciplinas\Análise de Séries Temporais")

library(readxl)
atmosfera <- read_excel("atmosfera.xls")
View(atmosfera)

install.packages("TSA")
install.packages("tseries")
install.packages("astsa")
install.packages("fUnitRoots")

require(TSA)
require(tseries)
require(astsa)
require(fUnitRoots)

#Digitando a série de dados no Script do Software
  #Série temporal (digitar os dados ou copiar e colar no Prompt do Software)
dados<-c(1626,1579,1738,1427,1701,1792,1656,1767,1769,1722,1624,
         1414,1472,1458, 1553, 1577,1806,1783,1546,1806,1791,1938,1709,1866,1621,
         1403,1450,1661, 1764, 1550, 1416,1532,1544,1560,1562,1460,1681,1342,1608,
         1647,1775,1784, 1473, 1743,1857, 1720,1544,1206,1416,1232,1445,1435,1600,
         1502, 1548,1539, 1526,1681,1665,1357, 1310,1292,1401,1345,1555,1489, 1335,
         1029,1103,1050, 1156,1074,1056,1044,1113,1129,1092, 996, 950,963,914,910,
         1199,1354,1313, 1311,1182,1245,1110,1045,1192,1077,1232,1329, 1269,1220)
plot(dados, type = "l")

#############TESTE DICKEY FULLER (Teste da Raiz Unitária)#########
adf <- adf.test(dados, lags = 1, type = c("nc", "c", "ct"),
                title = adf1, description = adf2)
adf
adf2 <- adf.test(dados)
adf2

#_____DADOS TEMPERATURA E UMIDADE___________________________#



temperatura <- c(18.3999999999999,atmosfera$`18.399999999999999`)
umidade <- c(atmosfera$umidade)



#___________________PLOTS E TESTE___________________________#

plot(temperatura, type = "l")

adf_temperatura <- adf.test(temperatura)
adf_temperatura

plot(umidade, type = "l")

adf_umidade <- adf.test(umidade)
adf_umidade

?adf.test

?htest

x <- rnorm(1000)  # no unit-root
adf.test(x)

y <- diffinv(x)   # contains a unit-root
adf.test(y)

#_______________________________________#


