# Impossível climatologicamente Verifica se o registro está fora de limites físicos#
# x é a coluna inicial dos dados, já que o DF pode conter coluna de data
# df1 é o DataFrame com os limites a serem analisádos
# limite é o DF de retorno da função
fisico <- function(arq1,df1,x)
{
  limite <- arq1
  z<-1
  y<-1
  h<-ncol(arq1)
  c<-h+1
  limite[,c]<-0
  for (j in x:h)
  {
    for (i in 1:nrow(arq1))
    {
      if (arq1[i,j] > df1[z,y+1] || arq1[i,j] < df1[z,y])
      {
        limite[i,c] <- 1
      }
    }
    z<-z+1
    c<-c+1
    if (j+1 <= h)
    {
      limite[,c]<-0
    }
  }
  return(limite)
}
####################################################################################################


  # Valores iguais ou zerados, pode indicar algum problema de operação do equipamento #
  for (i in 1:(nrow(arq1)-1))
  {
    if (arq1[i,x] == arq1[i+1,x])
    {
      arq1[i+1,y+2] <- 1;
    }
    if (arq1[i,x] == 0)
    {
      arq1[i,y+2] <- 1;
    }
    if (i == (length(arq1)-1))
    {
      if (arq1[i,x] == 0)
      {
        arq1[i,y+2] <- 1;
      }
    }
  }
  
  # Verificação de possíveis outliers por Desvio Padrão #
  for (i in 1:(nrow(arq1)))
  {
    dp <- sd(arq1[,x]);
    md <- mean(arq1[,x]);
    if (arq1[i,x]<=(md+dp) && arq1[i,x] >= (md-dp))
    {
      arq1[i,y+3] <- 1;
    }else if (arq1[i,x]<=(md+(dp*2)) && arq1[i,x] >= (md-(dp*2)))
    {
      arq1[i,y+3] <- 2;
    }else if (arq1[i,x]<=(md+(dp*3)) && arq1[i,x] >= (md-(dp*3)))
    {
      arq1[i,y+3] <- 3
    }else {arq1[i,y+3] <- 4}
  }
  return(arq1)
}

# Análise de quantidade de registros esperados pelo registrado #

dias <- 730
leit_hora <- (60/30)
leit_dia <- (leit_hora*24)
qtde_dados <- (leit_dia*dias)

new <- do.call(rbind, strsplit(as.character(new[,3])," "))
temp$Ano <- new[,1]

cj_analise <- subset(temp, Ano=='2016', select = c(V1,V2,V2,V3,V4,V5,Ano))

dados <- nrow(cj_analise)

Percent <- round(((dados*100)/qtde_dados),2)

teste2 <- teste
teste <- arq1

### Verificar quais datas estã com registro falho ###

int_medida <- 3#48 # Quantidade de medidas diárias esperada #
lista <- NA
i <- 1 
c <- 2 # Coluna com a data
while (i < nrow(teste))
{
  cont <- 0
  j <- i
  while (teste[i,c] == teste[j,c])
  {
    cont <- cont+1
    j <- j+1
    if (is.na(teste[j,1]))
    {
      if (cont != int_medida)
      {
        if (lista == 0)
        {
          lista <- teste[j,]
        }
        if (lista != 0)
        {
          lista <- rbind(lista,teste[j,])
        }
      }
      break
    }
  }
  if (cont != int_medida)
  {
    if (is.na(lista))
    {
      lista <- teste[j-1,]
    }else
    {
      lista <- rbind(lista,teste[j-1,])
    }
  }
  i<-j
}
