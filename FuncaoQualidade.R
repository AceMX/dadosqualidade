# Impossível climatologicamente Verifica se o registro está fora de limites físicos#
# x é a coluna inicial dos dados, já que o DF pode conter coluna de data
# df1 é o DataFrame com os limites a serem analisádos
# limite é o DF de retorno da função
#' @export
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

#' @export
fisico2 <- function(arq1,df1,x)
{
  limite <- arq1
  z<-1
  y<-1
  h<-ncol(arq1)
  c<-h+1
  limite[,c]<-0
  for (j in x:h)
  {
    limite[arq1[,j] < df1[z,1],c] <- 1
    limite[arq1[,j] > df1[z,2],c] <- 1
    
    z<-z+1
    c<-c+1
  }
  return(limite)
}
####################################################################################################

########### Verificação de valores zerados ou iguais consecutivos ###################
#' @export
equalval <- function(arq1,zero,x)
{
  zerado <- arq1
  h<-ncol(arq1)+1
  z<-1 # z controla o andamento do DF de valores zerados informado #
  for(j in x:ncol(arq1))
  {
    for (i in 1:(nrow(arq1)-1))
    {
      ## Verifica se os valores são zerados ##
      if (arq1[i,j] == zero[z])
      {
        zerado[i,h] <- 2
        next
      }
      if (i == (length(arq1)-1))
      {
        if (arq1[i,j] == zero[z])
        {
          zerado[i,h] <- 2
        }
      }
      ## Verifica se os valores são iguais ao próximo ##
      if (arq1[i,j] == arq1[i+1,j])
      {
        zerado[i+1,h] <- 1
      }
    }
    
      h<-h+1 ## Incrementa o índice para criar a próxima coluna para aplicar a marcação da verificação ##
      z<-z+1 ## Incrementa o índice para comprar o zerado da próxima variável ##
  }
  return(zerado)
}

########### Verificação de valores zerados ou iguais consecutivos ###################
#' @export
equalval2 <- function(arq1,zero,x)
{
  zerado <- arq1
  
  h<-ncol(arq1)+1
  z<-1 # z controla o andamento do DF de valores zerados informado #
  for(j in x:ncol(arq1))
  {
    zerado[,h] = NA
    pos = c(FALSE, diff(arq1[,j]) == 0)
    pos[is.na(pos)] = FALSE
    zerado[pos, h] = 1
    
    pos = zerado[,j] == zero[z]
    pos[is.na(pos)] = FALSE
    zerado[pos, h] = 2
    
    h<-h+1 ## Incrementa o índice para criar a próxima coluna para aplicar a marcação da verificação ##
    z<-z+1 ## Incrementa o índice para comprar o zerado da próxima variável ##
  }
  return(zerado)
}
#######################################################################################

################### Verificação de Outliers ############################
#' @export
outlier <- function(arq1,x) #x é a variável para identificar a partir de qual coluna a análise deve acontecer #
{
  out <- arq1
  h<-ncol(arq1) # identifica quantas colunas o arquivo passado possui #
  c<-h+1 # controle para inserir colunas no final do DF de saída #
  for (j in x:h)
  {
    dp <- sd(arq1[,j],na.rm=TRUE)
    md <- mean(arq1[,j],na.rm=TRUE)
    out[,c]<-ceiling(abs(arq1[,j]-md)/dp) # realiza o cálculo para identificar a quantas vezes o valor está do desvio padrão do conjunto em análise #
    c<-c+1
  }
  return(out)
}
#######################################################################

###### Verificar quais datas estão com registro falho ######
#' @export
falha <- function(arq1,dat,med)
{
  
  int_medida <- med # Quantidade de medidas diárias esperada #
  lista <- NA # Cria lista de datas com número de registros incompletos #
  i <- 1 
  c <- dat # Coluna com a data
  while (i < nrow(arq1))
  {
    cont <- 0
    j <- i
    while (arq1[i,c] == arq1[j,c])
    {
      cont <- cont+1 # Contador de registros no dia #
      j <- j+1
      if (is.na(arq1[j,c]))
      {
        if (cont != int_medida)
        {
          if (lista == 0)
          {
            lista <- arq1[j,c]
          }
          if (lista != 0)
          {
            lista <- rbind(lista,arq1[j,c])
          }
        }
        break
      }
    }
    if (cont != int_medida) # Se encontrou intervalo de data com menos medidas que o esperado #
    {
      if (is.na(lista)) # Verifica se a lista de falhas está vazia #
      {
        lista <- arq1[j-1,c] # lista de ocorrência recebe o registro anterior, identificando o dia com falta de dados #
      }else
      {
        lista <- rbind(lista,arq1[j-1,c])
      }
    }
    i<-j
  }
  return(data.frame(lista)) # retorna o DF com a lista de datas com falta de dados #
}

#' @export
falha2 <- function(arq1,dat,med)
{
  
  int_medida <- med # Quantidade de medidas diárias esperada #
  lista <- NA # Cria lista de datas com número de registros incompletos #
  i <- 1 
  c <- dat # Coluna com a data
  h = dim(arq1)[2]+1
  res = ftable(arq1)
  soma = rowSums(res, na.rm = TRUE)
  
  colunas = colnames(arq1)
  dias = levels(sub[[colunas[c]]])

  return (dias[soma!=int_medida])
  #return(data.frame(lista)) # retorna o DF com a lista de datas com falta de dados #
}
###############################################################
