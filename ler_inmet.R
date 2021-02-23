setwd("~/Codigo/git/dadosqualidade")
source("FuncaoQualidade.R")

folder = './INMET/'
folderResp='./clusterResp/'
arqs = list.files(folder,pattern='*',recursive=TRUE)


geral = data.frame(ano=integer(),regiao=character(),estado=character(),estacao=character(),qtd=integer(), stringsAsFactors=FALSE)

#ADICIONAR AS COLUNAS SOBRE QUALIDADE DOS DADOS
resp = data.frame(ano=integer(),regiao=character(),estado=character(),estacao=character(),qtd=integer(),variavel=character(),min=double(),max=double(),media=double(),desvio=double(),zerados=integer(), stringsAsFactors=FALSE)

for (arq in arqs){
  print(arq)
  metadados = unlist(strsplit(arq,'/'))
  ano = metadados[[1]]
  path = metadados[[2]]
  partes = unlist(strsplit(path,'_'))
  regiao = partes[[2]]
  estado = partes[[3]]
  codtorre = partes[[4]]
  torre = partes[[5]]
  dados = read.csv(paste(folder,arq,sep=''),sep=';',dec=',',header=TRUE,skip=8,fileEncoding = "ISO-8859-1")
  qtd = dim(dados)[1]

  linha = c(ano, regiao, estado, torre,qtd)
  geral[nrow(geral) + 1,] = linha

  colunas = colnames(dados)
  # COMEÇA DO 3 POIS A COLUNA 1 E 2 REPRESENTADA DATA E HORA RESPECTIVAMENTE
  for (i in seq(3,length(colunas))) {
    col = colunas[i]
    col = iconv(col,to="ASCII//TRANSLIT")
    
    sub = dados[c(1,2)]
    #falhas = falha2(sub,1,3)
    res = ftable(sub)
    soma = rowSums(res)
    print(colnames(res))
    #print(res[soma==24,1])
    break
    
    #A ULTIMA COLUNA É UMA COLUNA VAZIA COM O NOME DE 'X'
    if(col != 'X'){
      x = dados[i]
      #REMOVE OS VALORES IGNORADOS PARA VERIFICAR O MINIMO
      x = x[x != -9999]
      
      #print(col)
      sub = dados[c(1,2,i)]
      zeros2 = equalval2(sub,c(0),3)
      
      zerados = sum(!is.na(zeros2[,4]))
      
      # fisicos = data.frame(min=double(),max=double(), stringsAsFactors=FALSE)
      # fisicos[nrow(fisicos) + 1,] = c(0,45)
      # fis = fisico2(sub,fisicos,3)
      
      #ADICIONAR AS COLUNAS SOBRE QUALIDADE DOS DADOS
      linha = c(ano, regiao, estado, torre,qtd,col,min(x,na.rm = TRUE),max(x,na.rm = TRUE),mean(x,na.rm=TRUE),sd(x,na.rm=TRUE),zerados)
      resp[nrow(resp) + 1,] = linha
      break
    }
  }
  break
}

# write.csv(geral,'geral.csv',dec=',',sep=';')
# write.csv(resp,'individual.csv',dec=',',sep=';')
