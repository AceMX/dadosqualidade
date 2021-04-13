setwd("~/Codigo/git/dadosqualidade")

individual = read.csv('individual.csv',dec=',',sep=';')

individual['pZerados'] = individual['zerados']/individual['qtd']*100
individual['pFalhas'] = individual['falhas']/individual['qtd']*100
individual['pOutliers'] = individual['outliers']/individual['qtd']*100
n = 'RADIACAO.GLOBAL..Kj.m..'
individual[individual['variavel'] == n,6]='RADIACAO.GLOBAL..KJ.m..'
individual = droplevels(individual)
niveis = levels(individual$variavel)

path = './graficos/estado/boxplot'
for(n in niveis){
  print(n)
  filtro = individual[individual['variavel'] == n,]
  res = summary(aov(filtro$pFalhas~filtro$estado))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Falhas',n,'p value:',pvalor)
  boxplot(filtro$pFalhas~filtro$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pFalhas',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
  
  res = summary(aov(filtro$pZerados~filtro$estado))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Zerados',n,'p value:',pvalor)
  boxplot(filtro$pZerados~filtro$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pZerados',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
  
  res = summary(aov(filtro$pOutliers~filtro$estado))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Outliers',n,'p value:',pvalor)
  boxplot(filtro$pOutliers~filtro$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pOutliers',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
}

res = summary(aov(individual$pFalhas~individual$estado))
pvalor = res[[1]][[5]][1]
titulo = paste('Falhas p value:',pvalor)
boxplot(individual$pFalhas~individual$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pFalhas.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)

filtro = individual[individual['variavel'] != niveis[1],]
res = summary(aov(filtro$pZerados~filtro$estado))
pvalor = res[[1]][[5]][1]
titulo = paste('Zerados p value:',pvalor)
boxplot(filtro$pZerados~filtro$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pZerados.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)

res = summary(aov(individual$pOutliers~individual$estado))
pvalor = res[[1]][[5]][1]
titulo = paste('Outliers p value:',pvalor)
boxplot(individual$pOutliers~individual$estado,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pOutliers.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)

path = './graficos/regiao/boxplot'
for(n in niveis){
  print(n)
  filtro = individual[individual['variavel'] == n,]
  res = summary(aov(filtro$pFalhas~filtro$regiao))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Falhas',n,'p value:',pvalor)
  boxplot(filtro$pFalhas~filtro$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pFalhas',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
  
  res = summary(aov(filtro$pZerados~filtro$regiao))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Zerados',n,'p value:',pvalor)
  boxplot(filtro$pZerados~filtro$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pZerados',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
  
  res = summary(aov(filtro$pOutliers~filtro$regiao))
  pvalor = res[[1]][[5]][1]
  titulo = paste('Outliers',n,'p value:',pvalor)
  boxplot(filtro$pOutliers~filtro$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
  arquivo = paste(path,'/pOutliers',sep='')
  arquivo = paste(arquivo,n,sep='_')
  arquivo = paste(arquivo,'.png',sep='')
  dev.print(device=png, arquivo,width=1024,height=768)
}

res = summary(aov(individual$pFalhas~individual$regiao))
pvalor = res[[1]][[5]][1]
titulo = paste('Falhas p value:',pvalor)
boxplot(individual$pFalhas~individual$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pFalhas.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)

filtro = individual[individual['variavel'] != niveis[1],]
res = summary(aov(filtro$pZerados~filtro$regiao))
pvalor = res[[1]][[5]][1]
titulo = paste('Zerados p value:',pvalor)
boxplot(filtro$pZerados~filtro$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pZerados.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)

res = summary(aov(individual$pOutliers~individual$regiao))
pvalor = res[[1]][[5]][1]
titulo = paste('Outliers p value:',pvalor)
boxplot(individual$pOutliers~individual$regiao,range=c(0,1000),ylab='Porc (%)',main=titulo)
arquivo = paste(path,'/pOutliers.png',sep='')
dev.print(device=png, arquivo,width=1024,height=768)