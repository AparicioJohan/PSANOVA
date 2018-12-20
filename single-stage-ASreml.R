

rm(list = ls())

library(asreml)
library(SpATS)
library(lme4)
library(dplyr)
library(nadiv)
library("data.table")
library(sommer)
library(tidyverse)

VEFdata = data.table(read.csv('D:/APARICIO JOHAN/OneDrive - CGIAR/Modelo Espacial y Alfa lattice/all_merged_to_send.csv'))
Locations=factor(unique(VEFdata$dataset))  # Eliminar Darien  positions 7,8,9
VEFdata <- VEFdata[ VEFdata$dataset%in%Locations[-c(7,8,9)], ] ; VEFdata$dataset <- factor(VEFdata$dataset)

# Previous generation
VEFdata$PG <- (substr(VEFdata$previous_generation,1,5 ))

# Numero de lineas dentro de cada Localidad
VEFdata <- tbl_df(VEFdata)
n <- VEFdata%>%
  group_by(dataset) %>%
      summarise(n_distinct(line))

# Tabla comparacion del numero de lineas por localidad
vloc <- levels(VEFdata$dataset)
Comp <- matrix(NA,nrow = nlevels(VEFdata$dataset),ncol = nlevels(VEFdata$dataset))
for (i in 1:nlevels(VEFdata$dataset)) {
  for (j in 1:nlevels(VEFdata$dataset)) {
    Comp[i,j] <-  summary(unique(filter(VEFdata,dataset==vloc[i])$line)%in%unique(filter(VEFdata,dataset==vloc[j])$line))[3]
    if(i>j) Comp[i,j] <- NA
    if(i==4&j==5)  Comp[i,j] <- 380
  }
}
diag(Comp) <- n$`n_distinct(line)` ; colnames(Comp) <- vloc ; row.names(Comp) <- vloc ; Comp <- as.data.frame(Comp)
xtable::xtable(Comp)
  
# Add PG Pal16C-drt
pb <- winProgressBar(title = "progress bar", min = 0,max = 1188, width = 300)
TIME <- Sys.time()
for (i in 1:1188) {
  if(is.na(VEFdata[VEFdata$dataset=="Pal16C_drt",]$PG[i])) VEFdata[VEFdata$dataset=="Pal16C_drt",]$PG[i] <- "16ADB" 
  txtProgressBar(min = 0,max = 1,initial = 0 )
  setWinProgressBar(pb, i, title=paste( round(i/1188*100, 0),"% done"))
}
close(pb)
Sys.time() - TIME


"#############################
#   Modelo multiambiental   #
#############################"


# Funciones para  Eliminar duplicados en fila/columna y para crear grilla completa de datos
dup <- function(data,col="col",row="row"){
  dup <- data[,c(col,row)][duplicated(data[,c(col,row)]), ] 
  data  <-data[ which(!duplicated(data[,c(col,row)])) , ]
  return(data)
}
coords <- function(col,row,data) {
  deparse(substitute(data))
  x.coord <- data[,col]
  y.coord <- data[,row]
  
  columns <- seq(min(x.coord), max(x.coord), by = min(diff(sort(unique(x.coord)))))
  rows <- seq(min(y.coord), max(y.coord), by = min(diff(sort(unique(y.coord)))))
  
  xy.coord <- data.table(expand.grid(col = columns, row = rows))
  h <- merge(xy.coord, data, by.x = c( "row","col"), by.y = c( "row","col"), 
             all.x = TRUE, all.y = TRUE)
  data <- arrange(h,col,row)
  
  return(data)
  
}



# Base de datos por localidad
VEFdata <- as.data.frame(VEFdata)
Pal13C_drt = subset(VEFdata, dataset == 'Pal13C_drt');Pal14A_irr = subset(VEFdata, dataset == 'Pal14A_irr');
Pal14C_drt = subset(VEFdata, dataset == 'Pal14C_drt');Pal15C_drt = subset(VEFdata, dataset == 'Pal15C_drt') 
Pal15C_irr = subset(VEFdata, dataset == 'Pal15C_irr');Pal16C_drt = subset(VEFdata, dataset == 'Pal16C_drt')
Dar16C_loP = subset(VEFdata, dataset == 'Dar16C_loP');Dar16C_mdP = subset(VEFdata, dataset == 'Dar16C_mdP')
Dar16C_hiP = subset(VEFdata, dataset == 'Dar16C_hiP');Pal17C_drt = subset(VEFdata, dataset == 'Pal17C_drt')
Pal18A_irr = subset(VEFdata, dataset == 'Pal18A_irr')

for (coln in colnames(VEFdata)){
  if (is.factor(VEFdata[,coln])){
    Pal13C_drt[,coln] = factor(Pal13C_drt[,coln]);Pal14A_irr[,coln] = factor(Pal14A_irr[,coln]);Pal14C_drt[,coln] = factor(Pal14C_drt[,coln])
    Pal15C_drt[,coln] = factor(Pal15C_drt[,coln]);Pal15C_irr[,coln] = factor(Pal15C_irr[,coln]);Pal16C_drt[,coln] = factor(Pal16C_drt[,coln])
    Dar16C_loP[,coln] = factor(Dar16C_loP[,coln]);Dar16C_mdP[,coln] = factor(Dar16C_mdP[,coln]);Dar16C_hiP[,coln] = factor(Dar16C_hiP[,coln])
    Pal17C_drt[,coln] = factor(Pal17C_drt[,coln]);Pal18A_irr[,coln] = factor(Pal18A_irr[,coln])
  }
}

# Ensayos Por localidad
names.ensayos <- vloc
ensayos.all <-matrix(data=list(), nrow=8, ncol=1, dimnames=list(vloc, c("Data")))
for (i in 1:length(names.ensayos)){
  ensayos.all[[i]] <- dup(get(names.ensayos[i]))
  ensayos.all[[i]] <- coords(col = "col",row = "row",ensayos.all[[i]])
  ensayos.all[[i]] <- arrange(ensayos.all[[i]],col,row)
}


# Localidades 
for (i in 1:length(vloc)) {
  ensayos.all[[i]]$dataset[is.na(ensayos.all[[i]]$dataset)] <- ensayos.all[[i]]$dataset[!is.na(ensayos.all[[i]]$dataset)][1] 
}
Grilla.complet <- data.frame()
for ( i in 1:length(vloc)){
  Grilla.complet <- rbind(Grilla.complet,ensayos.all[[i]])
}


# Factores
Grilla.complet$col_f <-  as.factor(Grilla.complet$col)
Grilla.complet$row_f <-  as.factor(Grilla.complet$row)

 # Modelo numero 1
a <-Sys.time()
fit.asreml <- asreml(fixed = YDHPL ~ 1  ,
                     random = ~ line+ dataset+dataset:line + at(dataset,c(7,8)):PG,
                     rcov = ~ at(dataset):ar1(col_f):ar1(row_f),  # random = ~ line+ dataset + line:dataset + dataset:rep +dataset:rep:block + at(dataset,c(7,10)):PG,
                     maxiter = 100, trace = FALSE, na.method.X = "include", na.method.Y = "include",
                     data = Grilla.complet,
                     control = asreml.control(workspace =32e7,pworkspace=32e7))  # ,control = asreml.control(workspace =10*100e6,pworkspace=10*100e6)
b <-Sys.time(); b -a

# Blups genotipo ambiente
blups <-  data.table(predict(fit.asreml, classify="dataset:line")$pred$pvals[1:3]);
save.image(file = "D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 12/asreml.Rdata")

# resumen genotipo ambiente

blups <- tbl_df(blups)
blups %>% 
  group_by(dataset) %>%
    summarise(n())

k <- matrix(NA,nrow = 1009,ncol = 8)
blups <- as.data.frame(blups)
for(i in 1:8){
  k[,i] <- blups[blups==vloc[i],3]
}
k <- as.data.frame(k)


re <- residuals(fit.asreml)
re1 <- matrix(re[1:720],40,18)
windows()
levelplot(re1,par.settings = RdBuTheme)


windows()
plot(fit.asreml)
  