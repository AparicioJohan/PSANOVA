

# BASE DE DATOS INTERSECCIÓN 

# Base de datos  ####

rm(list=ls())

setwd("D:/APARICIO JOHAN/OneDrive - CGIAR/Modelo Espacial y Alfa lattice")

#setwd("C:/Users/Johan/OneDrive - CGIAR/Modelo Espacial y Alfa lattice")
# Check the required packages are installed. Install them otherwise.

list.of.packages = c("lme4","SpATS")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lme4)
library(SpATS)
library(ggplot2)

# Load the data and split by dataset

VEFdata = read.csv('all_merged_to_send.csv')

Pal17C = subset(VEFdata, dataset == 'Pal17C_drt')
Dar16C = subset(VEFdata, dataset == 'Dar16C_hiP')
Pal14C = subset(VEFdata, dataset == 'Pal14C_drt')

for (coln in colnames(VEFdata)){
  
  if (is.factor(VEFdata[,coln])){
    
    Pal17C[,coln] = factor(Pal17C[,coln])
    Dar16C[,coln] = factor(Dar16C[,coln])
    Pal14C[,coln] = factor(Pal14C[,coln])
    
  }
}

rm(coln,list.of.packages,new.packages)




# lineas compartidas entre ensayos  ####

cat("Total Lineas en Pal17C  ") ; linepal17c <- sort(unique(Pal17C$line)) ; length(linepal17c) 
cat("Total Lineas en Pal14C  ") ; linepal14c <- sort(unique(Pal14C$line)) ; length(linepal14c)
cat("Total Lineas en Dar16c  ") ; linedar16c <- sort(unique(Dar16C$line)) ; length(linedar16c)


library(gplots)

a <- as.vector(linepal17c)
b <- as.vector(linepal14c)
c <- as.vector(linedar16c)


my_list <- list(Palmira17 =a,Palmira14= b,Darien16= c)
venn(my_list)

# save object from venn
my_venn <- venn(my_list, show.plot = FALSE)
class(my_venn)


# which object class?
library(pryr)
otype(my_venn)


# what are the attributes names?
names(attributes(my_venn))


# use attr() to get intersections
w <- attr(x = my_venn, "intersections")$`Palmira17:Palmira14:Darien16`
w <- as.vector(w)
class(w)
length(w)


#  Base de datos interseccion ####

Pal17C_Int <- subset(Pal17C, Pal17C$line%in%w); Pal17C_Int$Loc <- "(3) Pal17c"
Pal14C_Int <- subset(Pal14C, Pal14C$line%in%w); Pal14C_Int$Loc <- "(1) Pal14c"
Dar16C_Int <- subset(Dar16C, Dar16C$line%in%w); Dar16C_Int$Loc <- "(2) Dar16c"


windows()
ggplot(Pal17C_Int, aes(col,row,color=rep))+ geom_point()

windows()
ggplot(Pal14C_Int, aes(col,row,color=rep))+ geom_point()

windows()
ggplot(Dar16C_Int, aes(col,row,color=rep))+ geom_point()


consolidado <- rbind(Pal14C_Int,Dar16C_Int,Pal17C_Int)

# YDHPL

windows()
p <- ggplot(consolidado, aes(x = Loc, y = YDHPL, fill=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw(base_size = 15)+
  geom_jitter(position=position_jitter(width=0.1,height=0),alpha=0.2, size=1.5 , aes( colour=Loc))+
  xlab("")+ ylab("YDHPL Kg/Ha")
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/int-ydhpl.pdf",width = 8,height = 6)
p
dev.off()


windows()
ggplot(consolidado, aes(x = Loc, y = YDHPL, color=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw()+facet_wrap(~rep)+
  geom_jitter(position=position_jitter(width=0.1,height=0),alpha=0.2, size=1.5)+
  xlab("")


#  DF



windows()
p <- ggplot(consolidado, aes(x = Loc, y = DF, fill=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw(base_size = 15)+
  xlab("")+ ylab("Days to Flowering")
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/int-df.pdf",width = 8,height = 6)
p
dev.off()


windows()
ggplot(consolidado, aes(x = Loc, y = DF, color=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw()+facet_wrap(~rep)+
  xlab("")



#  DPM



windows()
p <- ggplot(consolidado, aes(x = Loc, y = DPM, fill=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw(base_size = 15)+
  xlab("")+ ylab("Days to Physiological Maturity")
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/int-dpm.pdf",width = 8,height = 6)
p
dev.off()


windows()
ggplot(consolidado, aes(x = Loc, y = DPM, color=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw()+facet_wrap(~rep)+
  xlab("")



#  SW100



windows()
p <- ggplot(consolidado, aes(x = Loc, y = SW100, fill=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw(base_size = 15)+
  geom_jitter(position=position_jitter(width=0.1,height=0),alpha=0.2, size=1.5, aes( colour=Loc))+
  xlab("")+ ylab("Seed Weight 100")
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/int-sw100.pdf",width = 8,height = 6)
p
dev.off()


windows()
ggplot(consolidado, aes(x = Loc, y = SW100, color=Loc))+ 
  geom_boxplot( outlier.colour = "black",outlier.size = 1.5,outlier.alpha = 0.3)+theme_bw()+facet_wrap(~rep)+
  xlab("")




# correlaciones entre los blups interaccion genotipo ######


Pal17C_Int$col_f = factor(Pal17C_Int$col)
Pal17C_Int$row_f = factor(Pal17C_Int$row)
ncols = max(Pal17C_Int$col, na.rm=T) ;  ncols=length(unique(Pal17C_Int$col))
nrows = max(Pal17C_Int$row, na.rm=T) ;  nrows=length(unique(Pal17C_Int$row)) 


Pal14C_Int$col_f = factor(Pal14C_Int$col)
Pal14C_Int$row_f = factor(Pal14C_Int$row)
ncols = max(Pal17C_Int$col, na.rm=T) ;  ncols=length(unique(Pal14C_Int$col))
nrows = max(Pal17C_Int$row, na.rm=T) ;  nrows=length(unique(Pal14C_Int$row)) 


Dar16C_Int$col_f = factor(Dar16C_Int$col)
Dar16C_Int$row_f = factor(Dar16C_Int$row)
ncols = max(Dar16C_Int$col, na.rm=T) ;  ncols=length(unique(Dar16C_Int$col))
nrows = max(Dar16C_Int$row, na.rm=T) ;  nrows=length(unique(Dar16C_Int$row)) 

# YDHPL SPATIAL MODELS

fit.Pal17 = SpATS(response='YDHPL', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal17C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.Pal14 = SpATS(response='YDHPL', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal14C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.dar16 = SpATS(response='YDHPL', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Dar16C_Int,
                  control = list(tolerance=1e-03, monitoring=1))


getHeritability(fit.Pal17)
getHeritability(fit.Pal14)
getHeritability(fit.dar16)

# BLUPS para los 3 modelos
Adj.1 = predict(fit.Pal17, which = "line")
Adj.2= predict(fit.Pal14, which = "line")
Adj.3= predict(fit.dar16, which = "line")
blups <- data.frame(BLUPS_Pal17c=Adj.1$predicted.values,BLUPS_Pal14c=Adj.2$predicted.values,BLUPS_Dar16C=Adj.3$predicted.values)
windows()
plot(blups)
abline(0,1,col='red')

library("GGally")


lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

windows()
p <- ggpairs(
  blups, lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 10))
)

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/correblups.pdf",width = 8,height = 6)
p
dev.off()


# Days to flowering SPATIAL MODELS



fit.Pal17 = SpATS(response='DF', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal17C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.Pal14 = SpATS(response='DF', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal14C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.dar16 = SpATS(response='DF', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Dar16C_Int,
                  control = list(tolerance=1e-03, monitoring=1))


getHeritability(fit.Pal17)
getHeritability(fit.Pal14)
getHeritability(fit.dar16)

# BLUPS para los 3 modelos
Adj.1 = predict(fit.Pal17, which = "line")
Adj.2= predict(fit.Pal14, which = "line")
Adj.3= predict(fit.dar16, which = "line")
blups <- data.frame(BLUPS_Pal17c=Adj.1$predicted.values,BLUPS_Pal14c=Adj.2$predicted.values,BLUPS_Dar16C=Adj.3$predicted.values)
windows()
plot(blups)
abline(0,1,col='red')

library("GGally")


lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

windows()
p <- ggpairs(
  blups, lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 10))
)
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/correblupsdf.pdf",width = 8,height = 6)
p
dev.off()


# Days to phisyological  maturity SPATIAL MODELS



fit.Pal17 = SpATS(response='DPM', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal17C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.Pal14 = SpATS(response='DPM', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal14C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.dar16 = SpATS(response='DPM', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Dar16C_Int,
                  control = list(tolerance=1e-03, monitoring=1))


getHeritability(fit.Pal17)
getHeritability(fit.Pal14)
getHeritability(fit.dar16)

# BLUPS para los 3 modelos
Adj.1 = predict(fit.Pal17, which = "line")
Adj.2= predict(fit.Pal14, which = "line")
Adj.3= predict(fit.dar16, which = "line")
blups <- data.frame(BLUPS_Pal17c=Adj.1$predicted.values,BLUPS_Pal14c=Adj.2$predicted.values,BLUPS_Dar16C=Adj.3$predicted.values)
windows()
plot(blups)
abline(0,1,col='red')

library("GGally")


lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

windows()
p <- ggpairs(
  blups, lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 10))
)
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/correblupsdpm.pdf",width = 8,height = 6)
p
dev.off()



# seed weight SPATIAL MODELS



fit.Pal17 = SpATS(response='SW100', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal17C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.Pal14 = SpATS(response='SW100', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Pal14C_Int,
                  control = list(tolerance=1e-03, monitoring=1))

fit.dar16 = SpATS(response='SW100', genotype='line', genotype.as.random=T, fixed=NULL , 
                  spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3), nest.div=2),
                  random = ~ row_f + col_f , data=Dar16C_Int,
                  control = list(tolerance=1e-03, monitoring=1))


getHeritability(fit.Pal17)
getHeritability(fit.Pal14)
getHeritability(fit.dar16)

# BLUPS para los 3 modelos
Adj.1 = predict(fit.Pal17, which = "line")
Adj.2= predict(fit.Pal14, which = "line")
Adj.3= predict(fit.dar16, which = "line")
blups <- data.frame(BLUPS_Pal17c=Adj.1$predicted.values,BLUPS_Pal14c=Adj.2$predicted.values,BLUPS_Dar16C=Adj.3$predicted.values)
windows()
plot(blups)
abline(0,1,col='red')

library("GGally")


lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

windows()
p <- ggpairs(
  blups, lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 10))
)
p

pdf("D:/APARICIO JOHAN/OneDrive - CGIAR/Documento presentacion 4/Presentacion/ima/correblupssw100.pdf",width = 8,height = 6)
p
dev.off()



