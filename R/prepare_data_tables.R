### prepare data tables ####

#### upload libraries and functions ##

library(ggplot2)
library(tidyverse)
library(stringr)
library(GGally)
library(ggpubr)
library(FD)
library(tripack)
library(geometry)
library(matrixStats)
library(vegan)    
library(ape)

source("R/quality_funct_space.R")


# read raw data  
dim2d <- read.csv("data/DataSheet_Mal_may18.csv", h=T)

# clead and ready data 
str(dim2d)
dim2d$Species <- as.factor(dim2d$Species)
dim2d$Stained <- as.factor(dim2d$Stained)
dim2d$Destination <- as.factor(dim2d$Destination)
dim2d$Origin <- as.factor(dim2d$Origin)
dim2d$Genotype <- as.factor(dim2d$Genotype)
dim2d$rack <- as.factor(str_c("rack ",dim2d$Rack))
dim2d$Rack <- as.factor(dim2d$Rack)
dim2d$CoralID <- as.factor(dim2d$CoralID)
str(dim2d)
dim(dim2d)

# save data for mortality 
mort <- dim2d
#write.csv(mort,"output/mort.csv")

dim2d <- as.data.frame(dim2d %>%
                         drop_na())
dim(dim2d)
summary(dim2d)

## check d_f max, mlVol_i min e max and check data
dim2d$CoralID[dim2d$d_f == max(dim2d$d_f)] # PR_D_Y_S_D_08
dim2d$d_f[dim2d$CoralID == dim2d$CoralID[dim2d$d_f == max(dim2d$d_f)]] <- 7.82
dim2d$CoralID[dim2d$mlVol_i == max(dim2d$mlVol_i)]  #AM_D_N_N_D_03
dim2d$mlVol_i[dim2d$CoralID == dim2d$CoralID[dim2d$mlVol_i == max(dim2d$mlVol_i)]] <- 111.5
dim2d$CoralID[dim2d$mlVol_i == min(dim2d$mlVol_i)] #PR_D_R_N_D_03
dim2d$mlVol_i[dim2d$CoralID == dim2d$CoralID[dim2d$mlVol_i == min(dim2d$mlVol_i)]] <- 100.5
dim2d$L_i[dim2d$CoralID == "AD_S_G_S_D_08"] <- 4.56
## nubbin without PA and peri 
dim2d

## prepare long format table ###

dim2dl <- gather(dim2d,"par", "val", c(10:14,16:20))
str(dim2dl)

dim2dl$time <- NA

## add time based on file name 
dim2dl$time[str_detect(dim2dl$par, "_i")] <- "t0"
dim2dl$time[str_detect(dim2dl$par, "_f")] <- "t1"
dim2dl$time<- as.factor(dim2dl$time)

dim2dl$p <- str_sub(dim2dl$par, start = 1, end = 1)
unique(dim2dl$p)
dim2dl$par<- as.factor(dim2dl$par)
dim2dl$p[dim2dl$p == "m"] <- "V"
dim2dl$p <- as.factor(dim2dl$p)
str(dim2dl)
str(dim2d)

# get log ratios 
dim2d$lrD <- log10(dim2d$D_f/dim2d$D_i)
dim2d$lrd <- log10(dim2d$d_f/dim2d$d_i)
dim2d$lrL <- log10(dim2d$L_f/dim2d$L_i)
dim2d$lrV <- log10(dim2d$mlVol_f/dim2d$mlVol_i)
dim2d$lrW <- log10(dim2d$Weight_f/dim2d$Weight_i)

# Create explicit nested factors
dim2d <- within(dim2d, sourceColSpec <- factor(Origin:Species))
dim2d <- within(dim2d, sourceGen <- factor(sourceColSpec:Genotype))
dim2d <- within(dim2d, OrDest <- factor(Origin:Destination))
dim2d$Genus <- as.factor(str_sub(dim2d$Species, start = 1, end = 1))

str(dim2d)

### dim2dPA - add planar indexes #####

dim(dim2d)

PAs <- read.csv("output/PAs.csv", h = TRUE)
PA0<- PAs[PAs$time == "t0",]
PA1<- PAs[PAs$time == "t1",]
summary(PA0)
summary(PA1)
dim(PA0)
dim(PA1)
colnames(PA0)[5:13] <- c("Ci0", "FD0", "RI0", "area0","peri0","center_x0","center_y0","rC0","pC0")
#colnames(PA0)[4:12] <- c("Ci0", "FD0", "RI0", "area0","peri0","center_x0","center_y0","rC0","pC0")
PA0$CoralID<- as.factor(PA0$CoralID)
colnames(PA1)[5:13] <- c("Ci1", "FD1", "RI1", "area1","peri1","center_x1","center_y1","rC1","pC1")
#colnames(PA1)[4:12] <- c("Ci1", "FD1", "RI1", "area1","peri1","center_x1","center_y1","rC1","pC1")
PA1$CoralID<- as.factor(PA1$CoralID)

dim2dP <- merge(dim2d,PA0, by="CoralID",keep.all=TRUE)
dim(dim2dP)

dim2dPA <- merge(dim2dP,PA1, by="CoralID",keep.all=TRUE)
dim(dim2dPA)

dim2dPA$TH0 <- dim2dPA$rC0/((dim2dPA$D_i+dim2dPA$d_i)/2)
dim2dPA$TH1 <- dim2dPA$rC1/((dim2dPA$D_f+dim2dPA$d_f)/2)

str(dim2dPA)
dim2dPA <- within(dim2dPA, sourceColSpec <- factor(Origin:Species))
dim2dPA <- within(dim2dPA, sourceGen <- factor(sourceColSpec:Genotype))
dim2dPA <- within(dim2dPA, OrDest <- factor(Origin:Destination))
dim2dPA$Genus <- as.factor(str_sub(dim2dPA$Species, start = 1, end = 1))

str(dim2dPA)

# get log ratios
dim2dPA$lrA <- log10(dim2dPA$area1/dim2dPA$area0)
hist(dim2dPA$lrA)
plot(dim2dPA$lrA~dim2dPA$Species)
dim2dPA$lrR <- log10(dim2dPA$RI1/dim2dPA$RI0)
hist(dim2dPA$lrR)
dim2dPA$lrC <- log10(dim2dPA$Ci1/dim2dPA$Ci0)
hist(dim2dPA$lrC)
dim2dPA$lrF <- log10(dim2dPA$FD1/dim2dPA$FD0)
hist(dim2dPA$lrF)
dim2dPA$lrT <- log10(dim2dPA$TH1/dim2dPA$TH0)
hist(dim2dPA$lrT)
dim2dPA$lrTl<-dim2dPA$lrT
plot(dim2dPA$lrTl~dim2dPA$Species)

#write data
#write.csv(dim2dPA, "output/dim2dPA.csv", h = TRUE)

#### PCA ####

#build trait dataframe for computing PCA
names(dim2d)

time0 <- cbind(dim2dPA[,c("CoralID","Genus","Species", "Origin", "sourceGen",
                          "Destination","Rack", "D_i", "d_i", "L_i", "Weight_i", "mlVol_i",
                          "Ci0","FD0","RI0","area0", "peri0","TH0")], time = "t0")

time1 <- cbind(dim2dPA[,c("CoralID","Genus","Species", "Origin", "sourceGen",
                          "Destination","Rack", "D_f", "d_f", "L_f", "Weight_f", "mlVol_f",
                          "Ci1","FD1","RI1","area1", "peri1", "TH1")], time = "t1")
names(time0) <- c("CoralID","Genus","Species","Origin","sourceGen","Destination","Rack","D",
                  "d","L","Weight","mlVol",
                  "C", "DF", "RI", "area","peri", "TH","time")

names(time1) <- c("CoralID","Genus","Species","Origin","sourceGen","Destination","Rack","D",
                  "d","L","Weight","mlVol",
                  "C", "DF", "RI", "area","peri", "TH","time")

trait <- rbind(time0, time1)
trait$time <- as.factor(trait$time)
str(trait)

# transform 0-bounded variables

trait$Dl <- log10(trait$D)
trait$dl <- log10(trait$d)
trait$Ll <- log10(trait$L)
trait$Wl <- log10(trait$Weight)
trait$Vl <- log10(trait$mlVol)
trait$Al <- log10(trait$area)
trait$Fl <- log10(trait$DF)

#transform other variables
trait$Cl <- log10(trait$C)
trait$Tl <- log10(trait$TH)
trait$Rl <- log10(trait$RI)

# write data
# write.csv(trait, "output/trait.csv")

### pair plot - Fig SM4

str(trait)
dim(trait)
# ggcorr(trait[,20:29], palette = "RdYlGn", name = "rho", 
#        label = FALSE, label_color = "black")
columns <- 1:ncol(trait[,20:29])
pairs <- ggpairs(trait[,20:29], title = "",  
                 axisLabels = "show", columnLabels = colnames(trait[,20:29][, columns]))

ggsave(pairs, paste0("D:/Dropbox/My Dropbox/NC-RR_environment_data/outputs/pairs.png"),  
       width = 240, height = 190, units = "mm")
paste(pairs)
dev.off()


qfsE <- quality_funct_space(trait[,20:29], traits_weights=NULL, metric="Euclidean",nbdim = 10, dendro=FALSE, plot="qfs_Euclideanlr") 
#qfsE2 <- quality_funct_space(trait[,c("Fl","Rl","Ll")], traits_weights=NULL, metric="Euclidean", nbdim = 3,dendro=FALSE, plot="qfs_Euclidean2") 

round( qfsE$meanSD , 4)
fd.coord <- qfsE$details_funct_space$mat_coord[,1:4]
#fd.coord2 <- qfsE2$details_funct_space$mat_coord[,1:3]

gower<-qfsE$details_funct_space$mat_dissim
#gower2<-qfsE2$details_funct_space$mat_dissim

fit <- cmdscale(gower,eig=TRUE, k=2) # PCoA
#fit2 <- cmdscale(gower2,eig=TRUE, k=2) # PCoA
# variance explained by the axes
cumsum(fit$eig[fit$eig>=0]) / sum(fit$eig[fit$eig>0])

(cumsum(fit$eig[fit$eig>=0]) / sum(fit$eig[fit$eig>0]))[2] # the first 2 axis explain 84.5% of the variance
#(cumsum(fit2$eig[fit2$eig>=0]) / sum(fit2$eig[fit2$eig>0]))[2] 
(cumsum(fit$eig[fit$eig>=0]) / sum(fit$eig[fit$eig>0]))[1]

m <- fd.coord[rownames(fd.coord),]
tr <-tri.mesh(fd.coord[,1],fd.coord[,2])
ch <- convex.hull(tr)

tr0 <-tri.mesh(fd.coord[1:217,1],fd.coord[1:217,2])
ch0 <- convex.hull(tr0)

tr1 <-tri.mesh(fd.coord[218:434,1],fd.coord[218:434,2])
ch1 <- convex.hull(tr1)

trdS <-tri.mesh(fd.coord[trait$Destination == "S",1],fd.coord[trait$Destination == "S",2])
chdS <- convex.hull(trdS)

trdD <-tri.mesh(fd.coord[trait$Destination == "D",1],fd.coord[trait$Destination == "D",2])
chdD <- convex.hull(trdD)

troS <-tri.mesh(fd.coord[trait$Origin == "S",1],fd.coord[trait$Origin == "S",2])
choS <- convex.hull(troS)

troD <-tri.mesh(fd.coord[trait$Origin == "D",1],fd.coord[trait$Origin == "D",2])
choD <- convex.hull(troD)

n_axes = 4

fit <- cmdscale(gower,eig=TRUE, k=2) # PCoA

efit <- envfit(fit, trait["Cl"], na.rm=TRUE) 
efit <- envfit(fit, trait["Vl"], na.rm=TRUE) 


plot(fd.coord[,1], fd.coord[,2], xlab="PCoA1",main = "Trait space", ylab="PCoA2", type="n")
polygon(ch, col="grey", border=FALSE)
polygon(ch1, col =NA)
polygon(ch0, col =NA)
points(fd.coord[,1], fd.coord[,2], col = trait$Species)
#plot(fd.coord[1:217,1], fd.coord[1:217,2], col = trait$Species)
points(fd.coord[218:434,1], fd.coord[218:434,2], col = trait$Species, pch = 20)
for(i in 1:217) {
  segments(x0 = fd.coord[i,1], y0 = fd.coord[i,2], 
           x1 = fd.coord[i+217,1], y1 = fd.coord[i+217,2], col = trait$Species[i])
  
}
legend(-9.5, 3, legend=c("AD", "AM", "PC", "PR"),
       col=c("black", "red", "green", "blue"), pch = 20)
legend(-7, 3, legend=c("t0", "t1"),
       pch = c(1,20))

# get lrPCA

PCAdf <- cbind(trait,fd.coord)

# get delta and direction 
for(i in 1:217){
  dim2dPA$deltaPCA[i] <- sqrt((PCAdf$PC1[i] - PCAdf$PC1[i+217])^2+(PCAdf$PC2[i] - PCAdf$PC2[i+217])^2)
  dim2dPA$deltaPCA3[i] <- sqrt((PCAdf$PC1[i] - PCAdf$PC1[i+217])^2+(PCAdf$PC2[i] - PCAdf$PC2[i+217])^2 +
                                 (PCAdf$PC3[i] - PCAdf$PC3[i+217])^2)
  dim2dPA$dirtan[i] <- (PCAdf$PC2[i+217] - PCAdf$PC2[i])/(PCAdf$PC1[i+217] - PCAdf$PC1[i])
  dim2dPA$dirPCA[i] <- atan((PCAdf$PC2[i+217] - PCAdf$PC2[i])/(PCAdf$PC1[i+217] - PCAdf$PC1[i]))
  
}
hist(dim2dPA$deltaPCA)
hist(dim2dPA$dirPCA*180/pi)
plot(dim2dPA$dirPCA~dim2dPA$Species)

hist(dim2dPA$dirtan)
dim2dPA$dirtan[dim2dPA$dirtan>1]
hist(log10(dim2dPA$deltaPCA))
dim2dPA$dirtan[120]
dim2dPA[120,]
PCAdf[120+217,]
PCAdf[120,]
dim2dPA$deltaPCAl <- log10(dim2dPA$deltaPCA)
(-0.407345 -(-1.219634))/(-1.397566+1.45699)
atan(15)

hist(dim2dPA$deltaPCAl)
hist(dim2dPA$deltaPCA)

# overwrite df to include pca data 
# write.csv(dim2dPA, "output/dim2dPA.csv")
