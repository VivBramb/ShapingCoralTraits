### response to reviewer 3 ####

### upload libraries

library(tidyverse)
library(tripack)

### upload themes

themeRN <-theme(text = element_text(size = 12),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(vjust = 0.5),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 14),
                panel.grid = element_blank())


### get a subset of nubbins that reached similar PAs ####

df <- read.csv("output/dim2dPA.csv")
head(df)
range(df$area0)
range(df$area1)
hist(df$area0)
hist(df$area1)

min <- min(df$area1)
max <- max(df$area0)

df_sub <- as.data.frame( df %>%
                      filter(area0 > min) %>%
                      filter(area1 < max))

table(df_sub$Destination)
table(df_sub$Origin)

## small nubbin subset 

time0 <- cbind(df_sub[,c("CoralID","Genus","Species", "Origin", "sourceGen",
                          "Destination","Rack", "D_i", "d_i", "L_i", "Weight_i", "mlVol_i",
                          "Ci0","FD0","RI0","area0", "peri0","TH0")], time = "t0")

time1 <- cbind(df_sub[,c("CoralID","Genus","Species", "Origin", "sourceGen",
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


## build trait space ####

qfsE_sub <- quality_funct_space(trait[,20:29], traits_weights=NULL, metric="Euclidean",nbdim = 10, dendro=FALSE, plot="qfs_Euclidean_sub") 
#qfsE2 <- quality_funct_space(trait[,c("Fl","Rl","Ll")], traits_weights=NULL, metric="Euclidean", nbdim = 3,dendro=FALSE, plot="qfs_Euclidean2") 

round( qfsE_sub$meanSD , 4)
fd.coord <- qfsE_sub$details_funct_space$mat_coord[,1:4]


m <- fd.coord[rownames(fd.coord),]
tr <-tri.mesh(fd.coord[,1],fd.coord[,2])
ch <- convex.hull(tr)

tr0 <-tri.mesh(fd.coord[1:41,1],fd.coord[1:41,2])
ch0 <- convex.hull(tr0)

tr1 <-tri.mesh(fd.coord[42:82,1],fd.coord[42:82,2])
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

plot(fd.coord[,1], fd.coord[,2], xlab="PCoA1",main = "Trait space", ylab="PCoA2", type="n")
polygon(ch, col="grey", border=FALSE)
polygon(ch1, col =NA)
polygon(ch0, col =NA)
points(fd.coord[,1], fd.coord[,2], col = as.factor(trait$Species))
#plot(fd.coord[1:217,1], fd.coord[1:217,2], col = trait$Species)
points(fd.coord[42:82,1], fd.coord[42:82,2], col =  as.factor(trait$Species), pch = 20)
for(i in 1:41) {
  segments(x0 = fd.coord[i,1], y0 = fd.coord[i,2], 
           x1 = fd.coord[i+41,1], y1 = fd.coord[i+41,2], col = as.factor(trait$Species))
  
}
legend(2, 4, legend=c("AD", "AM", "PC", "PR"),
       col=c("black", "red", "green", "blue"), pch = 20)
legend(3, 4, legend=c("t0", "t1"),
       pch = c(1,20))

## make a better plot ####

PCA2 <- prcomp(trait[,20:29], center = T, scale = T)

PlotDat <- data.frame(PCA2$x,
                      Species = factor(trait$Species),
                      time = factor(trait$time),
                      Destination = factor(trait$Destination),
                      Origin = factor(trait$Origin)) #PCA data and growth form dataframe


tr0 <-tri.mesh(PlotDat[1:41,1],PlotDat[1:41,2])
ch0 <- convex.hull(tr0)

tr1 <-tri.mesh(PlotDat[42:82,1],PlotDat[42:82,2])
ch1 <- convex.hull(tr1)

ch0d <- as.data.frame(cbind(PC1 = c(ch0$x), PC2 = c(ch0$y)))
ch1d <- as.data.frame(cbind(PC1 = c(ch1$x), PC2 = c(ch1$y)))



PCAtime <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,colour= Species,fill = Species)) + #ggplot set up
  geom_polygon(data = ch0d, aes(x =PC1,y=PC2),col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_polygon(data = ch1d, aes(x =PC1,y=PC2), col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_point(shape = 21, size = 2,alpha = 0.4) + #Scatter plot of observed PC1 and 2
  scale_fill_viridis_d() + #set pallete colours
  # geom_text(data = TextT, #Loading variable text labels
  #           aes(x = PC1, y = PC2, label = VarsT),
  #           inherit.aes = FALSE,
  #           size = 5,
  #           parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  scale_color_viridis_d() + #set pallete colours
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,5), ylim = c(-3.2,5)) +
  theme_minimal()+
  theme(
    text = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(vjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "gray40"),
    strip.text = element_text(size = 12, colour = "white"),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

PCAtime


#### plot PCA with distance traveled #### 
Origin.labs <- c("Deep Origin", "Shallow Origin")
names(Origin.labs) <- c("D", "S")

Destination.labs <- c("Deep Destination", "Shallow Destination")
names(Destination.labs) <- c("D", "S")

cols <- c("D" = "black", "S" = "grey50")


trad <-tri.mesh(PlotDat[PlotDat$Species == "AD",1],PlotDat[PlotDat$Species == "AD",2])
chad <- convex.hull(trad)
chad <- as.data.frame(cbind(PC1 = c(chad$x), PC2 = c(chad$y)))
tram <-tri.mesh(PlotDat[PlotDat$Species == "AM",1],PlotDat[PlotDat$Species == "AM",2])
cham <- convex.hull(tram)
cham <- as.data.frame(cbind(PC1 = c(cham$x), PC2 = c(cham$y)))
trpc <-tri.mesh(PlotDat[PlotDat$Species == "PC",1],PlotDat[PlotDat$Species == "PC",2])
chpc <- convex.hull(trpc)
chpc <- as.data.frame(cbind(PC1 = c(chpc$x), PC2 = c(chpc$y)))
trpr <-tri.mesh(PlotDat[PlotDat$Species == "PR",1],PlotDat[PlotDat$Species == "PR",2])
chpr <- convex.hull(trpr)
chpr <- as.data.frame(cbind(PC1 = c(chpr$x), PC2 = c(chpr$y)))


segma <- PlotDat[1:41,c(1:2,11:14)]
segmb <- PlotDat[42:82,c(1:2)]
colnames(segmb) <- c("PC1f", "PC2f")
segm <- cbind(segma,segmb)


### SM8 ####
PCAsegm <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,col = Species)) + #ggplot set up
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  # geom_point(size = 2, alpha = 0.8, aes(col = Species)) + #Scatter plot of observed PC1 and 2
  # geom_polygon(data = ch0d, aes(x =PC1,y=PC2),col = "black",fill = "grey",
  #              inherit.aes = FALSE,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = ch1d, aes(x =PC1,y=PC2), col = "black",fill = "grey",
  #              inherit.aes = FALSE,
  #              alpha = .3, show.legend = FALSE)+
  geom_segment(data = segm, size = 1.5, aes(x =PC1,y=PC2, xend = PC1f, yend = PC2f, col = Species), 
               arrow = arrow(length = unit(0.03, "npc")),
               lineend = "round", linejoin = "round",
               alpha = .8)+
  # geom_polygon(data = chadd, aes(x =PC1,y=PC2),col = NA,fill = "#440154FF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = chamd, aes(x =PC1,y=PC2), col = NA,fill = "#31688EFF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = chpcd, aes(x =PC1,y=PC2),col = NA,fill = "#35B779FF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = chprd, aes(x =PC1,y=PC2), col = NA,fill = "#FDE725FF",
  #              inherit.aes = F,
#              alpha = .3, show.legend = FALSE)+
  #scale_fill_viridis_d() + #set pallete colours
  scale_color_viridis_d() +  #set pallete colours
  #guides(colour = F, fill = guide_legend(nrow = 1)) + #remove colour guide, force fill legend to  single row
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN +
  facet_grid(~ Destination,
             labeller = labeller(Destination = Destination.labs))+
  theme(
    text = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(vjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    #panel.grid = element_blank(),
    strip.background = element_rect(fill = "gray40"),
    strip.text = element_text(size = 12, colour = "white"),
    #legend.position = "none",
    #legend.margin = margin(6, 6, 6, 6)
  )

PCAsegm




