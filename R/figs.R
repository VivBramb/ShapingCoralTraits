### morphospace figures ###
library(ggplot2)

###screeplot ####
PCA2 <- prcomp(trait[,20:29], center = T, scale. = T)
var_explained = PCA2$sdev^2 / sum(PCA2$sdev^2)
variance <- as.data.frame(cbind(PC = c(1:length(var_explained)),var_explained))


scree <- ggplot(data = variance, aes(x = factor(PC), y = var_explained, group = 1)) + 
  geom_point() + 
  geom_line()+
  xlab("\nPrincipal Component") + 
  ylab("Variance Explained\n") +
  ylim(0, 1)+
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
    legend.position = c(.98, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

png("figs/screeplot.png",width = 400, height = 200)
print(scree)
dev.off()

###biplot #####
Loadings <- as.data.frame(cor(trait[,20:29], PCA2$x)) #factor loadings
Arrows <- data.frame(xS = rep(0,nrow(Loadings)), #loading arrow x center
                     yS = rep(0,nrow(Loadings)), #loading arrow y center
                     xE = Loadings$PC1, #loading arrow x coordinate
                     yE = Loadings$PC2) #loading arrow y coordinate
Arrows <- Arrows *5 #scaling of arrows

Text <- data.frame(X = Arrows$xE *1.1, # x coordinate for variable labels, scaled away from arrow end
                   Y = Arrows$yE *1.1) # y coordinate for variable lables, scale away from arrow end
row.names(Text) <- c(colnames(trait[,20:29])) #variable labels
# manually dodge labels
Text[1,1] <- Text[1,1]*1.1
Text[2,1] <- Text[1,1]*0.5
Text[5,2] <- Text[5,2]+ 0.1
Text[4,1] <- Text[4,1]+ 0.1
Vars <- c("D", "d", "L", "W", "V", "A", "F", "C", "T", "R") #variable labels

PlotDat <- data.frame(PCA2$x,
                      Species = factor(trait$Species),
                      time = factor(trait$time),
                      Destination = factor(trait$Destination),
                      Origin = factor(trait$Origin)) #PCA data and growth form dataframe


Biplot <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,colour= Species,fill = Species)) + #ggplot set up
  geom_point(shape = 21, size = 3,alpha = 0.5) + #Scatter plot of observed PC1 and 2
  geom_segment(inherit.aes = FALSE,
               data = Arrows, #Arrows
               aes(x= xS,y=yS,xend=xE,yend=yE),
               colour = "grey21",
               size = 0.85,
               arrow = arrow(length = unit(0.25,"cm"))) +
  geom_text(inherit.aes = FALSE,
            data = as.data.frame(Vars), #Loading variable text labels
            aes(x = Text$X, y = Text$Y, label = Vars),
            size = 5,
            parse = TRUE) +
  #geom_text(aes(label = Labels), colour = "black") +
  scale_fill_viridis_d() + #set pallete colours
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  scale_color_viridis_d() + #set pallete colours
  #guides(colour = F, fill = guide_legend(nrow = 1)) + #remove colour guide, force fill legend to  single row
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN+
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
    legend.position = c(.98, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
Biplot
#save as png
png("figs/BiplotFinal.png", 500, 400, type='cairo')
Biplot
dev.off()


#### plot pca with time space ####

tr0 <-tri.mesh(PlotDat[1:217,1],PlotDat[1:217,2])
ch0 <- convex.hull(tr0)

tr1 <-tri.mesh(PlotDat[218:434,1],PlotDat[218:434,2])
ch1 <- convex.hull(tr1)

ch0d <- as.data.frame(cbind(PC1 = c(ch0$x), PC2 = c(ch0$y)))
ch1d <- as.data.frame(cbind(PC1 = c(ch1$x), PC2 = c(ch1$y)))

# get coords for text "time 0", "time 1"
TextT <- as.data.frame(cbind(c(-1.30,4),
                             c(0.5,-0.5)))
row.names(TextT) <- c("t0","t1") #variable labels
colnames(TextT) <- c("PC1","PC2") #variable labels
VarsT <- c("time0","time1") #variable labels


PCAtime <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,colour= Species,fill = Species)) + #ggplot set up
  geom_polygon(data = ch0d, aes(x =PC1,y=PC2),col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_polygon(data = ch1d, aes(x =PC1,y=PC2), col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_point(shape = 21, size = 2,alpha = 0.4) + #Scatter plot of observed PC1 and 2
  scale_fill_viridis_d() + #set pallete colours
  geom_text(data = TextT, #Loading variable text labels
            aes(x = PC1, y = PC2, label = VarsT),
            inherit.aes = FALSE,
            size = 5,
            parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  scale_color_viridis_d() + #set pallete colours
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN+
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
#save as png
png("figs/PCAtime.png", 600, 500, type='cairo')
PCAtime
dev.off()

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


segma <- PlotDat[1:217,c(1:2,11:14)]
segmb <- PlotDat[218:434,c(1:2)]
colnames(segmb) <- c("PC1f", "PC2f")
segm <- cbind(segma,segmb)

PCAsegm <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,fill = Species)) + #ggplot set up
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  geom_point(shape = 21, size = 2,alpha = 0.8, col = NA) + #Scatter plot of observed PC1 and 2
  geom_polygon(data = ch0d, aes(x =PC1,y=PC2),col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_polygon(data = ch1d, aes(x =PC1,y=PC2), col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_segment(data = segm, aes(x =PC1,y=PC2, xend = PC1f, yend = PC2f, col = Species), alpha = .5, size = 1.2)+
  # geom_polygon(data = chad, aes(x =PC1,y=PC2),col = NA,fill = "#440154FF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = cham, aes(x =PC1,y=PC2), col = NA,fill = "#31688EFF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = chpc, aes(x =PC1,y=PC2),col = NA,fill = "#35B779FF",
  #              inherit.aes = F,
  #              alpha = .3, show.legend = FALSE)+
  # geom_polygon(data = chpr, aes(x =PC1,y=PC2), col = NA,fill = "#FDE725FF",
  #              inherit.aes = F,
#              alpha = .3, show.legend = FALSE)+
scale_fill_viridis_d() + #set pallete colours
  scale_color_viridis_d() +  #set pallete colours
  #guides(colour = F, fill = guide_legend(nrow = 1)) + #remove colour guide, force fill legend to  single row
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN+
  facet_grid(Destination~Origin,
             labeller = labeller(Origin = Origin.labs, Destination = Destination.labs))+
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
    legend.position = "none",
    legend.margin = margin(6, 6, 6, 6)
  )

PCAsegm


png("figs/PCAsegmhor.png",
    width = 700, height = 300)
ggarrange(PCAtime, PCAsegm, ncol = 2, nrow = 1)
dev.off()



#### plot pca with Destination ####

trdS <-tri.mesh(PlotDat[trait$Destination == "S",1],PlotDat[trait$Destination == "S",2])
chdS <- convex.hull(trdS)
trdD <-tri.mesh(PlotDat[trait$Destination == "D",1],PlotDat[trait$Destination == "D",2])
chdD <- convex.hull(trdD)


chdSd <- as.data.frame(cbind(PC1 = c(chdS$x), PC2 = c(chdS$y)))
chdDd <- as.data.frame(cbind(PC1 = c(chdD$x), PC2 = c(chdD$y)))

#Biplot

# get coords for text "time 0", "time 1"
TextT <- as.data.frame(cbind(c(0,4.5),
                             c(0.5,-1.5)))
row.names(TextT) <- c("S","D") #variable labels
colnames(TextT) <- c("PC1","PC2") #variable labels
VarsT <- c("Shallow\nDestination","Deep\nDestination") #variable labels

PCAdest <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,colour= Species,fill = Species)) + #ggplot set up
  geom_point(shape = 21, size = 2,alpha = 0.4) + #Scatter plot of observed PC1 and 2
  scale_fill_viridis_d() + #set pallete colours
  geom_polygon(data = chdDd, aes(x =PC1,y=PC2),col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_polygon(data = chdSd, aes(x =PC1,y=PC2), col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_text(data = TextT, #Loading variable text labels
            aes(x = PC1, y = PC2, label = VarsT),
            inherit.aes = FALSE,
            size = 5,
            parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  #lapply(1:length(MaxPng), function(p) annotation_custom(MaxPng[[p]],PngXMin[p],PngXMax[p],PngYMin[p],PngYMax[p])) + #add maximum variable specimens
  #lapply(1:length(MinPng), function(p) annotation_custom(MinPng[[p]],-PngXMin[p],-PngXMax[p],-PngYMin[p],-PngYMax[p])) + #add minimum variable speciemens
  scale_color_viridis_d() + #set pallete colours
  #guides(colour = F, fill = guide_legend(nrow = 1)) + #remove colour guide, force fill legend to  single row
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN+
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
PCAdest
#save as png
png("figs/PCADest.png", 600, 500, type='cairo')
PCAdest
dev.off()


#### plot pca with origin ####


troS <-tri.mesh(PlotDat[trait$Origin == "S",1],PlotDat[trait$Origin == "S",2])
choS <- convex.hull(troS)
troD <-tri.mesh(PlotDat[trait$Origin == "D",1],PlotDat[trait$Origin == "D",2])
choD <- convex.hull(troD)


choSd <- as.data.frame(cbind(PC1 = c(choS$x), PC2 = c(choS$y)))
choDd <- as.data.frame(cbind(PC1 = c(choD$x), PC2 = c(choD$y)))

# get coords for text "time 0", "time 1"
TextT <- as.data.frame(cbind(c(1,7),
                             c(-0.5,-2.5)))
row.names(TextT) <- c("S","D") #variable labels
colnames(TextT) <- c("PC1","PC2") #variable labels
VarsT <- c("Shallow","Deep") #variable labels

PCAori <- ggplot(data = PlotDat, aes(x =PC1,y=PC2,colour= Species,fill = Species)) + #ggplot set up
  geom_point(shape = 21, size = 2,alpha = 0.4) + #Scatter plot of observed PC1 and 2
  scale_fill_viridis_d() + #set pallete colours
  geom_polygon(data = choDd, aes(x =PC1,y=PC2),col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_polygon(data = choSd, aes(x =PC1,y=PC2), col = "black",fill = "grey",
               inherit.aes = FALSE,
               alpha = .3, show.legend = FALSE)+
  geom_text(data = TextT, #Loading variable text labels
            aes(x = PC1, y = PC2, label = VarsT),
            inherit.aes = FALSE,
            size = 5,
            parse = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey31") + #horizontal reference line
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey31") + #verticalreference line
  scale_color_viridis_d() + #set pallete colours
  theme_bw() + #remove panel grid lines
  coord_cartesian(xlim = c(-5,11), ylim = c(-5,5)) +
  themeRN+
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
PCAori
#save as png
png("figs/PCAori.png", 600, 500, type='cairo')
PCAori
dev.off()
