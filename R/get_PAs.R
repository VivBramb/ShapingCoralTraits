####  get PA ####

library(pracma)
library(fractaldim)
library(sp)

#read file directory
txt_files <- dir("data/nubbin_outlines")
PAs <- data.frame()

#loop into files
for (file in txt_files) {
  
outline <- read.delim(paste0("data/nubbin_outlines/", file), head = FALSE)
colnames(outline) <- c("x","y")
#plot(outline, type = "n", xlab = "x", ylab = "y")
#polygon(outline, col = "gray", border = "red")

area <- abs(polyarea(outline$x,outline$y))
peri <- poly_length(outline$x,outline$y)
center_x <- mean(outline$x)
center_y <- mean(outline$y)
rC <- sqrt(area/pi)
pC <- 2*pi*rC
nubbinID <- str_sub(file,1,13)
time <-str_sub(file,15,15)

C <- (4 * pi * area)/(peri^ 2)
D <-fd.estim.boxcount(cbind(outline[,1],outline[,2]), nlags = "auto")$fd
R <- peri/pC
PAs <- rbind(PAs, data.frame(file, CoralID = nubbinID, time, C,D, R,
                             area,peri, center_x, center_y, rC, pC))
}

summary(PAs)

# add time
PAs$time[PAs$time == 1] <- "t1"
PAs$time[PAs$time == 0] <- "t0"
str(PAs)

PAs$time <- as.factor(PAs$time)

# sanity checks
plot(PAs$R~as.factor(substr(PAs$CoralID,1,1)))
plot(PAs$D~as.factor(substr(PAs$CoralID,1,1)))
plot(PAs$C~as.factor(substr(PAs$CoralID,1,1)))

plot(PAs$R~PAs$time)
plot(PAs$D~PAs$time)
plot(PAs$C~PAs$time)

#write.csv(PAs,"output/PAs.csv")

