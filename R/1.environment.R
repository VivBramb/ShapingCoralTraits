#### Environmental variables ####
library(stringr)
library(ggplot2)
library(tidyverse)
library(lubridate)

#### themes ####

themeRN <-theme(text = element_text(size = 12),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(vjust = 0.5),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 14),
                panel.grid = element_blank())
themeRN_noLegend <-theme(text = element_text(size = 12),
                         axis.title.x = element_text(size = 14),
                         axis.title.y = element_text(size = 14),
                         axis.text = element_text(size = 12),
                         axis.text.x = element_text(vjust = 0.5),
                         legend.position = "none",
                         panel.grid = element_blank(),
                         strip.background = element_rect(fill = "gray40"),
                         strip.text = element_text(size = 13, colour = "white"))
#### temperature #####

store_t_all <- data.frame()

files <- dir("data/env/temp/csv")

# read hobos looping in folders
for (file in files) {
  
  HB <- read.csv(paste0("data/env/temp/csv/", file), skip = 1)
  HB <- HB[,1:4]
  HB[,2] <- as.POSIXct(as.character(HB[,2]), 
                       format = "%m/%d/%y %I:%M:%S %p")
  HB$site <- str_sub(file,3,3)
  HB$rack <- str_sub(file,4,4)
  HB$rep <- str_sub(file,5,5)
  colnames(HB) <- c("x","datetime","temp","light","site","rack","rep")
  store_t_all <- rbind(store_t_all, HB)
}

FtoC <- function (x) {
  y <- (x - 32) * 5/9
  return(y)}

indexF <- store_t_all$temp> 45
store_t_all$temp[indexF] <- sapply(store_t_all$temp[indexF],FtoC) 

HB$site <- as.factor(HB$site)
HB$rack <- as.factor(HB$rack)
HB$rep <- as.factor(HB$rep)

rep.labs <- c("Mar 2017", "Dec-Jan 17-18", "April 2018")
names(rep.labs) <- c("a", "b","c")

store_t_all <- data.frame((store_t_all)%>%
                   group_by(rack,rep) %>%
                   mutate(mean_temp = mean(temp)))

summary(lm(store_t_all$mean_temp~store_t_all$site))
t_means <-  data.frame((store_t_all)%>%
  select(mean_temp,site,rack,rep) %>%
  distinct())
str(store_t_all)
summary(store_t_all)
summary(aov(t_means$mean_temp~t_means$site))

ggplot(data = store_t_all, aes(x = site, y = temp, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "temperature (\u00B0C)")+
  facet_wrap(.~rep, scales = "free_x", labeller = labeller(rep = rep.labs))+
  themeRN

ggplot(data = store_t_all, aes(x = site, y = temp, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "temperature (\u00B0C)")+
  themeRN


ggplot(data = store_t_all, aes(x = rack, y = temp, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "temperature (\u00B0C)")+
  themeRN+
  facet_wrap(.~rep, scales = "free_x", labeller = labeller(rep = rep.labs))

##### light #####
store_l_all <- data.frame()

files <- dir("data/env/light/csv")
files

# read hobos looping in folders
for (file in files) {
  
  HB <- read.csv(paste0("data/env/light/csv/", file), skip = 1)
  HB <- HB[,1:4]
  HB$site <- str_sub(file,3,3)
  HB$rack <- str_sub(file,4,4)
  HB$rep <- str_sub(file,5,5)
  colnames(HB) <- c("x","datetime","temp","light","site","rack","rep")
  if(HB$rep[1] =='a') {HB$light <- 10.7639104167097*HB$light}
  store_l_all <- rbind(store_l_all, HB)
}

HB$site <- as.factor(HB$site)
HB$rack <- as.factor(HB$rack)
HB$rep <- as.factor(HB$rep)

store_l_all <- data.frame((store_l_all)%>%
                            group_by(rack,rep) %>%
                            mutate(mean_light = mean(light)))



l_means <-  data.frame((store_l_all)%>%
                         select(mean_light,site,rack,rep) %>%
                         distinct())
str(store_l_all)
summary(store_l_all)
summary(aov(l_means$mean_light~l_means$site))
l_means%>% group_by(site) %>% summarise(mean = mean(mean_light))
2961/1329

ggplot(data = store_l_all, aes(x = site,y = light)) +
  geom_boxplot(shape = 20)+
  geom_smooth()+
  theme_minimal()+
  facet_wrap(.~rep, scales = "free_x")

rep.labs <- c("Feb 2017", "Apr 2017", "March 2018")
names(rep.labs) <- c("a", "b","c")

ggplot(data =  store_l_all, aes(x = site, y = light, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "light (lux)")+
  themeRN+
  facet_wrap(.~rep, scales = "free_x", labeller = labeller(rep = rep.labs))

ggplot(data =  store_l_all[store_l_all$light>0,], aes(x = site, y = light, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  ylim(c(0,2000))+
  theme_bw()+
  labs(y = "light (lux)")+
  themeRN


ggplot(data = store_l_all[store_l_all$light>0,], aes(x = rack, y = light, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "light (lux)")+
  themeRN+
  facet_wrap(.~rep, scales = "free_x", labeller = labeller(rep = rep.labs))


summary(lm(store_l_all$light~store_l_all$site))


#### sediment ###
sed <- read.csv("data/env/Sediment.csv", h = T)
sed
summary(aov(sed$W~sed$Site))
A=pi*9
t.test(sed$W[1:6],sed$W[7:12])
ggplot(data =  sed, aes(x = Site, y = W*1000/A, fill = Site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "sediment (g/m/day)")+
  themeRN


####  flow ####
flow <- read.csv("data/env/Gypsum_balls.csv", h = T)
flow
summary(lm(flow$W1.W2~flow$site))
summary(aov(flow$W1.W2~flow$site))

t.test(flow$W1.W2[1:6],flow$W1.W2[7:12])
sd(flow$W1.W2[7:12])
sd(flow$W1.W2[1:6])
ggplot(data =  flow, aes(x = site, y = W1.W2, fill = site)) +
  geom_boxplot(key_glyph = "rect")+
  scale_fill_viridis_d(option = "cividis")+
  scale_colour_viridis_d(option = "cividis")+  
  geom_smooth()+
  theme_bw()+
  labs(y = "weight loss (g)")+
  themeRN

