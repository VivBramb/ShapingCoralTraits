library(ggplot2)
library(tidyverse)
library(stringr)
library(lme4)
library(stargazer)
library(MuMIn)
library(GGally)
library(nlme)
library(ggpubr)
library(broom.mixed)

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

##functions ####

get_AICs <- function (data, response, random){ 
  m.1 <- lme(data = data, response ~ Origin*Destination*Genus,
             random = random)
  m.2 <- lme(data = data, response ~ Origin*Destination+
               Origin*Genus + Genus*Destination,
             random = random)
  m.3 <- lme(data = data, response ~ Origin*Destination+ Origin*Genus,
             random = random)
  m.4 <- lme(data = data, response ~ Origin*Destination+ Genus*Destination,
             random = random)
  m.5 <- lme(data = data, response ~ Origin*Genus + Genus*Destination,
             random = random)
  m.6 <- lme(data = data, response ~ Origin*Destination+ Genus,
             random = random)
  m.7 <- lme(data = data, response ~ Genus*Destination +Origin,
             random = random)
  m.8 <- lme(data = data, response ~ Origin*Genus + Destination,
             random = random)
  m.9 <- lme(data = data, response ~ Origin*Destination,
             random = random)
  m.10 <- lme(data = data, response ~ Genus*Destination,
              random = random)
  m.11 <- lme(data = data, response ~ Origin*Genus,
              random = random)
  m.12 <- lme(data = data, response ~ Origin + Genus + Destination,
              random = random)
  m.13 <- lme(data = data, response ~ Origin + Genus,
              random = random)
  m.14 <- lme(data = data, response ~ Genus + Destination,
              random = random)
  m.15 <- lme(data = data, response ~ Origin + Destination,
              random = random)
  m.16 <- lme(data = data, response ~ Origin,
              random = random)
  m.17 <- lme(data = data, response ~ Genus ,
              random = random)
  m.18 <- lme(data = data, response ~Destination,
              random = random)
  return(list(AICs = AIC(m.1,m.2,m.3,m.4,m.5,m.6,
                         m.7,m.8,m.9,m.10,m.11,m.12,
                         m.13,m.14,m.15,m.16,m.17,m.18), m7 = m.7, m10 = m.10,m15 = m.15, m17 = m.17))
}

get_AICs_mortality <- function (data){ 
  m.1 <- glmer(data = data, status ~ Origin*Destination*Genus +(1|sourceGen) + (1|Rack),
               start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)), family = binomial)
  m.2 <- glmer(data = data, status ~ Origin*Destination+
                 Origin*Genus + Genus*Destination +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.3 <- glmer(data = data, status ~ Origin*Destination+ Origin*Genus +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.4 <- glmer(data = data, status ~ Origin*Destination+ Genus*Destination +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.5 <- glmer(data = data, status ~ Origin*Genus + Genus*Destination +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.6 <- glmer(data = data, status ~ Origin*Destination+ Genus +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.7 <- glmer(data = data, status ~ Genus*Destination +Origin +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.8 <- glmer(data = data, status ~ Origin*Genus + Destination +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.9 <- glmer(data = data, status ~ Origin*Destination +(1|sourceGen) + (1|Rack),
               family = binomial)
  m.10 <- glmer(data = data, status ~ Genus*Destination +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.11 <- glmer(data = data, status ~ Origin*Genus +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.12 <- glmer(data = data, status ~ Origin + Genus + Destination +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.13 <- glmer(data = data, status ~ Origin + Genus +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.14 <- glmer(data = data, status ~ Genus + Destination +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.15 <- glmer(data = data, status ~ Origin + Destination +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.16 <- glmer(data = data, status ~ Origin +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.17 <- glmer(data = data, status ~ Genus  +(1|sourceGen) + (1|Rack),
                family = binomial)
  m.18 <- glmer(data = data, status ~ Destination +(1|sourceGen) + (1|Rack),
                family = binomial)
  return(list(AICs = AIC(m.1,m.2,m.3,m.4,m.5,m.6,
                         m.7,m.8,m.9,m.10,m.11,m.12,
                         m.13,m.14,m.15,m.16,m.17,m.18), m7 = m.7, m10 = m.10,m17 = m.17))
}
#### mortality model selection #### 

#read.csv("output/mort.csv", head = TRUE)
mort$status <- NA
mort$status[is.na(mort$L_f)] <- 0
mort$status[!is.na(mort$L_f)] <- 1
#View(mort)
str(mort)
mort$Genus <- as.factor(str_sub(mort$Species, start = 1, end = 1))
mort <- within(mort, sourceColSpec <- factor(Origin:Species))
mort <- within(mort, sourceGen <- factor(sourceColSpec:Genotype))
mort <- within(mort, OrDest <- factor(Origin:Destination))
mort$Genus <- as.factor(str_sub(mort$Species, start = 1, end = 1))
str(mort)

# step 2
mlm <- glm(data = mort, status ~ Origin*Destination*Genus, family = binomial(), maxit = 100)
mlme1 <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                 (1|Species) + (1|Rack) +(1|sourceGen),
               family = binomial)
tt <- getME(mlme1,"theta")
ll <- getME(mlme1,"lower")
min(tt[ll==0])
ss <- getME(mlme1,c("theta","fixef"))
mlme1u <- update(mlme1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
mlme1 <- mlme1u


mlme2a <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|Species) + (1|Rack),
                family = binomial)
tt <- getME(mlme2a,"theta")
ll <- getME(mlme2a,"lower")
min(tt[ll==0])

ss <- getME(mlme2a,c("theta","fixef"))
mlme2au <- update(mlme2a,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

mlme2b <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|Species) + (1|sourceGen),
                family = binomial)
ss <- getME(mlme2b,c("theta","fixef"))
mlme2bu <- update(mlme2b,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

mlme2c <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|sourceGen) + (1|Rack),
                family = binomial)
ss <- getME(mlme2c,c("theta","fixef"))
mlme2cu <- update(mlme2c,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

mlme3a <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|Species),
                family = binomial)
mlme3b <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|Rack),
                family = binomial)
mlme3c <- glmer(data = mort, status ~ Origin*Destination*Genus + 
                  (1|sourceGen),
                family = binomial)
tt <- getME(mlme1,"theta")
ll <- getME(mlme1,"lower")
min(tt[ll==0])


AIC(mlm,mlme1,mlme2a,mlme2b,mlme2c, mlme3a, mlme3b, mlme3c) # nlm2b doesn't have singularity problem

#step3
mlme2b<-mlme2c


m <- get_AICs_mortality(mort)
m$AICs[m$AICs$AIC == min(m$AICs$AIC), ]
mlmes <- m$m7
summary(mlmes)
summary(mlme1)

plot(mlmes)
plot(mlme1)


mortality_full <- rbind(cbind(var = "mortality", 
                              as.data.frame( tidy(mlme1,conf.int=TRUE,effects="fixed"))[,-1]))
mortality_sel <- rbind(cbind(var = "mortality", 
                              as.data.frame( tidy(mlmes,conf.int=TRUE,effects="fixed"))[,-1]))
mortality_sel$Term_labels <- c("Intercept", "Genus(P)", "Destination(S)", "Origin(S)", "Genus(P):Destination(S)") 

order <- c("OriginS"="b", "DestinationS"="c", "GenusP"="d", 
           "OriginS:DestinationS"="e", "OriginS:GenusP"="f",
           "DestinationS:GenusP" = "g", "GenusP:DestinationS" = "g",
           "OriginS:DestinationS:GenusP" = "h",
           "(Intercept)" = "a")
mortality_full$Term_order <- order[mortality_full$term]
mortality_sel$Term_order <- order[mortality_sel$term]
labs <- list('b'="Origin(S)", 'c'="Destination(S)", 'd'="Genus(P)", 
             'e'="Origin(S):Destination(S)", 'f'="Origin(S):Genus(P)",
             'g' = "Destination(S):Genus(P)",'g' = "Genus(P):Destination(S)",
             'h' = "Origin(S):Destination(S):Genus(P)",
             'a'= "Intercept")

mortality_full$Term_labels <- as.character(pairs[mortality_full$Term_order])

mortality_full$sig <- 0
mortality_full$sig[mortality_full$p.value <= 0.05] <- 1
mortality_full$sigcor <- 0
mortality_full$sigcor[mortality_full$p.value <= 0.05/8] <- 1

mortality_sel$sig <- 0
mortality_sel$sig[mortality_sel$p.value <= 0.05] <- 1


ggplot()+
  geom_linerange(data=mortality_full,
                 mapping=aes(y=reorder(term,desc(Term_order)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=mortality_full,
             mapping=aes(y=term, x=estimate, col = factor(sigcor)), size = 4)+
  xlab("Estimates with 95% confidence intervals") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "Response variable")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 12), axis.title = element_text(size = 15))+
  themeRNf

ggplot()+
  geom_linerange(data=mortality_sel,
                 mapping=aes(y=reorder(Term_labels,desc(Term_order)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=mortality_sel,
             mapping=aes(y=Term_labels, x=estimate, col = factor(sig)), size = 4)+
  xlab("Estimates with 95% CI") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 18), 
        panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 18), axis.title = element_text(size = 18),axis.title.x = element_blank())+
  themeRNf
### fig2b
ggsave("figs/mortality_sel_estim.png", width =11, height = 6, units ="cm",  dpi = 300)

mort_prob <- cbind(mlmes@frame, mlmes@resp$eta)
genus <- c("A"="Acropora sp.", "P"="Porites sp.")
mort_prob$Genus_long<- genus[mort_prob$Genus]
dest <- c("S"="Shallow", "D"="Deep")
mort_prob$Destination<- dest[mort_prob$Destination]
cols <- c("Shallow" = "dodgerblue4", "Deep" = "cyan3")
#mort_prob$Destination <- mort_prob$Dest_long
mort_prob$pred <- predict(mlmes)

ggplot(data=mort_prob)+
  geom_boxplot(aes(fill=Destination, x=Genus_long, col = Destination, y=pred),outlier.shape = NA, width = .4, alpha = .4, size = 1.3)+
  geom_point(aes(col = Destination, x=Genus_long, y=pred), size =1.1,
             position=position_jitterdodge(jitter.width = .2,
                                           jitter.height = 0,
                                           dodge.width =.4))+
  theme_bw()+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  ylab("Survival probability")+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, face = "italic"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16))
## fig 2c
ggsave("figs/mortality_prob_estim.png", width =15, height = 7, units ="cm",  dpi = 300)

str(mort_prob)
ggplot(data=mort_prob)+
geom_boxplot(aes(fill=Destination, x=Genus_long, col = Destination, y=mlmes@resp$mu),outlier.shape = NA, width = .4, alpha = .4, size = 1.3)+
geom_point(aes(col = Destination, x=Genus_long, y=mlmes@resp$mu), binaxis='y', size =1.1, stackdir='center',
position=position_jitterdodge(jitter.width = .2,
jitter.height = 0,
dodge.width =.4))+
theme_bw()+
scale_fill_manual(values = cols)+
scale_colour_manual(values = cols)+
ylab("Survival probability")+
theme(text = element_text(size = 18),
axis.title.x = element_blank(),
axis.title.y = element_text(size = 18),
axis.text.x = element_text(vjust = 0.5, face = "italic"),
legend.title = element_blank(),
legend.text =element_blank(),
panel.grid = element_blank(),
strip.text = element_text(size = 16, color = "white"),
strip.background = element_rect(fill="grey40"),
plot.title = element_text(size = 16))

ggplot(data=mort_prob)+
  geom_boxplot(aes(fill=Destination, x=Genus_long, col = Destination, y=mlmes@resp$mu),outlier.shape = NA, width = .4, alpha = .4, size = 1.3)+
  geom_point(aes(col = Destination, x=Genus_long, y=mlmes@resp$mu), binaxis='y', size =1.1, stackdir='center',
             position=position_jitterdodge(jitter.width = .2,
                                           jitter.height = 0,
                                           dodge.width =.4))+
  theme_bw()+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  ylab("Survival probability")+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, face = "italic"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16))

####trait model selection ####
### D ####
#step 1:lm

lmc <- lmeControl(niter = 5000, msMaxIter = 5000)

Dlm <- lm(data = dim2d, lrD ~ Origin*Destination*Genus)

# step 2
Dlm <- gls(data = dim2d, lrD ~ Origin*Destination*Genus)
Dlme1 <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Dlme2a <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Dlme2b <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Dlme2c <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Dlme3a <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
             random = list(~ 1|sourceGen))
Dlme3b <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
             random = list(~ 1|Rack))
Dlme3c <- lme(data = dim2d, lrD ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Dlm,Dlme1,Dlme2a,Dlme2b,Dlme2c,Dlme3a, Dlme3b, Dlme3c)

#step3
Dlme2 <- Dlme2a
summary(Dlme2) # drop triple interaction
anova(Dlme2)

data <- dim2d
response <- dim2d$lrD
random <- list(~ 1|Species, ~ 1|sourceGen)
D <- get_AICs(data, response, random)
D$AICs[D$AICs$AIC == min(D$AICs$AIC), ]
Dlmes <- D$m10


### d ####
#step 1:lm

dlm <- gls(data = dim2d, lrd ~ Origin*Destination*Genus)
dlme1 <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
dlme2a <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
dlme2b <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
dlme2c <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
dlme3a <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
dlme3b <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
dlme3c <- lme(data = dim2d, lrd ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(dlm,dlme1,dlme2a,dlme2b,dlme2c,dlme3a, dlme3b, dlme3c)

#step3
dlme2<- dlme2a
summary(dlme2) # drop triple interaction
anova(dlme2)

data <- dim2d
response <- dim2d$lrd
random <- list(~ 1|Species, ~ 1|sourceGen)
d <- get_AICs(data, response, random)
d$AICs[d$AICs$AIC == min(d$AICs$AIC), ]
dlmes <- d$m10
### L ####
#step 1:lm

Llm <- gls(data = dim2d, lrL ~ Origin*Destination*Genus)
Llme1 <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Llme2a <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Llme2b <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Llme2c <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Llme3a <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Llme3b <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Llme3c <- lme(data = dim2d, lrL ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Llm,Llme1,Llme2a,Llme2b,Llme2c,Llme3a, Llme3b, Llme3c)

#step3

summary(Llme2a) # drop triple interaction
anova(Llme2a)


data <- dim2d
response <- dim2d$lrL
random <- list(~ 1|Species, ~ 1|sourceGen)
L <- get_AICs(data, response, random)
L$AICs[L$AICs$AIC == min(L$AICs$AIC), ]
Llmes <- L$m7
##### V #####
#step 1:lm

Vlm <- gls(data = dim2d, lrV ~ Origin*Destination*Genus)
Vlme1 <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Vlme2a <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Vlme2b <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Vlme2c <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Vlme3a <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Vlme3b <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Vlme3c <- lme(data = dim2d, lrV ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Vlm,Vlme1,Vlme2a,Vlme2b,Vlme2c,Vlme3a, Vlme3b, Vlme3c)


#step3

Vlme4 <- Vlme3a
summary(Vlme4) # drop triple interaction
anova(Vlme4)

data <- dim2d
response <- dim2d$lrV
random <- list(~ 1|sourceGen)
V <- get_AICs(data, response, random)
V$AICs[V$AICs$AIC == min(V$AICs$AIC), ]
Vlmes <- V$m10
##### W #####
#step 1:lm

Wlm <- gls(data = dim2d, lrW ~ Origin*Destination*Genus)
Wlme1 <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Wlme2a <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Wlme2b <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Wlme2c <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Wlme3a <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Wlme3b <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Wlme3c <- lme(data = dim2d, lrW ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Wlm,Wlme1,Wlme2a,Wlme2b,Wlme2c,Wlme3a, Wlme3b, Wlme3c)


#step3
Wlme4 <- Wlme3a
summary(Wlme4) # drop triple interaction
anova(Wlme4)

data <- dim2d
response <- dim2d$lrW
random <- list(~ 1|sourceGen)
W <- get_AICs(data, response, random)
W$AICs[W$AICs$AIC == min(W$AICs$AIC), ]
Wlmes <- W$m10


#### PA ####
## Create explicit nested factors
PAlm <- gls(data = dim2dPA, lrA ~ Origin*Destination*Genus)
PAlme1 <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
PAlme2a <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
PAlme2b <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
PAlme2c <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
PAlme3a <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
PAlme3b <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
PAlme3c <- lme(data = dim2dPA, lrA ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(PAlm,PAlme1,PAlme2a,PAlme2b,PAlme2c,PAlme3a, PAlme3b, PAlme3c)

#step3
PAlme2<- PAlme3a
summary(PAlme2) # drop triple interaction
anova(PAlme2)

data <- dim2dPA
response <- dim2dPA$lrA
random <- list(~ 1|sourceGen)
PA <- get_AICs(data, response, random)
PA$AICs[PA$AICs$AIC == min(PA$AICs$AIC), ]
PAlmes <- PA$m7

png("PArn 2d.png",width = 400, height = 400)
print(PAn)
dev.off()


png("PAln 2d.png",width = 400, height = 400)
print(PAln)
dev.off()

#### R ####

Rlm <- gls(data = dim2dPA, lrR ~ Origin*Destination*Genus)
Rlme1 <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Rlme2a <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Rlme2b <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Rlme2c <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Rlme3a <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Rlme3b <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Rlme3c <- lme(data = dim2dPA, lrR ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Rlm,Rlme1,Rlme2a,Rlme2b,Rlme2c,Rlme3a, Rlme3b, Rlme3c)


#step3
  Rlme2 <- Rlme2a
summary(Rlme2) # drop triple interaction
anova(Rlme2)

data <- dim2dPA
response <- dim2dPA$lrR
random <- list(~ 1|Species, ~ 1|sourceGen)
R <- get_AICs(data, response, random)

R$AICs[R$AICs$AIC == min(R$AICs$AIC), ]
Rlmes <- R$m10

#### C ####

Clm <- gls(data = dim2dPA, lrC ~ Origin*Destination*Genus)
Clme1 <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Clme2a <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Clme2b <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Clme2c <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Clme3a <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Clme3b <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Clme3c <- lme(data = dim2dPA, lrC ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Clm,Clme1,Clme2a,Clme2b,Clme2c,Clme3a, Clme3b, Clme3c)

#step3
Clme2 <- Clme2a
summary(Clme2) # drop triple interaction
anova(Clme2)

data <- dim2dPA
response <- dim2dPA$lrC
random <- list(~ 1|Species, ~ 1|sourceGen)
C <- get_AICs(data, response, random)

C$AICs[C$AICs$AIC == min(C$AICs$AIC), ]
Clmes <- C$m10
#### F ####

Flm <- gls(data = dim2dPA, lrF ~ Origin*Destination*Genus)
Flme1 <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Flme2a <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Flme2b <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Flme2c <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Flme3a <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Flme3b <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Flme3c <- lme(data = dim2dPA, lrF ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Flm,Flme1,Flme2a,Flme2b,Flme2c,Flme3a, Flme3b, Flme3c)


#step3
Flme2 <- Flme3a
summary(Flme2) # drop triple interaction
anova(Flme2)

data <- dim2dPA
response <- dim2dPA$lrF
random <- list(~ 1|sourceGen)
F <- get_AICs(data, response, random)

F$AICs[F$AICs$AIC == min(F$AICs$AIC), ]
Flmes <- F$m17

###### T #####
Tlm <- gls(data = dim2dPA, lrTl ~ Origin*Destination*Genus)
Tlme1 <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
Tlme2a <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
Tlme2b <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
Tlme2c <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
Tlme3a <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
Tlme3b <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
Tlme3c <- lme(data = dim2dPA, lrTl ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(Tlm,Tlme1,Tlme2a,Tlme2b,Tlme2c,Tlme3a, Tlme3b, Tlme3c)

#step3
Tlme2<- Tlme2a
summary(Tlme2) # do not drop triple interaction
anova(Tlme2)
data <- dim2dPA
response <- dim2dPA$lrT
random <- list(~ 1|Species, ~ 1|sourceGen)
T <- get_AICs(data, response, random)

T$AICs[T$AICs$AIC == min(T$AICs$AIC), ]
Tlmes <- T$m10


### fit PCA model ####
#step 1:lm
PCAlm <- gls(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus)
PCAlme1 <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
             random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
PCAlme2a <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|Species, ~ 1|sourceGen))
PCAlme2b <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Rack))
PCAlme2c <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen,~ 1|Species))
PCAlme3a <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|sourceGen))
PCAlme3b <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|Rack))
PCAlme3c <- lme(data = dim2dPA, deltaPCAl ~ Origin*Destination*Genus, 
              random = list(~ 1|Origin))
AIC(PCAlm,PCAlme1,PCAlme2a,PCAlme2b,PCAlme2c,PCAlme3a, PCAlme3b, PCAlme3c)

#step3
data <- dim2dPA
response <- dim2dPA$deltaPCAl
random <- list(~ 1|Species, ~ 1|sourceGen)
PCA <- get_AICs(data, response, random)

PCA$AICs[PCA$AICs$AIC == min(PCA$AICs$AIC), ]
PCAlmes <-PCA$m15

### fit PCA direction model ####
#step 1:lm
PCAdm <- gls(data = dim2dPA, dirPCA ~ Origin*Destination*Genus)
PCAdme1 <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
               random = list(~ 1|Species, ~ 1|sourceGen,~ 1|Rack))
PCAdme2a <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|Species, ~ 1|sourceGen))
PCAdme2b <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|sourceGen,~ 1|Rack))
PCAdme2c <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|sourceGen,~ 1|Species))
PCAdme3a <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|sourceGen))
PCAdme3b <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|Rack))
PCAdme3c <- lme(data = dim2dPA, dirPCA ~ Origin*Destination*Genus, 
                random = list(~ 1|Origin))
AIC(PCAdm,PCAdme1,PCAdme2a,PCAdme2b,PCAdme2c,PCAdme3a, PCAdme3b, PCAdme3c)

#step3
data <- dim2dPA
response <- dim2dPA$dirPCA
random <- list(~ 1|Species, ~ 1|sourceGen)
PCAd <- get_AICs(data, response, random)

PCAd$AICs[PCAd$AICs$AIC == min(PCAd$AICs$AIC), ]
PCAdmes <- PCAd$m10



#### model summary and figures ####
stargazer(Dlmes,dlmes,Llmes,Vlmes,Wlmes, PAlmes, Clmes, Rlmes,Flmes,Tlmes, PCAlmes,PCAdmes,
          type = "html",title="Results -selected fixed and random effect", align=TRUE, out="models_fixed_noNesting.html", omit.stat=c("LL","bic"),
          column.labels = c("lrD", "lrd","lrL", "lrV","lrW", "lrPA","lrC", "lrR","lrF", "lrT","deltaPCA","dirPCA"), model.numbers = FALSE, 
          star.cutoffs = 0.05, notes = "*p<0.05", notes.append = FALSE)
stargazer(Dlme1,dlme1,Llme1,Vlme1,Wlme1,PAlme1,
          type = "html",title="Results -full models", align=TRUE, out="models_full1.html",
          covariate.labels = c("Origin(S)","Destination(S)","Genus(P)", "Origin(S):Destination(S)", 
          "Origin(S):Genus(P)", "Destination(S):Genus(P)","Origin(S):Destination(S):Genus(P)",
          "Intercept"),
          omit.stat=c("LL","bic"), model.numbers = FALSE, 
          star.cutoffs = 0.05/8, notes = "*p<0.05", ci = TRUE,  notes.append = FALSE)

stargazer( Clme1, Rlme1,Flme1,Tlme1, PCAlme1,PCAdme1,
          type = "html",title="Results -full models", align=TRUE, out="models_full2.html",
          covariate.labels = c("Origin(S)","Destination(S)","Genus(P)", "Origin(S):Destination(S)", 
                               "Origin(S):Genus(P)", "Destination(S):Genus(P)","Origin(S):Destination(S):Genus(P)",
                               "Intercept"),
          omit.stat=c("LL","bic"), model.numbers = FALSE, 
          star.cutoffs = 0.05/8, notes = "*p<0.05", ci = TRUE,  notes.append = FALSE)


p.adj <- as.data.frame(
  rbind(
    c("mortality", p.adjust(summary(mlme1)$coefficients[, 4], method = "bonferroni", n = 8)),
    c("lrD", p.adjust(summary(Dlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrd", p.adjust(summary(dlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrL", p.adjust(summary(Llme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrV", p.adjust(summary(Vlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrW", p.adjust(summary(Wlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrPA", p.adjust(summary(PAlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrC", p.adjust(summary(Clme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrR", p.adjust(summary(Rlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrF", p.adjust(summary(Flme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("lrT", p.adjust(summary(Tlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("deltaPCA", p.adjust(summary(PCAlme1)$tTable[, 5], method = "bonferroni", n = 8)),
    c("dirPCA", p.adjust(summary(PCAdme1)$tTable[, 5], method = "bonferroni", n = 8))
  )
)

p.adj.holm <-as.data.frame(
  rbind(
    c("mortality", p.adjust(summary(mlme1)$coefficients[, 4], method = "holm", n = 8)),
    c("lrD", p.adjust(summary(Dlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrd", p.adjust(summary(dlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrL", p.adjust(summary(Llme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrV", p.adjust(summary(Vlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrW", p.adjust(summary(Wlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrPA", p.adjust(summary(PAlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrC", p.adjust(summary(Clme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrR", p.adjust(summary(Rlme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrF", p.adjust(summary(Flme1)$tTable[, 5], method = "holm", n = 8)),
    c("lrT", p.adjust(summary(Tlme1)$tTable[, 5], method = "holm", n = 8)),
    c("deltaPCA", p.adjust(summary(PCAlme1)$tTable[, 5], method = "holm", n = 8)),
    c("dirPCA", p.adjust(summary(PCAdme1)$tTable[, 5], method = "holm", n = 8))
  )
)


summary_full <-
  rbind(
    cbind(var = "0. mortality", as.data.frame( tidy(mlme1,conf.int=TRUE,effects="fixed"))[,-1]),
    cbind(var = "a. lrD", as.data.frame( tidy(Dlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "b. lrd", as.data.frame( tidy(dlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "c. lrL", as.data.frame( tidy(Llme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "d. lrV", as.data.frame( tidy(Vlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "e. lrW", as.data.frame( tidy(Wlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "f. lrPAl", as.data.frame( tidy(PAlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "g. lrC", as.data.frame( tidy(Clme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "h. lrR", as.data.frame( tidy(Rlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "i. lrF", as.data.frame( tidy(Flme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "l. lrT", as.data.frame( tidy(Tlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "m. deltaPCA", as.data.frame( tidy(PCAlme1,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "n. dirPCA", as.data.frame( tidy(PCAdme1,conf.int=TRUE,effects="fixed"))[,-4]))


summary_full$sig <- 0
summary_full$sig[summary_full$p.value <= 0.05] <- 1

summary_full$sigcor <- 0
summary_full$sigcor[summary_full$p.value*8 <= 0.05] <- 1
summary_full$int <- 0



order <- c("OriginS"="b", "DestinationS"="c", "GenusP"="d", 
           "OriginS:DestinationS"="e", "OriginS:GenusP"="f",
           "DestinationS:GenusP" = "g", "GenusP:DestinationS" = "g",
           "OriginS:DestinationS:GenusP" = "h",
           "(Intercept)" = "a")
summary_full$Term_order <- order[summary_full$term]

labs <- list('b'="Origin(S)", 'c'="Destination(S)", 'd'="Genus(P)", 
           'e'="Origin(S):Destination(S)", 'f'="Origin(S):Genus(P)",
           'g' = "Destination(S):Genus(P)",'g' = "Genus(P):Destination(S)",
           'h' = "Origin(S):Destination(S):Genus(P)",
           'a'= "Intercept")

summary_full$Term_labels <- as.character(pairs[summary_full$Term_order])

summary_full$int[summary_full$term == "OriginS"| summary_full$term == "DestinationS"| summary_full$term == "GenusP"] <- 1
summary_full$int[summary_full$term == "OriginS:DestinationS"| summary_full$term == "OriginS:GenusP"| summary_full$term == "DestinationS:GenusP"] <- 2
 summary_full$int[summary_full$term == "OriginS:DestinationS:GenusP"] <- 3

 facet_labeller <- function(variable,value){
   return(labs[value])
 }

# 
# ggplot()+
#   geom_pointrange(data=summary_full[summary_full$var != "0. mortality" & summary_full$term != "(Intercept)",],
#                   mapping=aes(y=reorder(var, desc(var)), x=estimate, xmin=conf.low, xmax=conf.high, col = factor(sigcor)), fill = "grey",
#                   position="identity", width=5, size=2, fatten = 1.5, linewidth=5)+
#   theme_bw() +
#   theme(panel.border=element_blank(), axis.line=element_line(), axis.line.y=element_blank()) +  
#   theme(axis.ticks=element_blank()) +
#   # Dispose of the legend
#   theme(legend.position="none") +
#   xlab("Estimates and confidence intervals") +
#   ylab("Response variable") +
#   scale_colour_manual(values = c("grey50", "black"))+
#   facet_wrap(~Term_order, scales = "free_x", labeller = facet_labeller)+
#   # Big bold line at y=0
#   geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
#   themeRN

ggplot()+
  geom_linerange(data=summary_full[summary_full$var != "0. mortality" & summary_full$term != "(Intercept)",],
                  mapping=aes(y=reorder(var, desc(var)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=summary_full[summary_full$var != "0. mortality" & summary_full$term != "(Intercept)",],
                  mapping=aes(y=reorder(var, desc(var)), x=estimate, col = factor(sigcor)), size = 4)+
  xlab("Estimates with 95% confidence intervals") +
  ylab("Response variable/n") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  facet_wrap(~Term_order, scales = "free_x", labeller = facet_labeller)+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "Response variable")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 12), axis.title = element_text(size = 15))+
  themeRN

## fig. SM9
ggsave("figs/summary_full_estim.png", width =28, height = 23, units ="cm",  dpi = 300)


summary_sel <-
  rbind(
#    cbind(var = "0. mortality", as.data.frame( tidy(mglme,conf.int=TRUE,effects="fixed"))[,-1]),
    cbind(var = "a. lrD", as.data.frame( tidy(Dlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "b. lrd", as.data.frame( tidy(dlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "c. lrL", as.data.frame( tidy(Llmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "d. lrV", as.data.frame( tidy(Vlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "e. lrW", as.data.frame( tidy(Wlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "f. lrPAl", as.data.frame( tidy(PAlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "g. lrC", as.data.frame( tidy(Clmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "h. lrR", as.data.frame( tidy(Rlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "i. lrF", as.data.frame( tidy(Flmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "l. lrT", as.data.frame( tidy(Tlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "m. deltaPCA", as.data.frame( tidy(PCAlmes,conf.int=TRUE,effects="fixed"))[,-4]),
    cbind(var = "n. dirPCA", as.data.frame( tidy(PCAdmes,conf.int=TRUE,effects="fixed"))[,-4]))


summary_sel$sig <- 0
summary_sel$sig[summary_sel$p.value <= 0.05] <- 1


summary_sel$Term_order <- order[summary_sel$term]
summary_sel$Term_labels <- as.character(pairs[summary_sel$Term_order])

ggplot()+
  geom_pointrange(data=summary_sel[summary_sel$var != "0. mortality" & summary_sel$term != "(Intercept)",],
                  mapping=aes(y=var, x=estimate, xmin=conf.low, xmax=conf.high, col = factor(sig)), 
                  position="identity", width=1, size=.5, linewidth=2)+
  theme_bw() +
  theme(panel.border=element_blank(), axis.line=element_line(), axis.line.y=element_blank()) +  
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  xlab("Estimates and confidence intervals") +
  ylab("Response variable") +
  scale_colour_manual(values = c("grey70", "navy"), aesthetics = c("colour", "fill"))+
  facet_wrap(~Term_order, scales = "free_x", labeller = facet_labeller)+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  themeRN

ggplot()+
  geom_linerange(data=summary_sel[summary_sel$var != "0. mortality" & summary_sel$term != "(Intercept)",],
                 mapping=aes(y=reorder(var, desc(var)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=summary_sel[summary_sel$var != "0. mortality" & summary_sel$term != "(Intercept)",],
             mapping=aes(y=reorder(var, desc(var)), x=estimate, col = factor(sig)), size = 4)+
  xlab("Estimates with 95% confidence intervals") +
  ylab("Response variable/n") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  facet_wrap(~Term_order, scales = "free_x", labeller = facet_labeller)+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "Response variable")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 12), axis.title = element_text(size = 15))
## fig. 4
ggsave("figs/summary_sel_estim.png", width =20, height = 15, units ="cm",  dpi = 300)

#####PCA figures ####

##fig 3.c
PCA_sel <- rbind(cbind(var = "deltaPCA", 
                             as.data.frame( tidy(PCAlmes,conf.int=TRUE,effects="fixed"))[,-1]))
PCA_sel$Term_labels <- c("Intercept", "Genus(P)", "Destination(S)") 
order <- c("Origin(S)"="b", "Destination(S)"="c", "Genus(P)"="d", 
           "Genus(P):Destination(S)"="e", "Origin(S):Genus(P)"="f",
           "Destination(S):GenusP" = "g", "Genus(P):Destination(S)" = "g",
           "Origin(S):Destination(S):Genus(P)" = "h",
           "Intercept" = "a")
PCA_full$Term_order <- order[mortality_full$term]
PCA_sel$Term_order <- order[PCA_sel$Term_labels]
PCA_sel$sig <- 0
PCA_sel$sig[PCA_sel$p.value <= 0.05] <- 1

PCA_sel_f <-ggplot()+
  geom_linerange(data=PCA_sel,
                 mapping=aes(y=reorder(Term_labels,desc(Term_order)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=PCA_sel,
             mapping=aes(y=Term_labels, x=estimate), col = "black", size = 4)+
  xlab("Estimates with 95% CI") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "", title = "deltaPCA")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 18), 
        panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 18), axis.title = element_text(size = 18),axis.title.x = element_blank())+
  themeRN

png("figs/PCAsel.png",width = 11, height = 6, units = "cm", res = 300)
print(PCA_sel_f)
dev.off()

## direction
PCA_dir_sel <- rbind(cbind(var = "dirPCA", 
                       as.data.frame( tidy(PCAdmes,conf.int=TRUE,effects="fixed"))[,-1]))
PCA_dir_sel$Term_labels <- c("Intercept", "Genus(P)", "Destination(S)", "Genus(P):Destination(S)") 
order <- c("Origin(S)"="b", "Destination(S)"="c", "Genus(P)"="d", 
           "Genus(P):Destination(S)"="e", "Origin(S):Genus(P)"="f",
           "Destination(S):GenusP" = "g", "Genus(P):Destination(S)" = "g",
           "Origin(S):Destination(S):Genus(P)" = "h",
           "Intercept" = "a")
PCA_full$Term_order <- order[mortality_full$term]
PCA_dir_sel$Term_order <- order[PCA_dir_sel$Term_labels]
PCA_dir_sel$sig <- 0
PCA_dir_sel$sig[PCA_dir_sel$p.value <= 0.05] <- 1

PCA_dir_sel_f <-ggplot()+
  geom_linerange(data=PCA_dir_sel,
                 mapping=aes(y=reorder(Term_labels,desc(Term_order)), xmin=conf.low, xmax=conf.high), size = 3, col = "grey80")+
  geom_point(data=PCA_dir_sel,
             mapping=aes(y=Term_labels, x=estimate), col = "black", size = 4)+
  xlab("Estimates with 95% CI") +
  scale_colour_manual(values = c("grey60", "grey20"))+
  # Big bold line at y=0
  geom_vline(xintercept=0,size=1, alpha=0.3, linetype="dashed")+
  theme_minimal() +
  labs(y= "", title = "dirPCA")+
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 18), 
        panel.grid.major.y = element_line(colour="grey90", size=0.5),legend.position="none", 
        axis.text = element_text(size = 18), axis.title = element_text(size = 18),axis.title.x = element_blank())+
  themeRN

png("figs/PCAdirsel.png",width = 11, height = 6, units = "cm", res = 300)
print(PCA_dir_sel_f)
dev.off()

#### random effects - fig 5 #####

ran.dt <- data.frame()
ran.sp.dt<- data.frame()
labs2 <- c("Dlmes","dlmes","Llmes"," Clmes"," Rlmes","Tlmes"," PCAlmes", " PCAdmes")
labs1 <- c("Vlmes","Wlmes"," PAlmes","Flmes")
mods2 <- list(Dlmes,dlmes,Llmes,Clmes, Rlmes,Tlmes, PCAlmes, PCAdmes)
mods1 <- list(Vlmes,Wlmes, PAlmes,Flmes)
for (i in 1:8) {
  re <- as.data.frame(ranef(mods2[[i]])$sourceGen)
  colnames(re) <- "re"
  ran.dt <- rbind(cbind(mod = labs2[i], eff = "gen",re, Species = str_sub(row.names(re),start = 1, end = 2)),ran.dt)
}
for (i in 1:4) {
  re <- as.data.frame(ranef(mods1[[i]]))
  colnames(re) <- "re"
  ran.dt <- rbind(cbind(mod = labs1[i], eff = "gen",re, Species = str_sub(row.names(re),start = 3, end = 4)),ran.dt)
}


for (i in 1:8) {
  re <- as.data.frame(ranef(mods2[[i]])$Species)
  colnames(re) <- "re"
  ran.sp.dt <- rbind(cbind(mod = labs2[i], eff = "sp",re, Species = row.names(re)),ran.sp.dt)
}

ranef.dt <- rbind(ran.dt, ran.sp.dt)

ggplot(ranef.dt, aes(y=re,x=Species, col = Species)) +
    geom_boxplot()+facet_wrap(~mod, scales = "free_y")

labs <- list('Dlmes' = "a. lrD", 'dlmes' = "b. lrd", 'Llmes' = "c. lrL", 'Vlmes' = "d. lrV",
             'Wlmes' = "e. lrW", ' PAlmes'  = "f. lrPA", ' Clmes' = "g. lrC", ' Rlmes' = "h. lrR", 
             'Flmes' = "i. lrF", 'Tlmes' = "l. lrT", ' PCAlmes' = "m. δPCA", ' PCAdmes' = "n. dirPCA")
ranef.dt$Term_labels <- as.character(labs[ranef.dt$mod])
labs2 <- list('gen' = "Genotype", 'sp' = "Species")
ranef.dt$eff <- as.character(labs2[ranef.dt$eff])

ranef.dt1 <- ranef.dt[ranef.dt$mod == "Vlmes"|ranef.dt$mod == "Wlmes"|ranef.dt$mod == " PAlmes"|ranef.dt$mod == "Flmes",]
ranef.dt2 <- ranef.dt[ranef.dt$mod != "Vlmes" & ranef.dt$mod != "Wlmes" & ranef.dt$mod != " PAlmes" & ranef.dt$mod != "Flmes",]

g1 <- ggplot(ranef.dt1, aes(y=re,x=Species, col = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70")+
  geom_point() + facet_grid(eff~Term_labels, scales = "free_x") + 
  theme_bw()+
  scale_color_viridis_d()+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16),
        legend.position="bottom")
g2 <-ggplot(ranef.dt2, aes(y=re,x=Species, col = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70")+
  geom_point() + facet_grid(eff~Term_labels, scales = "free_x") + 
  theme_bw()+
  scale_color_viridis_d()+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16),
        legend.position="bottom")
g1 <- ggplot(ranef.dt1, aes(y=re,x=Species, col = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70")+
  geom_point() + facet_grid(eff~Term_labels, scales = "free_x") + 
  theme_bw()+
  scale_color_viridis_d()+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16),
        legend.position="bottom")

g <- ggplot(ranef.dt, aes(y=re,x=Species, col = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70")+
  geom_point() + facet_grid(eff~Term_labels, scales = "free_x") + 
  theme_bw()+
  scale_color_viridis_d()+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 0.5, angle = 45),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 14, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16),
        legend.position="bottom")
g
ggsave("figs/random_eff.png", width =33, height = 11, units ="cm",  dpi = 300)

ran.dt.full <- data.frame()
labs <- c("Dlme1","dlme1","Llme1"," Clme1"," Rlme1","Tlme1"," PCAlme1", "Vlme1","Wlme1"," PAlme1","Flme1")
mods <- list(Dlme1,dlme1,Llme1,Clme1, Rlme1,Tlme1, PCAlme1, Vlme1,Wlme1, PAlme1,Flme1)

for (i in 1:11) {
  re <- as.data.frame(ranef(mods[[i]])$sourceGen)
  colnames(re) <- "re"
  ran.dt.full <- rbind(cbind(mod = labs[i], eff = "gen",re, Species = str_sub(row.names(re),start = 1, end = 2)),ran.dt.full)
}
for (i in 1:11) {
  re <- as.data.frame(ranef(mods[[i]])$Species)
  colnames(re) <- "re"
  ran.dt.full <- rbind(cbind(mod = labs[i], eff = "sp",re, Species = row.names(re)),ran.dt.full)
}

ggplot(ran.dt.full, aes(y=re,x=eff, col = Species)) +
  geom_boxplot()+facet_wrap(~mod, scales = "free")

######## AICs table SM 1######
colnames(D$AICs) <- c("df", "lrD")
colnames(d$AICs) <- c("df", "lrd")
colnames(L$AICs) <- c("df", "lrL")
colnames(V$AICs) <- c("df", "lrV")
colnames(W$AICs) <- c("df", "lrW")
colnames(PA$AICs) <- c("df", "lrA")
colnames(C$AICs) <- c("df", "lrC")
colnames(R$AICs) <- c("df", "lrR")
colnames(F$AICs) <- c("df", "lrF")
colnames(T$AICs) <- c("df", "lrT")
colnames(PCA$AICs) <- c("df", "deltaPCA")
colnames(PCAd$AICs) <- c("df", "dirPCA")
AICs_table <- D$AICs
AICs_table <- cbind(AICs_table,d$AICs$lrd, L$AICs$lrL, V$AICs$lrV, W$AICs$lrW, PA$AICs$lrA, 
                    C$AICs$lrC, R$AICs$lrR, F$AICs$lrF, T$AICs$lrT, PCA$AICs$deltaPCA, PCAd$AICs$dirPCA )
colnames(AICs_table) <- c("df", "lrD", "lrd","lrL", "lrV","lrW", "lrPA","lrC", "lrR","lrF", "lrT","deltaPCA","dirPCA")
AICs_table
#write.csv(AICs_table, "output/AICs_table.csv", row.names = TRUE)


### figures ####
data_long <- as.data.frame(dim2dPA %>%
                             select(lrD,lrd, lrL, lrV,lrW,lrA,lrC,lrR, lrF, lrT, deltaPCA, dirPCA, Genus, Destination) %>%
                             gather(var, value, c("lrD","lrd", "lrL", "lrV","lrW","lrA","lrC","lrR", "lrF", "lrT", "deltaPCA", "dirPCA")))
var_labs <- c("lrD"="a. lrD","lrd" ="b. lrd", "lrL"="c. lrL", 
           "lrV"="d. lrV", "lrW"="e. lrW",
           "lrC" = "g. lrC",
           "lrA" = "f. lrA",
           "lrR" = "h. lrR", "lrF" = "i. lrF", "lrT"= "l. lrT",
           "deltaPCA"= "m. deltaPCA",
           "dirPCA"= "n. dirPCA")
data_long$var_lab <- var_labs[data_long$var]


# Interactions

dest_labs <- c("D"="Deep","S" ="Shallow")
data_long$dest_lab <- dest_labs[data_long$Destination]

Destination.labs <- c("Deep treatment", "Shallow treatment")
names(Destination.labs) <- c("D", "S")

gen_labs <- c("A"="Acropora sp.","P" ="Porites sp.")
data_long$gen_lab <- gen_labs[data_long$Genus]

cols <- c("D" = "dodgerblue4", "S" = "cyan3")


ggplot(data=data_long)+
  geom_boxplot(aes(fill=Destination, x=gen_lab, col = Destination, y=value),outlier.shape = NA, width = .4, alpha = .4, size = 1.3)+
  geom_point(aes(col=Destination, x=gen_lab, y=value), binaxis='y', size =1.1, stackdir='center',
              position=position_jitterdodge(jitter.width = .2,
                                            jitter.height = 0,
                                            dodge.width =.4))+
  theme_bw()+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  facet_wrap(~var_lab, scales = "free_y", ncol = 3)+
    theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = 0.5, face = "italic"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16))


ggsave("figs/interactions.png", width =25, height = 25, units ="cm",  dpi = 300)

###PCA interaction #####

str(data_long)
dataPCA <- data_long[data_long$var == "dirPCA",]
str(dataPCA)

PCA_int <- ggplot(data=dataPCA)+
  geom_boxplot(aes(fill=Destination, x=gen_lab, col = Destination, y=value),outlier.shape = NA, width = .4, alpha = .4, size = 1.3)+
  geom_point(aes(col=Destination, x=gen_lab, y=value), binaxis='y', size =1.1, stackdir='center',
             position=position_jitterdodge(jitter.width = .2,
                                           jitter.height = 0,
                                           dodge.width =.4))+
  theme_bw()+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  ylab("direction PCA")+
  theme(text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(vjust = 0.5, face = "italic"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 16, color = "white"),
        strip.background = element_rect(fill="grey40"),
        plot.title = element_text(size = 16))
ggsave("PCAint.png", width =15, height = 7, units ="cm",  dpi = 300)
png("figs/PCAint.png",width = 15, height = 6, units = "cm", res = 300)
print(PCA_int)
dev.off()


ggsave("figs/PCAinteractions.png", width =25, height = 25, units ="cm",  dpi = 300)


