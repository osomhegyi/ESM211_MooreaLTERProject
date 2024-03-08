# GLARMA example
# March 2024
# Modified by Tim Commerford, Zack de la Rocha, Tom Morello and Brad Wilk with contributions from CLJ
################

library(glarma)
library(here) # makes a local directory for the R project
library(tidyverse)

#imports eBird species richness data for Pepperwood Preserve
# SR<-read.csv(here("data","SR_df.csv"))
PF<-read.csv(here("data","common_parrotfish_data.csv"))
PF2<-read.csv(here("data","common_parrotfish_data_site2.csv"))
PF3<-read.csv(here("data","common_parrotfish_data_site3.csv"))

############################################
#Change: the intercept should be 1 not 0
PF$intercept<-1
PF2$intercept<-1
PF3$intercept<-1
############################################

# Y.SR<-SR$S # makes the response variable only the species richness (constant across models)
Y.PF<-PF$annual_count
Y.PF2<-PF2$annual_count
Y.PF3<-PF3$annual_count

# X.1.SR<-SR %>% select(intercept, dist) #makes model 1's explanatory variables of the intercept (1)
X.1.PF<-PF |> select(intercept, major_event_crown_of_thorns) # makes a model with crown of thorns event and intercept (1)
X.2.PF<-PF |> select(intercept, major_event_bleaching) # makes a model with crown of thorns event and intercept (1)
X.3.PF<-PF |> select(intercept, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)
X.4.PF<-PF |> select(intercept, major_event_crown_of_thorns, major_event_bleaching)
X.5.PF<-PF |> select(intercept, major_event_cylcone, major_event_bleaching)
X.6.PF<-PF |> select(intercept, major_event_cylcone, major_event_crown_of_thorns)
X.7.PF<-PF |> select(intercept, major_event_bleaching, major_event_crown_of_thorns, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)

#site 2 (best model for site 1)
X.4.PF2<-PF2 |> select(intercept, major_event_bleaching, major_event_crown_of_thorns, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)

#site 3 (best model for site 1)
X.4.PF3<-PF3 |> select(intercept, major_event_bleaching, major_event_crown_of_thorns, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)
  
#Makes into a matrix
# X.1.SR<-as.matrix(X.1.SR) 
X.1.PF<-as.matrix(X.1.PF)
X.2.PF<-as.matrix(X.2.PF)
X.3.PF<-as.matrix(X.3.PF)
X.4.PF<-as.matrix(X.4.PF)
X.5.PF<-as.matrix(X.5.PF)
X.6.PF<-as.matrix(X.6.PF)
X.7.PF<-as.matrix(X.7.PF)

X.4.PF2<-as.matrix(X.4.PF2)
X.4.PF3<-as.matrix(X.4.PF3)



#SR model 1
# model with fire events included
# glarmamod.1.SR <- glarma(Y.SR, X.1.SR, phiLags = c(1), type = "Poi", method = "FS",
                      # residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.1A.PF <- glarma(Y.PF, X.1.PF, phiLags = c(1), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.1B.PF <- glarma(Y.PF, X.1.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.2.PF <- glarma(Y.PF, X.2.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.3.PF <- glarma(Y.PF, X.3.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.4.PF <- glarma(Y.PF, X.4.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.5.PF <- glarma(Y.PF, X.5.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.6.PF <- glarma(Y.PF, X.6.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.7.PF <- glarma(Y.PF, X.7.PF, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)

glarmamod.4.PF2 <- glarma(Y.PF2, X.4.PF2, phiLags = c(2), type = "Poi", method = "FS",
                         residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod.4.PF3 <- glarma(Y.PF3, X.4.PF3, phiLags = c(2), type = "Poi", method = "FS",
                          residuals = "Pearson", maxit = 100, grad = 1e-6)

#summary statistics just like getting information out of linear regression
# summary(glarmamod.1.SR)
glarmamod.1A.PF.summary<-summary(glarmamod.1A.PF)
aic_value <- glarmamod.1A.PF.summary$aic
summary(glarmamod.1B.PF)
summary(glarmamod.2.PF)
summary(glarmamod.3.PF)
summary(glarmamod.4.PF)
summary(glarmamod.5.PF)
summary(glarmamod.6.PF)
summary(glarmamod.7.PF)


summary(glarmamod.4.PF2)
summary(glarmamod.4.PF3)


# 
# #### Modified SR plot for Pepperwood
# SR_mod<-SR
# 
# SR_mod$est<-glarmamod.1.SR$fitted.values #adds the fitted values of the GLARMA model to the data
# 
# #plots the model (black) and the species richness data (gray)
# SR_plot_mod<-ggplot(SR_mod, aes(x=year)) + 
#   geom_point(col="gray",aes(y=S),size=3) + 
#   geom_point(col="black",aes(y=est),size=3) +
#   geom_line(col="black",aes(y=est)) +
#   labs(x="Year", y="Species Richness") +
#   ylim(0,150) + xlim(2005,2023)+
#   theme_bw()
# 
# #Adds two red horizontal lines for the occurance and duration of the fires
# SR_plot_mod<-SR_plot_mod + annotate("rect", xmin = 2017.767, xmax =2017.833 , ymin = 0, ymax = 150, alpha = .75,fill = "red")+annotate("rect", xmin = 2019.808, xmax =2019.855 , ymin = 0, ymax = 150, alpha = .75,fill = "red")
# SR_plot_mod
# ggsave(here("plot","Pepperwood_SR_Glarma.jpg"),SR_plot_mod, dpi=500, width=6, height=3, unit="in")
# 

#### Modified PF plot for Moorea
PF_mod<-PF

PF_mod$est<-glarmamod.1B.PF$fitted.values #adds the fitted values of the GLARMA model to the data

#plots the model (black) and the species richness data (gray)
PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
  geom_point(col="gray",aes(y=annual_count),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Parrot Fish Count") +
  ylim(0,400) + xlim(2005,2023)+
  theme_bw()

PF_plot_mod
ggsave(here("plot","Moorea_PF_Glarma_m1.jpg"),PF_plot_mod, dpi=500, width=6, height=3, unit="in")


#site 2 model 4 figure
PF_mod<-PF2

PF_mod$est<-glarmamod.4.PF2$fitted.values #adds the fitted values of the GLARMA model to the data

#plots the model (black) and the species richness data (gray)
PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
  geom_point(col="gray",aes(y=annual_count),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Parrot Fish Count") +
  ylim(0,400) + xlim(2005,2023)+
  theme_bw()

PF_plot_mod

#site 3 model 4 figure
PF_mod<-PF3

PF_mod$est<-glarmamod.4.PF3$fitted.values #adds the fitted values of the GLARMA model to the data

#plots the model (black) and the species richness data (gray)
PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
  geom_point(col="gray",aes(y=annual_count),size=3) + 
  geom_point(col="black",aes(y=est),size=3) +
  geom_line(col="black",aes(y=est)) +
  labs(x="Year", y="Parrot Fish Count") +
  ylim(0,400) + xlim(2005,2023)+
  theme_bw()

PF_plot_mod



