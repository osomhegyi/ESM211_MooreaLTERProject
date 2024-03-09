# GLARMA Moorea Parrotfish
# March 2024
################

library(glarma)
library(here) # makes a local directory for the R project
library(tidyverse)
library(gridExtra)

#############################################load in that data#############################
PF1<-read.csv(here("data","common_parrotfish_data.csv"))
PF2<-read.csv(here("data","common_parrotfish_data_site2.csv"))
PF3<-read.csv(here("data","common_parrotfish_data_site3.csv"))
PF4<-read.csv(here("data","common_parrotfish_data_site4.csv"))
PF5<-read.csv(here("data","common_parrotfish_data_site5.csv"))
PF6<-read.csv(here("data","common_parrotfish_data_site6.csv"))


###########################################load the model#####################################
model_business <- function(PF, site_number) {
  PF$intercept<-1
  Y.PF<-PF$annual_count
  
  X.1.PF<-PF |> select(intercept, major_event_crown_of_thorns) # makes a model with crown of thorns event and intercept (1)
  X.2.PF<-PF |> select(intercept, major_event_bleaching) # makes a model with crown of thorns event and intercept (1)
  X.3.PF<-PF |> select(intercept, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)
  X.4.PF<-PF |> select(intercept, major_event_crown_of_thorns, major_event_bleaching)
  X.5.PF<-PF |> select(intercept, major_event_cylcone, major_event_bleaching)
  X.6.PF<-PF |> select(intercept, major_event_cylcone, major_event_crown_of_thorns)
  X.7.PF<-PF |> select(intercept, major_event_bleaching, major_event_crown_of_thorns, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)
  
  X.1.PF<-as.matrix(X.1.PF)
  X.2.PF<-as.matrix(X.2.PF)
  X.3.PF<-as.matrix(X.3.PF)
  X.4.PF<-as.matrix(X.4.PF)
  X.5.PF<-as.matrix(X.5.PF)
  X.6.PF<-as.matrix(X.6.PF)
  X.7.PF<-as.matrix(X.7.PF)
  
  # glarmamod.1A.PF <- glarma(Y.PF, X.1.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.1B.PF <- glarma(Y.PF, X.1.PF, phiLags = c(2), type = "Poi", method = "FS",
                            residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.2A.PF <- glarma(Y.PF, X.2.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.2B.PF <- glarma(Y.PF, X.2.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.3A.PF <- glarma(Y.PF, X.3.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.3B.PF <- glarma(Y.PF, X.3.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.4A.PF <- glarma(Y.PF, X.4.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.4B.PF <- glarma(Y.PF, X.4.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.5A.PF <- glarma(Y.PF, X.5.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.5B.PF <- glarma(Y.PF, X.5.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.6A.PF <- glarma(Y.PF, X.6.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.6B.PF <- glarma(Y.PF, X.6.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  # glarmamod.7A.PF <- glarma(Y.PF, X.7.PF, phiLags = c(1), type = "Poi", method = "FS",
  #                          residuals = "Pearson", maxit = 100, grad = 1e-6)
  glarmamod.7B.PF <- glarma(Y.PF, X.7.PF, phiLags = c(2), type = "Poi", method = "FS",
                           residuals = "Pearson", maxit = 100, grad = 1e-6)
  
  # sum_1A<-summary(glarmamod.1A.PF)
  # sum_2A<-summary(glarmamod.2A.PF)
  # sum_3A<-summary(glarmamod.3A.PF)
  # sum_4A<-summary(glarmamod.4A.PF)
  # sum_5A<-summary(glarmamod.5A.PF)
  # sum_6A<-summary(glarmamod.6A.PF)
  # sum_7A<-summary(glarmamod.7A.PF)
  sum_1B<-summary(glarmamod.1B.PF)
  sum_2B<-summary(glarmamod.2B.PF)
  sum_3B<-summary(glarmamod.3B.PF)
  sum_4B<-summary(glarmamod.4B.PF)
  sum_5B<-summary(glarmamod.5B.PF)
  sum_6B<-summary(glarmamod.6B.PF)
  sum_7B<-summary(glarmamod.7B.PF)
  
  aic_vector<-c(
                # sum_1A$aic, 
                sum_1B$aic, 
                # sum_2A$aic, 
                sum_2B$aic, 
                # sum_3A$aic, 
                sum_3B$aic, 
                # sum_4A$aic, 
                sum_4B$aic, 
                # sum_5A$aic, 
                sum_5B$aic, 
                # sum_6A$aic, 
                sum_6B$aic,
                # sum_7A$aic, 
                sum_7B$aic)
  
  model_vector<-c(
                  # "1A", 
                  "1B", 
                  # "2A", 
                  "2B", 
                  # "3A", 
                  "3B", 
                  # "4A", 
                  "4B", 
                  # "5A", 
                  "5B", 
                  # "6A", 
                  "6B", 
                  # "7A", 
                  "7B")
  result_df <- data.frame(model = model_vector, aic = aic_vector)
  result_df$site<-site_number
  
  #coefficients dataframe for 7B
  coefficients_df <- sum_7B$coefficients1
  coefficients_df$site<-site_number
  
  # return(result_df)
  return(list(result_df, coefficients_df, glarmamod.7B.PF))
}

#outputs for site 1
site1_results_list<-model_business(PF1, 1) 
site1_results<- site1_results_list[[1]]  #for choosing model
site1_coefficients_df <- site1_results_list[[2]]  #for coefficient plotting
site1_7B_output<-site1_results_list[[3]] #for plot model vs data

#outputs for site 2
site2_results_list<-model_business(PF2, 2)
site2_results<- site2_results_list[[1]]  
site2_coefficients_df <- site2_results_list[[2]] 
site2_7B_output<-site2_results_list[[3]] 

#outputs for site 3
site3_results_list<-model_business(PF3, 3)
site3_results<- site3_results_list[[1]]  
site3_coefficients_df <- site3_results_list[[2]]
site3_7B_output<-site3_results_list[[3]] 

#outputs for site 4
site4_results_list<-model_business(PF4, 4)
site4_results<- site4_results_list[[1]]  
site4_coefficients_df <- site4_results_list[[2]] 
site4_7B_output<-site4_results_list[[3]] 

#outputs for site 5
site5_results_list<-model_business(PF5, 5)
site5_results<- site5_results_list[[1]]  
site5_coefficients_df <- site5_results_list[[2]]
site5_7B_output<-site5_results_list[[3]] 

#outputs for site 6
site6_results_list<-model_business(PF6, 6)
site6_results<- site6_results_list[[1]]  
site6_coefficients_df <- site6_results_list[[2]] 
site6_7B_output<-site6_results_list[[3]] 


##################################keep only the best models############################
best_aic_model<-function(input_df){

  # Find the row with the lowest AIC value
  lowest_aic <- min(input_df$aic)
  
  # Find AIC values within 7 of the lowest AIC
  close_aic <- input_df$aic <= (lowest_aic + 7)
  
  # Filter the dataframe to keep rows with the lowest AIC and AIC values within 7 of the lowest AIC
  df_filtered <- input_df[site1_results$aic == lowest_aic | close_aic, ]
  
}

best_site1<-best_aic_model(site1_results)
best_site2<-best_aic_model(site2_results)
best_site3<-best_aic_model(site3_results)
best_site4<-best_aic_model(site4_results)
best_site5<-best_aic_model(site5_results)
best_site6<-best_aic_model(site6_results)

#################################plotting each of the coefficients for 7B########################

#bleaching first
bleaching_coefficient_fun<-function(input_df){
  
  input_df$major_events <- rownames(input_df)
  rownames(input_df) <- NULL
  bleaching_coefficient<-input_df[2, ]
  
}

site1_bleaching<-bleaching_coefficient_fun(site1_coefficients_df)
site2_bleaching<-bleaching_coefficient_fun(site2_coefficients_df)
site3_bleaching<-bleaching_coefficient_fun(site3_coefficients_df)
site4_bleaching<-bleaching_coefficient_fun(site4_coefficients_df)
site5_bleaching<-bleaching_coefficient_fun(site5_coefficients_df)
site6_bleaching<-bleaching_coefficient_fun(site6_coefficients_df)

#COTs
COTs_coefficient_fun<-function(input_df){
  
  input_df$major_events <- rownames(input_df)
  rownames(input_df) <- NULL
  bleaching_coefficient<-input_df[3, ]
  
}

site1_COTs<-COTs_coefficient_fun(site1_coefficients_df)
site2_COTs<-COTs_coefficient_fun(site2_coefficients_df)
site3_COTs<-COTs_coefficient_fun(site3_coefficients_df)
site4_COTs<-COTs_coefficient_fun(site4_coefficients_df)
site5_COTs<-COTs_coefficient_fun(site5_coefficients_df)
site6_COTs<-COTs_coefficient_fun(site6_coefficients_df)

#cyclone
cyclone_coefficient_fun<-function(input_df){
  
  input_df$major_events <- rownames(input_df)
  rownames(input_df) <- NULL
  bleaching_coefficient<-input_df[3, ]
  
}

site1_cyclone<-cyclone_coefficient_fun(site1_coefficients_df)
site2_cyclone<-cyclone_coefficient_fun(site2_coefficients_df)
site3_cyclone<-cyclone_coefficient_fun(site3_coefficients_df)
site4_cyclone<-cyclone_coefficient_fun(site4_coefficients_df)
site5_cyclone<-cyclone_coefficient_fun(site5_coefficients_df)
site6_cyclone<-cyclone_coefficient_fun(site6_coefficients_df)

#combine and plot

plot_coefficients<-function(input_df1, input_df2, input_df3, input_df4, input_df5, input_df6, plot_name){
  
  all_co <- do.call(rbind, list(input_df1, input_df2,
                                input_df3, input_df4, 
                                input_df5, input_df6))
  
  plot<-ggplot(all_co, aes(x = as.factor(site), y = Estimate)) +
    geom_point() + 
    geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2) +
    labs(title = plot_name,
         x = "Site", y = "Estimate")+
    theme_minimal()+
    coord_flip()
  
  return(plot)
}

bleaching_plot<-plot_coefficients(site1_bleaching, site2_bleaching,
                  site3_bleaching, site4_bleaching, 
                  site5_bleaching, site6_bleaching, plot_name="Bleaching Coefficient")
COTs_plot<-plot_coefficients(site1_COTs, site2_COTs,
                                  site3_COTs, site4_COTs, 
                                  site5_COTs, site6_COTs, plot_name="COTs Coefficient")
cyclone_plot<-plot_coefficients(site1_cyclone, site2_cyclone,
                             site3_cyclone, site4_cyclone, 
                             site5_cyclone, site6_cyclone, plot_name="Cyclone Coefficient")

ggsave(here("figures","bleaching_coefficients.jpg"), bleaching_plot, width=8, height=5)
ggsave(here("figures","COTs_coefficients.jpg"), COTs_plot, width=8, height=5)
ggsave(here("figures","cyclone_coefficients.jpg"), cyclone_plot, width=8, height=5)

############################make plots of 7B fit for each site###########################

plot_7B_fun<-function(raw_data, model_output, title_site_string){
  
  PF_mod<-raw_data
  
  PF_mod$est<-model_output$fitted.values #adds the fitted values of the GLARMA model to the data
  
  #plots the model (black) and the species richness data (gray)
  PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
    geom_point(col="gray",aes(y=annual_count),size=3) + 
    geom_point(col="black",aes(y=est),size=3) +
    geom_line(col="black",aes(y=est)) +
    labs(x="Year", y="Parrot Fish Count", title=title_site_string) +
    ylim(0,400) + xlim(2005,2023)+
    theme_bw()
  
  return(PF_plot_mod)
  
}

Site1_Model7B<-plot_7B_fun(PF1, site1_7B_output, "Site 1")
ggsave(here("figures","Site1_Model7B.jpg"),Site1_Model7B, dpi=500, width=6, height=5, unit="in")

Site2_Model7B<-plot_7B_fun(PF2, site2_7B_output, "Site 2")
ggsave(here("figures","Site2_Model7B.jpg"),Site2_Model7B, dpi=500, width=6, height=5, unit="in")

Site3_Model7B<-plot_7B_fun(PF3, site3_7B_output, "Site 3")
ggsave(here("figures","Site3_Model7B.jpg"),Site3_Model7B, dpi=500, width=6, height=5, unit="in")

Site4_Model7B<-plot_7B_fun(PF4, site4_7B_output, "Site 4")
ggsave(here("figures","Site4_Model7B.jpg"),Site4_Model7B, dpi=500, width=6, height=5, unit="in")

Site5_Model7B<-plot_7B_fun(PF5, site5_7B_output, "Site 5")
ggsave(here("figures","Site5_Model7B.jpg"),Site5_Model7B, dpi=500, width=6, height=5, unit="in")

Site6_Model7B<-plot_7B_fun(PF6, site6_7B_output, "Site 6")
ggsave(here("figures","Site6_Model7B.jpg"),Site6_Model7B, dpi=500, width=6, height=5, unit="in")

all_7B_plots<-grid.arrange(Site1_Model7B, Site2_Model7B, Site3_Model7B,
               Site4_Model7B, Site5_Model7B, Site6_Model7B,
               nrow = 2, ncol = 3)
ggsave(here("figures","all_Model7B.jpg"),all_7B_plots, dpi=500, width=12, height=8, unit="in")


# 
# 
# #Manually
# ############################################
# #Change: the intercept should be 1 not 0
# PF<-read.csv(here("data","common_parrotfish_data.csv"))
# PF$intercept<-1
# 
# ############################################
# 
# # Y.SR<-SR$S # makes the response variable only the species richness (constant across models)
# Y.PF<-PF$annual_count
# 
# # X.1.SR<-SR %>% select(intercept, dist) #makes model 1's explanatory variables of the intercept (1)
# X.1.PF<-PF |> select(intercept, major_event_crown_of_thorns, major_event_bleaching, major_event_cylcone) # makes a model with crown of thorns event and intercept (1)
# X.2.PF<-PF |> select(intercept, major_event_bleaching) # makes a model with crown of thorns event and intercept (1)
# 
# #Makes into a matrix
# # X.1.SR<-as.matrix(X.1.SR) 
# X.1.PF<-as.matrix(X.1.PF)
# X.2.PF<-as.matrix(X.2.PF)
# 
# #SR model 1
# # model with fire events included
# # glarmamod.1.SR <- glarma(Y.SR, X.1.SR, phiLags = c(1), type = "Poi", method = "FS",
#                       # residuals = "Pearson", maxit = 100, grad = 1e-6)
# glarmamod.1A.PF <- glarma(Y.PF, X.1.PF, phiLags = c(1), type = "Poi", method = "FS",
#                          residuals = "Pearson", maxit = 100, grad = 1e-6)
# glarmamod.1B.PF <- glarma(Y.PF, X.1.PF, phiLags = c(2), type = "Poi", method = "FS",
#                          residuals = "Pearson", maxit = 100, grad = 1e-6)
# glarmamod.2A.PF <- glarma(Y.PF, X.2.PF, phiLags = c(1), type = "Poi", method = "FS",
#                           residuals = "Pearson", maxit = 100, grad = 1e-6)
# glarmamod.2B.PF <- glarma(Y.PF, X.2.PF, phiLags = c(2), type = "Poi", method = "FS",
#                           residuals = "Pearson", maxit = 100, grad = 1e-6)
# 
# 
# #summary statistics just like getting information out of linear regression
# # summary(glarmamod.1.SR)
# glarmamod.1A.PF.summary<-summary(glarmamod.1A.PF)
# glarmamod.1A.PF
# beta_coefficients <- glarmamod.1A.PF.summary$coefficients1
# 
# 
# glarmamod.1B.PF.summary<-summary(glarmamod.1B.PF)
# 
# glarmamod.2A.PF.summary<-summary(glarmamod.2A.PF)
# 
# glarmamod.2B.PF.summary<-summary(glarmamod.2B.PF)
# 
# #### Modified PF plot for Moorea
# PF_mod<-PF
# 
# PF_mod$est<-glarmamod.1B.PF$fitted.values #adds the fitted values of the GLARMA model to the data
# 
# #plots the model (black) and the species richness data (gray)
# PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
#   geom_point(col="gray",aes(y=annual_count),size=3) + 
#   geom_point(col="black",aes(y=est),size=3) +
#   geom_line(col="black",aes(y=est)) +
#   labs(x="Year", y="Parrot Fish Count") +
#   ylim(0,400) + xlim(2005,2023)+
#   theme_bw()
# 
# PF_plot_mod
# ggsave(here("plot","Moorea_PF_Glarma_m1.jpg"),PF_plot_mod, dpi=500, width=6, height=3, unit="in")
# 
# 
# #site 2 model 4 figure
# PF_mod<-PF2
# 
# PF_mod$est<-glarmamod.4.PF2$fitted.values #adds the fitted values of the GLARMA model to the data
# 
# #plots the model (black) and the species richness data (gray)
# PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
#   geom_point(col="gray",aes(y=annual_count),size=3) + 
#   geom_point(col="black",aes(y=est),size=3) +
#   geom_line(col="black",aes(y=est)) +
#   labs(x="Year", y="Parrot Fish Count") +
#   ylim(0,400) + xlim(2005,2023)+
#   theme_bw()
# 
# PF_plot_mod
# 
# #site 3 model 4 figure
# PF_mod<-PF3
# 
# PF_mod$est<-glarmamod.4.PF3$fitted.values #adds the fitted values of the GLARMA model to the data
# 
# #plots the model (black) and the species richness data (gray)
# PF_plot_mod<-ggplot(PF_mod, aes(x=year)) + 
#   geom_point(col="gray",aes(y=annual_count),size=3) + 
#   geom_point(col="black",aes(y=est),size=3) +
#   geom_line(col="black",aes(y=est)) +
#   labs(x="Year", y="Parrot Fish Count") +
#   ylim(0,400) + xlim(2005,2023)+
#   theme_bw()
# 
# PF_plot_mod



