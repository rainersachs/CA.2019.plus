source('MonteCarlo.R')
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #This centers plot titles for ggplots

##Mix1,3
MC_results_4para_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_4para, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.01))
MIXDER_4para_cov_H250Si260 = MC_results_4para_cov_H250Si260[[1]]
MC_results_2paraTE_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_2paraTE, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005))
MIXDER_2paraTE_cov_H250Si260 = MC_results_2paraTE_cov_H250Si260[[1]]

MC_results_2paraTE_var_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_2paraTE, n =500, r = c(0.6, 0.4), cov = F,d = seq(0, 1.1, 0.005))
MIXDER_2paraTE_var_H250Si260 = MC_results_2paraTE_var_H250Si260[[1]]
MC_results_2para_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_2para, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005))
MIXDER_2para_cov_H250Si260 = MC_results_2para_cov_H250Si260[[1]]

MC_results_2para_var_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_2para, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005), cov = F)
MIXDER_2para_var_H250Si260 = MC_results_2para_var_H250Si260[[1]]

MC_results_4para_cov_full = monte_carlo(ions = c("Fe300", "Fe450","Fe600", "Ti300", "Si170", "Si260", "O77", "O350"), para = MC_4para, n = 500) #This outputs a list of MIXDER dataframe and a vector of the indexes at which there were convergence issues. 
MIXDER_4para_cov_full = MC_results_4para_cov_full[[1]] #This is the dataframe that will be passed into the graph

MC_results_2paraTE_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_2paraTE, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005))
MIXDER_2paraTE_cov_H250Si260 = MC_results_2paraTE_cov_H250Si260[[1]]

MC_results_NEW1_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_NEW1, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005))
MIXDER_NEW1_cov_H250Si260 = MC_results_NEW1_cov_H250Si260[[1]]
MC_results_NEW1_var_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_NEW1, n =500, r = c(0.6, 0.4), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW1_var_H250Si260 = MC_results_NEW1_var_H250Si260[[1]]
MC_results_NEW2_cov_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_NEW2, n =500, r = c(0.6, 0.4), d = seq(0, 1.1, 0.005))
MIXDER_NEW2_cov_H250Si260 = MC_results_NEW2_cov_H250Si260[[1]]
MC_results_NEW2_var_H250Si260 = monte_carlo(ions = c("H250", "Si260"), para = MC_NEW2, n =500, r = c(0.6, 0.4), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW2_var_H250Si260 = MC_results_NEW2_var_H250Si260[[1]]

##Mix6,7,8
MC_results_2paraTE_cov_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_2paraTE, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385), d = seq(0, 0.7, 0.005))
MIXDER_2paraTE_cov_mix6 = MC_results_2paraTE_cov_mix6[[1]]
MC_results_2paraTE_var_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_2paraTE, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385),cov = F, d = seq(0, 0.7, 0.005))
MIXDER_2paraTE_var_mix6 = MC_results_2paraTE_var_mix6[[1]]

MC_results_2para_cov_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_2para, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385), d = seq(0, 0.7, 0.005))
MIXDER_2para_cov_mix6 = MC_results_2para_cov_mix6[[1]]
MC_results_2para_var_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_2para, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385),cov = F, d = seq(0, 0.7, 0.005))
MIXDER_2para_var_mix6 = MC_results_2para_var_mix6[[1]]

MC_results_NEW1_cov_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_NEW1, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385), d = seq(0, 0.7, 0.005))
MIXDER_NEW1_cov_mix6 = MC_results_NEW1_cov_mix6[[1]]
MC_results_NEW1_var_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_NEW1, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385),cov = F, d = seq(0, 0.7, 0.005))
MIXDER_NEW1_var_mix6 = MC_results_NEW1_var_mix6[[1]]

MC_results_NEW2_cov_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_NEW2, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385), d = seq(0, 0.7, 0.005))
MIXDER_NEW2_cov_mix6 = MC_results_NEW2_cov_mix6[[1]]
MC_results_NEW2_var_mix6 = monte_carlo(ions = c("H250", "He250", "O350", "Ti300"), para = MC_NEW2, n =500, r = c(0.680769231, 0.202884615, 0.069230769, 0.047115385),cov = F, d = seq(0, 0.7, 0.005))
MIXDER_NEW2_var_mix6 = MC_results_NEW2_var_mix6[[1]]

##Mix9,10,11
MC_results_NEW1_cov_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), d = seq(0, 1.1, 0.005))
MIXDER_NEW1_cov_mix9 = MC_results_NEW1_cov_mix9[[1]]
MC_results_NEW1_var_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW1_var_mix9 = MC_results_NEW1_var_mix9[[1]]

MC_results_NEW2_cov_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW2, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), d = seq(0, 1.1, 0.005))
MIXDER_NEW2_cov_mix9 = MC_results_NEW2_cov_mix9[[1]]
MC_results_NEW2_var_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW2, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW2_var_mix9 = MC_results_NEW2_var_mix9[[1]]

##Mix12,13
MC_results_NEW1_cov_mix12 = monte_carlo(ions = c("Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(1/3, 1/3, 1/3), d = seq(0, 0.7, 0.005))
MIXDER_NEW1_cov_mix12 = MC_results_NEW1_cov_mix12[[1]]
MC_results_NEW1_var_mix12 = monte_carlo(ions = c("Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(1/3, 1/3, 1/3), cov = F, d = seq(0, 0.7, 0.005))
MIXDER_NEW1_var_mix12 = MC_results_NEW1_var_mix12[[1]]



##################################################################
#Selecting Specific Data by Using Function subset().
#Example: Subset: d >= 0.05, CA > 0.01, Z = 22 and Comment section includes "2018"
subset(HZE_data, d >= 0.05 & CA > 0.01 & Z == 22 & grepl("2018", Comment))
#This returns a sub dataset with all the rows that satisfy the restrictions put in, which you can put in to the function "IDER_graph" as the only input. See example below.
IDER_graph(subset(HZE_data, d >= 0.05 & CA > 0.01 & Z == 22 & grepl("2018", Comment)))
##################################################################

#Graphing func_model
func_4para_kap0 = function(x){
  return(func_4para(d = 0.05, L = 99, Z.b = 689.4011091, eta0 = 1.997681e-05, eta1 = 1.902494e-03, sig0 = 7.112208e+00, kap0 = x) - BG_CA)
}
ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun = func_4para_kap0) + xlim(-300, 100)

func_4para_sig0 = function(x){
  return(func_4para(d = 0.05, L = 99, Z.b = 689.4011091, eta0 = 1.997681e-05, eta1 = 1.902494e-03, sig0 = x, kap0 = 1.951239e+03) - BG_CA)
}
samples = rmvnorm(500, parameters_4para$value, sigma = sig_4para)
sample_kap0 = samples[,4]
sample_eta0 = samples[,1]
ggplot(data = data.frame(x = 0, kap0 = sample_kap0), mapping = aes(x = x)) + stat_function(fun = func_4para_kap0) + xlim(-300, 300) + 
  geom_point(aes(x = kap0, y = 0), color = "red")

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun = func_4para_sig0) + xlim(-3.112208e+00, 17.112208e+00)


#Plots

##Model Free Plot in Intro
p_intro_all = ggplot(main_df %>% filter(d > 0) %>% filter(d <= 0.1), aes(x = 100*d, y = 100*CA)) + 
  geom_point(aes(col = ion,size = 1/sqrt(error))) +
  geom_smooth(method = "lm", size = 1.2, se = F, mapping = aes(weight = (1/error^2), linetype = paste("0-10 cGy\nIntercept =", round((lm(CA~d, data = main_df %>% filter(d > 0) %>% filter(d <=0.1), weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2)))) +
  geom_smooth(method = "lm", size = 1.2, se = F, mapping = aes(weight = (1/error^2), linetype = paste("0-50 cGy\nIntercept =", round((lm(CA~d, data = main_df %>% filter(d > 0) %>% filter(d <=0.5), weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2))), data = main_df %>% filter(d > 0) %>% filter(d <= 0.5)) + 
  geom_errorbar(aes(x = 0, ymin = 100*(BG_CA - BG_error), ymax = 100*(BG_CA + BG_error)), width = 0.5, color = "red") +
  geom_point(aes(x = 0, y = 100*BG_CA, color = paste("Background\n CA =", round(100*BG_CA,2))), size = 2.5) +
  
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 3)) +
  scale_size_continuous("Weights",range = c(1.7, 4)) +
  scale_linetype_manual("Inverse Variance LS", values = c("solid", "dotdash"),
                        breaks = c(paste("0-10 cGy\nIntercept =", round((lm(CA~d, data = HZE_data %>% filter(d <=0.1), weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2)), 
                                                  paste("0-50 cGy\nIntercept =", round((lm(CA~d, data = HZE_data %>% filter(d <=0.5), weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2))))+
  labs(x = "Dose d (cGy)", y = "CA (%)", title = "Visual Evidence for NTE", subtitle = "New combined Data")#axis labels and plot title
p_intro_all

ggsave("Intro_combined_New.eps",device = "eps", width = 10, height = 7)

squeezed_df = HZE_data %>% filter((d <= 0.5)&(d > 0)) %>% group_by(d) %>% summarise(p = sum(X.T)/sum(At.Risk), CA = p, error = sqrt((p*(1-p))/(sum(At.Risk))))
p_intro_squeezed = ggplot(squeezed_df, aes(x = 100*d, y = 100*CA)) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, mapping = aes(weight = (1/error^2), color = paste("Weighted LS\n Intercept =", round((lm(CA~d, data = squeezed_df, weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2)))) +
  geom_errorbar(aes(ymin = 100*(CA - error), ymax = 100*(CA + error)), width = 1) + #Errorbars for each point from original data
  geom_errorbar(aes(x = 0, ymin = 100*(BG_CA - BG_error), ymax = 100*(BG_CA + BG_error)), width = 1) +
  geom_point(aes(x = 0, y = 100*BG_CA, color = paste("Background\n CA =", round(100*BG_CA,2))), size = 2.5) +
  scale_color_manual("Values", breaks = c(paste("Background\n CA =", round(100*BG_CA,2)), 
                                          paste("Weighted LS\n Intercept =", round((lm(CA~d, data = squeezed_df, weights = 1/(error^2)) %>% coef %>% as.numeric)[1]*100, 2))), values= c("red2", "blue")) + #legend for Ions
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
      panel.grid.major.y = element_line(colour = "grey80"),
      panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line(),
      plot.title = element_text(size = 20, face = "bold"), 
      axis.title = element_text(size = 15, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  scale_x_continuous(expand = c(0, 0))+
  labs(x = "Dosage (cGy)", y = "Chromosomal Aberrations (%)", title = "Visual Evidence for NTE", subtitle = "Old Combined Data Grouped by Dosage") #axis labels and plot title

p_intro_squeezed
#ggsave("Old_Combined.eps",device = "eps", width = 10, height = 7)

###################################################################################################################################################################

##Dose Effect Relationship Plots
DER_df = HZE_data %>% filter(ion %in% c("Fe300", "Fe450","Fe600", "Ti300", "Si170", "Si260","O77", "O350"))
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0) & (d <= 0.5)) #Taking out 0 dosage points and cut at 0.5
MIXDER = MIXDER_4para_cov_full



###Individual DER Plot with ribbons (Si170 as example since it has the most data points)
IDER_plot = function(data = DER_df, ion, d_cap = 0.5, var_ribbon = T, para = MC_4para){
  name = ion
  ion_data = subset(x = data, ion == name & d <= d_cap)
  MC_results_cov = monte_carlo(ions = ion, para = para, n = 500, IDER = T) 
  IDER_cov = MC_results_cov[[1]]
  
  p = ggplot(data = IDER_cov, aes(x = d * 100))
  if (var_ribbon){
    MC_results_var = monte_carlo(ions = ion, para = para, n = 500, IDER = T, cov = F) 
    IDER_var = MC_results_var[[1]]
    p = p + geom_ribbon(data = IDER_var, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper, fill = "Variance"), alpha = 0.3) #MonteCarlo Ribbons without cov (blue ribbons)
  }
  p = p + 
    geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper, fill = "Covariance")) +
    geom_line(aes(y = 100 * IDER), size = 1) +  #IDER (colored solid lines)
    geom_point(data = ion_data, aes(x= 100*d, y = 100*CA), size = 2) + #Actual Data for these for types of ions (colored points)
    geom_errorbar(data = ion_data, aes(x = 100*d, ymin = 100*(CA - error), ymax = 100*(CA + error)), width = 0.5) + #Errorbars for each point from original data
    scale_fill_manual("MonteCarlo CI", breaks = c("Covariance", "Variance"), values=c("yellow", "blue")) + #legend for cov vs var
    
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
          panel.grid.major.y = element_line(colour = "grey80"),
          panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
          panel.grid.major.x = element_blank(),
          axis.ticks = element_line(),
          plot.title = element_text(size = 20, face = "bold"), 
          axis.title = element_text(size = 15, face = "bold"),
          legend.title = element_text(size = 12, face = "bold")
    ) +
    labs(x = "100*Dosage", y = "100*Chromosomal Aberrations", title = "Individual Dose Effect Relationship", subtitle = ion) #axis labels and plot title
  return(p)
}

p_Si170 = IDER_plot(ion = "Si170", var_ribbon = F)
p_Si170
ggsave("Si170IDER.png", width = 10, height = 7)

p_Si260 = IDER_plot(ion = "Si260")
p_Si260
ggsave("Si260IDER.png", width = 10, height = 7)

p_Fe600 = IDER_plot(ion = "Fe600")
p_Fe600
ggsave("Fe600IDER.png", width = 10, height = 7)

p_Fe450 = IDER_plot(ion = "Fe450")
p_Fe450
ggsave("Fe450IDER.png", width = 10, height = 7)

p_Fe300 = IDER_plot(ion = "Fe300")
p_Fe300
ggsave("Fe300IDER.png", width = 10, height = 7)

p_O77 = IDER_plot(ion = "O77")
p_O77
ggsave("O77IDER.png", width = 10, height = 7)
p_O350 = IDER_plot(ion = "O350")
p_O350
ggsave("O350IDER.png", width = 10, height = 7)

library(gridExtra)
grid.arrange(p_Fe600, p_Fe450, p_Fe300, nrow = 2)

grid.arrange(p_Si170, p_Si260, nrow = 1)

##MIXDER Plot, using the same ions.
names(MIXDER)[2] <- "mixDER"
p_mix = ggplot(data = MIXDER, aes(x = d * 100)) + #This example consists a mixture of 4 ions: "Fe600", "Si170", "O77", "O350". 
  geom_line(aes(y = 100 * Fe300, color = "Fe300"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe450, color = "Fe450"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe600, color = "Fe600"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti300, color = "Ti300"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O350, color = "O350"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O77, color = "O77"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si170, color = "Si170"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si170, color = "Si260"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100*mixDER, color = "MIXDER"), linetype = "solid", size = 1.2) + #MIXDER (black solid line)
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Actual Data for these for types of ions (colored points)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA")) + #Simple Effect Additivity (black dashed line)
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  
  scale_color_manual("Ions", breaks = c("MIXDER", "Fe300", "Fe450","Fe600",  "Ti300", "Si170", "Si260", "O77", "O350"), values=c("cyan2",  "cornflowerblue", "blue3", "black", "purple", "red", "orange","tan4", "darkgreen")) + #legend for Ions
  guides(alpha=FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
        ) +
  
  labs(x = "100*Dosage", y = "100*Chromosomal Aberrations", title = "Dose Effect Relationships", subtitle = "4 Parameter Model") #axis labels and plot title
p_mix
ggsave("DoseEffectRelationships.png", width = 10, height = 7)

##Zoom in
p_mix_15 = p_mix + xlim(c(0, 15)) + ylim(c(0, 3.5))
p_mix_15
ggsave("DoseEffectRelationships15.png", width = 10, height = 7)

#================================ Mixture (IEA and SEA) Plots with Data ==================================#
#mixture 1,3
MIXDER = MIXDER_NEW2_cov_H250Si260
names(MIXDER)[2] <- "mixDER"
DER_df = main_df %>% filter(ion %in% c("H250", "Si260"))
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0)) #Taking out 0 dosage points and cut at 0.5

p_mix = ggplot(data = MIXDER, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * H250, color = "H250"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si260, color = "Si260"), linetype = "dashed", size = 1) +

  geom_ribbon(data = MIXDER_NEW2_var_H250Si260, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Actual Data for these for types of ions (colored points)
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  geom_point(aes(x = 100, y = 5.6379822), size = 4) + geom_errorbar(aes(x = 100, ymin = 5.6379822-0.8884449, ymax = 5.6379822 + 0.8884449)) +
  geom_point(aes(x = 50, y = 2.417795), size = 4) + geom_errorbar(aes(x = 50, ymin = 2.417795 - 0.4776775, ymax = 2.417795 + 0.4776775)) +

  scale_color_manual("Ions", breaks = c("H250","Si260"), values=c("blue3", "red")) + #legend for Ions
  guides(alpha=FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
  ) +
  
  labs(x = "Dosage (cGy)", y = "Chromosomal Aberrations (%)", title = "IDER and MixDER for H250 + Si260", subtitle = "Model = NEW TE, r = (0.6, 0.4)") #axis labels and plot title
p_mix
ggsave("mix13_NEWTE.eps",device = cairo_ps, width = 10, height = 7)

ggsave("mix13_NEWTE.png",width = 10, height = 7)

#mixture 678
MIXDER = MIXDER_NEW1_cov_mix6
names(MIXDER)[2] <- "mixDER"
DER_df = main_df %>% filter(ion %in% c("H250", "He250", "O350", "Ti300"))
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0)) #Taking out 0 dosage points and cut at 0.5

p_mix = ggplot(data = MIXDER, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * H250, color = "H250"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * He250, color = "He250"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O350, color = "O350"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti300, color = "Ti300"), linetype = "dashed", size = 1) +
  
  geom_ribbon(data = MIXDER_NEW1_var_mix6, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Actual Data for these for types of ions (colored points)
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  geom_point(aes(x = 15.6, y = 0.3976143), size = 4) + geom_errorbar(aes(x = 15.6, ymin = 0.3976143-0.1984115, ymax = 0.3976143 + 0.1984115)) +
  geom_point(aes(x = 31.2, y = 1.0805501), size = 4) + geom_errorbar(aes(x = 31.2, ymin = 1.0805501-0.3240331, ymax = 1.0805501 + 0.3240331)) +
  geom_point(aes(x = 62.4, y = 2.3300971), size = 4) + geom_errorbar(aes(x = 62.4, ymin = 2.3300971-0.4700551, ymax = 2.3300971 + 0.4700551)) +

  scale_color_manual("Ions", breaks = c("H250", "He250", "O350", "Ti300"), values=c("blue3", "red", "purple", "darkgreen")) + #legend for Ions
  guides(alpha=FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
  ) +
  
  labs(x = "Dosage (cGy)", y = "Chromosomal Aberrations (%)", title = "IDER and MixDER for H250 + He250 + O350 + Ti300", subtitle = "Model = NEW NTE, r = (0.68, 0.20, 0.07, 0.05)") #axis labels and plot title
p_mix
ggsave("mix678_NEWNTE.eps",device = cairo_ps, width = 10, height = 7)

#mixture 9,10,11
MIXDER = MIXDER_NEW1_cov_mix9
names(MIXDER)[2] <- "mixDER"
DER_df = main_df %>% filter(ion %in% c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"))
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0)) #Taking out 0 dosage points and cut at 0.5

p_mix = ggplot(data = MIXDER, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * H250, color = "H250"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * He250, color = "He228"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O350, color = "O350"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si260, color = "Si260"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti1000, color = "Ti1000"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe600, color = "Fe600"), linetype = "dashed", size = 1) +
  
  geom_ribbon(data = MIXDER_NEW1_var_mix9, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Actual Data for these for types of ions (colored points)
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  geom_point(aes(x = 100, y = 3.3851276), size = 4) + geom_errorbar(aes(x = 100, ymin = 3.3851276-0.4260221, ymax = 3.3851276 + 0.4260221)) +
  geom_point(aes(x = 50, y = 1.5483182), size = 4) + geom_errorbar(aes(x = 50, ymin = 1.5483182-0.285281, ymax = 1.5483182 + 0.285281)) +
  geom_point(aes(x = 25, y = 1.2321341), size = 4) + geom_errorbar(aes(x = 25, ymin = 1.2321341-0.244904, ymax = 1.2321341 + 0.244904)) +
  
  scale_color_manual("Ions", breaks = c("H250", "He228", "O350", "Si260", "Ti1000", "Fe600"), values=c("blue3", "red", "purple", "darkgreen", "cyan", "orange", "steelblue")) + #legend for Ions
  guides(alpha=FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
  ) +
  
  labs(x = "Dosage (cGy)", y = "Chromosomal Aberrations (%)", title = "IDER and MixDER for H250 + He228 + O350 + Si260 + Ti1000 + Fe600", subtitle = "Model = NEW NTE, r = (0.6, 0.2, 0.1, 0.025, 0.025, 0.05)") #axis labels and plot title
p_mix
ggsave("mix91011_NEWNTE_var.eps", device = cairo_ps, width = 10, height = 7)

ggsave("mix91011_NEWTE_var.png",width = 10, height = 7)

#mixture 12,13
MIXDER = MIXDER_NEW1_cov_mix12
names(MIXDER)[2] <- "mixDER"
DER_df = main_df %>% filter(ion %in% c("Si260", "Ti1000", "Fe600"))
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0 & d < 0.8)) #Taking out 0 dosage points and cut at 0.5

p_mix = ggplot(data = MIXDER, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * Si260, color = "Si260"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti1000, color = "Ti1000"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe600, color = "Fe600"), linetype = "dashed", size = 1) +
  
  geom_ribbon(data = MIXDER_NEW1_var_mix12, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Actual Data for these for types of ions (colored points)
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  geom_point(aes(x = 30, y = 3.1976744), size = 4) + geom_errorbar(aes(x = 30, ymin = 3.1976744-0.5476715, ymax = 3.1976744 + 0.5476715)) +
  geom_point(aes(x = 60, y = 5.6218058), size = 4) + geom_errorbar(aes(x = 60, ymin = 5.6218058-0.9507245, ymax = 5.6218058 + 0.9507245)) +

  scale_color_manual("Ions", breaks = c("Si260", "Ti1000", "Fe600"), values=c("blue3", "red", "darkgreen")) + #legend for Ions
  guides(alpha=FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
  ) +
  
  labs(x = "Dosage (cGy)", y = "Chromosomal Aberrations (%)", title = "IDER and MixDER for Si260 + Ti1000 + Fe600", subtitle = "Model = NEW1, r = (1/3, 1/3, 1/3)") #axis labels and plot title
p_mix

ggsave("mix1213_NEW1_var.png",width = 10, height = 7)
###########################################################################
#The SLI Plots
##Linear Model
DER_SLI_df = swift_light_df %>% mutate(CA = CA - BG_CA) %>% select(d, CA, ion, error)
slope = coef(model_sli_linear1) %>% as.numeric()

ggplot(DER_SLI_df, aes(x = 100*d)) +
  
  geom_abline(slope = slope) + 
  geom_point(aes(y = 100*CA*(CA > 0), color = ion, size = 1/sqrt(error), alpha = 1/sqrt(error))) + #Making negative values 0
  
  scale_color_manual("Ions", breaks = c("H250", "He250"), values = c("blue3", "red")) +
  scale_size_continuous("Accuracy",range = c(1.5,4)) + 
  scale_alpha_continuous(range = c(0.3, 1)) +
  guides(alpha=FALSE) +
  
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),  #Some random tweeks of the ggplot theme. Need someone more artistic to do this part :)
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey80"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 12, face = "bold")
  ) +
  lims(x = c(0,60)) +
  labs(x = "100*Dosage", y = "100*Chromosomal Aberrations", title = "Dose Effect Relationships", subtitle = "Swift Light Ions Linear Model")
ggsave("SLILinearDER.png", width = 10, height = 7)


ggplot(HZE_data, aes(x = d, y = CA)) +
  geom_point(aes(color = ion))

ggplot(HZE_data %>% filter(ion == "Fe600") %>% filter(d <= 0.4) %>% filter(MeH_. != "P-24"), 
       aes(x = d, y = CA)) + 
  geom_point(aes(color = MeH_., size = 1/sqrt(error))) + 
  scale_size_continuous("Accuracy",range = c(1.7, 3)) + 
  geom_smooth(method = "lm", se = F, aes(color = MeH_.))

ggplot(HZE_data %>% filter(ion == "O77") %>% filter(d <= 0.1), 
       aes(x = d, y = CA)) + 
  geom_point(aes(color = MeH_., size = 1/sqrt(error))) + 
  scale_size_continuous("Accuracy",range = c(1.7, 3)) + 
  geom_smooth(method = "lm", se = F, aes(color = MeH_.))
