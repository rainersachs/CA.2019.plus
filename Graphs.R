source('MonteCarlo.R')
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #This centers plot titles for ggplots

#================================ Monte Carlo Dataset ==================================#
#NEW1: 4 parameter NTE-also model
#NEW2: 3 parameter TE-only model
#The way to input the model type into monte_carlo is through "para", a collection of lists defined in the previous MonteCarlo.R file, each uniquely corresponding to a model.

##Here is an example of Mix Group number 9,10,11
MC_results_NEW1_cov_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), d = seq(0, 1.1, 0.005))
MIXDER_NEW1_cov_mix9 = MC_results_NEW1_cov_mix9[[1]] #The MC_results_NEW1_cov_mix9 object from monte_carlo function is a list of two elements: the first is the data frame, second is a vector of indices of non-convergence
MC_results_NEW1_var_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW1, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW1_var_mix9 = MC_results_NEW1_var_mix9[[1]]

MC_results_NEW2_cov_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW2, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), d = seq(0, 1.1, 0.005))
MIXDER_NEW2_cov_mix9 = MC_results_NEW2_cov_mix9[[1]]
MC_results_NEW2_var_mix9 = monte_carlo(ions = c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600"), para = MC_NEW2, n =500, r = c(0.6, 0.2, 0.1, 0.025, 0.025, 0.05), cov = F, d = seq(0, 1.1, 0.005))
MIXDER_NEW2_var_mix9 = MC_results_NEW2_var_mix9[[1]]


#================================ Mixture Plot ==================================#

#mixture 9,10,11
MIXDER_df = MIXDER_NEW1_cov_mix9 #The main data set we are using is the var-cov version, and the variance only version is only used as a superposed ribbon
colnames(MIXDER_df)[2] = "mixDER"
DER_df = main_df %>% filter(ion %in% c("H250", "He250", "O350", "Si260", "Ti1000", "Fe600")) #For individual rows (data points)
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0)) #Taking out 0 dosage points

#Mixture Example 1: 
#Here is an example without ribbons (commented out), with 1-ion DERs, without error bars on individual data points except the mixture data.
p_mix = ggplot(data = MIXDER_df, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * H250, color = "H250"), linetype = "dashed", size = 1) + 
  geom_line(aes(y = 100 * He250, color = "He228"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O350, color = "O350"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si260, color = "Si260"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti1000, color = "Ti1000"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe600, color = "Fe600"), linetype = "dashed", size = 1) +
  #The following two lines are ribbons. To make the grabbing easier in Illustrator, it is best to have a plot without the ribbon and a plot with.
  #geom_ribbon(data = MIXDER_NEW1_var_mix9, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  #geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error))) + #Data Points, all solid colors now, no more transparency.
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_size_continuous("Accuracy",range = c(1.5,4)) +
  geom_point(aes(x = 100, y = 3.3851276), size = 4) + geom_errorbar(aes(x = 100, ymin = 3.3851276-0.4260221, ymax = 3.3851276 + 0.4260221), size = 1) +
  geom_point(aes(x = 50, y = 1.5483182), size = 4) + geom_errorbar(aes(x = 50, ymin = 1.5483182-0.285281, ymax = 1.5483182 + 0.285281), size = 1) +
  geom_point(aes(x = 25, y = 1.2321341), size = 4) + geom_errorbar(aes(x = 25, ymin = 1.2321341-0.244904, ymax = 1.2321341 + 0.244904), size = 1) +
  
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
ggsave("mix91011_NEWNTE_var.eps", device = cairo_ps, width = 20, height = 14) #This line outputs the EPS file. After testing, what seemed to help was to increase the resolution by increasing width and height (these are in inches). Do not make them too big. It will crash the computer.

#Mixture Example 2: 
#Here is an example with ribbons, with 1-ion DERs, with error bars on individual data points except the mixture data.
p_mix = ggplot(data = MIXDER_df, aes(x = d * 100)) + #This example consists a mixture of 2 ions: H250 and Si260
  geom_line(aes(y = 100 * H250, color = "H250"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * He250, color = "He228"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * O350, color = "O350"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Si260, color = "Si260"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Ti1000, color = "Ti1000"), linetype = "dashed", size = 1) +
  geom_line(aes(y = 100 * Fe600, color = "Fe600"), linetype = "dashed", size = 1) +
  #The following two lines are ribbons. To make the grabbing easier in Illustrator, it is best to have a plot without the ribbon and a plot with.
  geom_ribbon(data = MIXDER_NEW1_var_mix9, aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * CI_lower, ymax = 100 * CI_upper), fill = "red", alpha = 0.3) + #MonteCarlo Ribbons (red ribbons)
  geom_point(data = DER_df, aes(x= 100*d, y = 100*CA, color = ion, size = 1/sqrt(error))) + geom_errorbar(data = DER_df, aes(x = 100*d, ymin = 100*(CA - error), ymax = 100*(CA + error)), size = 0.4, width = 1.2) +#Data Points, all solid colors now, no more transparency.
  geom_line(aes(y = 100*mixDER, linetype = "IEA (mixDER)"), size = 1.2) + #MIXDER (black solid line)
  geom_line(aes(y = 100*simpleeffect, linetype = "SEA"), size = 1.2) + #Simple Effect Additivity (black dashed line)
  scale_size_continuous("Accuracy",range = c(2,4)) +
  geom_point(aes(x = 100, y = 3.3851276), size = 4) + geom_errorbar(aes(x = 100, ymin = 3.3851276-0.4260221, ymax = 3.3851276 + 0.4260221), size = 1) +
  geom_point(aes(x = 50, y = 1.5483182), size = 4) + geom_errorbar(aes(x = 50, ymin = 1.5483182-0.285281, ymax = 1.5483182 + 0.285281), size = 1) +
  geom_point(aes(x = 25, y = 1.2321341), size = 4) + geom_errorbar(aes(x = 25, ymin = 1.2321341-0.244904, ymax = 1.2321341 + 0.244904), size = 1) +
  
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
ggsave("mix91011_NEWNTE_var.eps", device = cairo_ps, width = 20, height = 14)

#================================ One Ion IDER Plots ==================================#
#Here it is done with a function instead of raw code. 
DER_df = HZE_data
DER_df$CA = DER_df$CA - BG_CA #Taking out background
DER_df = filter(DER_df, (d > 0) & (d <= 0.5)) #Taking out 0 dosage points and cut at 0.5

IDER_plot = function(data = DER_df, ion, d_cap = 0.5, var_ribbon = T, para = MC_NEW1){
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

p_Si170 = IDER_plot(ion = "Si170", var_ribbon = F, para = MC_NEW2)
p_Si170
ggsave("Si170IDER.png", width = 10, height = 7)

p_Si260 = IDER_plot(ion = "Si260", var_ribbon = T, para = MC_NEW2)
p_Si260
ggsave("Si260IDER.png", width = 10, height = 7)


#================================ One Ion IDER Plots ==================================#
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

