###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

#Run for the first time only
#install.packages(librarian)

librarian::shelf(ggplot2,dplyr,rgl)

# library(ggplot2)
# library(dplyr)

#Data:
dat <- read.csv("~/GitHub/metabolism-river-networks/data/cum_resp_YRB_data_0725_2022_entropy.csv", 
                stringsAsFactors=TRUE)
#Plot:
p <- ggplot(dat,aes(AreSqKM,cum_totco2g_m2_day))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
p

#Land-cover plot (2016)
thd <- 70

dat$lnd_cat <- as.factor(with(dat,ifelse(TOT_urban16>thd,"Urban",
                                         ifelse(TOT_forest16>thd,"Forest",
                                                ifelse(TOT_wetland16>thd,"Wetland",
                                                       ifelse(TOT_agrc16>thd,"Agriculture",
                                                              ifelse(TOT_shrub16>thd,"Shrubland",
                                                                     "Mixed covers")))))))

dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(TOT_BASIN_AREA,cum_totco2g_m2_day,color=lnd_cat))+
  xlab("Watershed Area")+
  ylab("Cummulative respiration rates")+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~lnd_cat)+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12, hjust = 0),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size=14))

dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(TOT_BASIN_AREA,cum_totco2g_m2_day, color = h_rel_3))+
  xlab("Watershed Area")+
  ylab("Cummulative respiration rates")+
  # geom_point(alpha = 0.05)+
  geom_point()+
  geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
  scale_x_log10()+
  scale_y_log10()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size=14))


p <- plot3d( 
  x=dat$`tot_other_3`, y=data$`tot_ntrl_3`, z=data$`h_rel_3`, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")
p