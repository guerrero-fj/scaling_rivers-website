###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl)


#Data:

#header info
hdr <- read.csv("data/header_info_cum_resp_YRB_data_0725_2022.csv")
hdr

#values
dat <- read.csv("data/cum_resp_YRB_data_0725_2022_entropy.csv", 
                stringsAsFactors=TRUE)

#Land-cover categories (2016)
thd <- 70

dat$lnd_cat <- as.factor(with(dat,ifelse(TOT_urban16>thd,"Urban",
                                         ifelse(TOT_forest16>thd,"Forest",
                                                ifelse(TOT_wetland16>thd,"Wetland",
                                                       ifelse(TOT_agrc16>thd,"Agriculture",
                                                              ifelse(TOT_shrub16>thd,"Shrubland",
                                                                     "Mixed covers")))))))

#exploratory plots

# Slope distribution (segments)
slp.p <- ggplot(dat,aes(TOT_STREAM_SLOPE))+
  geom_vline(xintercept = 0.01425)+
  geom_density()+
  scale_x_log10()
slp.p

# # Raw slope data
# dat %>% ggplot(aes(CAT_STREAM_SLOPE, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+
#   geom_vline(xintercept = 0.01425, linetype = "dashed")+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# 
# #color scale
# colors <- ggplot_build(dat %>% ggplot(aes(CAT_STREAM_SLOPE, fill = lnd_cat))+
#                geom_density(alpha = 0.5)+
#                geom_vline(xintercept = 0.01425, linetype = "dashed")+
#                scale_x_log10()+
#                guides(fill = guide_legend(title="Land use type")))
# unique(colors$data[[1]]$fill)

# Raw slope data (color scale)
dat %>% ggplot(aes(TOT_STREAM_SLOPE, fill = lnd_cat))+
  geom_density(alpha = 0.5)+#scale densities
  geom_vline(xintercept = 0.01425, linetype = "dashed")+
  scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3"))+
  scale_x_log10()+
  guides(fill = guide_legend(title="Land use type"))


# Area distribution (segments)
are.p <- ggplot(dat,aes(CAT_BASIN_AREA, fill = lnd_cat))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
  scale_x_log10()+
  guides(fill = guide_legend(title="Land use type"))
are.p

# Area distribution excluding urban and wetlands
dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  ggplot(aes(CAT_BASIN_AREA, fill = lnd_cat))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
  scale_x_log10()+
  guides(fill = guide_legend(title="Land use type"))


# Stream length distribution (cumulative at drainage point)
lgt.p <- ggplot(dat,aes(length_m, fill = lnd_cat))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
  scale_x_log10()+
  guides(fill = guide_legend(title="Land use type"))
lgt.p

# Stream length distribution excluding urban and wetlands
dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  ggplot(aes(length_m, fill = lnd_cat))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
  scale_x_log10()+
  guides(fill = guide_legend(title="Land use type"))

# Entropy and area
p2 <- ggplot(dat,aes(AreSqKM,h_rel_3))+
  geom_point()+
  scale_x_log10()
p2



# Scaling relationships by land cover

## Area and stream length 

dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(CAT_BASIN_AREA,length_m,color=lnd_cat))+
  xlab("Watershed Area")+
  ylab("Stream length")+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se = FALSE)+
  geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
  scale_x_log10()+
  scale_y_log10()+
  guides(color = guide_legend(title="Land use type"))


## Area and Discharge

# Since predicted discharge at a single location is cumulative, it should correlate
# with the watershed area up to that point

dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(TOT_BASIN_AREA,logQ_m3_div_s,color=lnd_cat))+
  geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
  xlab("Watershed Area")+
  ylab("Log [Predicted Discharge]")+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_log10()+
  # scale_y_log10()+
  guides(color = guide_legend(title="Land use type"))+
  facet_wrap(~lnd_cat)

## Area and Slope

dat %>% 
  filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(TOT_BASIN_AREA,CAT_STREAM_SLOPE,color=lnd_cat))+
  xlab("Watershed Area")+
  ylab("Local Stream Slope")+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm",se = FALSE, span = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  guides(color = guide_legend(title="Land use type"))+
  facet_wrap(~lnd_cat)

# Watershed area and cummulative metabolic rate

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

# Role of landscape heterogeneity

dat %>% 
  filter(lnd_cat %in% c("Mixed covers")) %>%
  filter(cum_totco2g_m2_day>0)%>%
  ggplot(aes(TOT_BASIN_AREA,cum_totco2g_m2_day, color = h_rel_3))+
  xlab("Watershed Area")+
  ylab("Cummulative respiration rates")+
  # geom_point(alpha = 0.05)+
  geom_point()+
  geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
  scale_x_log10()+
  scale_y_log10()

# Entropy plot

with(dat,scatter3D(tot_other_3,tot_ntrl_3,h_rel_3,bty ='b2',
                   colvar = as.integer(lnd_cat),
                   clab=c("Relative","Entropy"), 
                   theta = 15, phi =20, main = "Landscape Heterogeneity", 
                   xlab = "Mixed cover", ylab = "Natural cover",
                   zlab = "Relative entropy"))
plotrgl()

