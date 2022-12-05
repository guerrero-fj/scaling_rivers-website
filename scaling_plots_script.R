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

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl,tidyverse)

#Data:

#header info
hdr_o <- read.csv("data/header_info_cum_resp_YRB_data_0725_2022.csv")

#values
dat_o <- read.csv("data/cum_resp_YRB_data_0725_2022_entropy.csv", 
                stringsAsFactors=TRUE)
lnd_o <- read.csv("data/YRB_comid_landuse_2011.csv", 
                stringsAsFactors=TRUE)
#This is the land use data used by Son et al., 2022a and 2022b to model inputs of 
#DOC, NO3, and OD to the YR (Although the map in the figures uses NLDC-2016).


# Processing land cover data

#We calculate landscape heterogeneity as the entropy of the proportions of the 
#different landcover in both the local and cumulative drainage area to each 
#stream segment. Let's first create a working dataset, which right now is just
#a copy of the original lnd:

dat <- as.tibble(dat_o)
lnd <- as.tibble(lnd_o)

#Let's shorten column names

lnd <- rename(lnd,
            urbn = urban,
            frst = forest,
            wtnd = wetland, 
            shrb = shrub,
            urbn_t = turban,
            frst_t = tforest,
            wtnd_t = twetland,
            agrc_t = tagrc,
            shrb_t = tshrub)
              
            
#Let's first re-calculate the relative proportions of the land use categories as
#not all of them add up to 100%

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(tot_loc = urbn + frst + wtnd + agrc + shrb) %>% 
  mutate(tot_acm = urbn_t + frst_t + wtnd_t + agrc_t + shrb_t) %>% 
  mutate(p_urbn = if_else(tot_loc > 0, urbn / tot_loc, -9999)) %>% 
  mutate(p_frst = if_else(tot_loc > 0, frst / tot_loc, -9999)) %>%
  mutate(p_wtnd = if_else(tot_loc > 0, wtnd / tot_loc, -9999)) %>% 
  mutate(p_agrc = if_else(tot_loc > 0, agrc / tot_loc, -9999)) %>%
  mutate(p_shrb = if_else(tot_loc > 0, shrb / tot_loc, -9999))
           
# Let's now calculate the relative contribution to information entropy from each 
# land use type, at the local scale, as p*ln(p),as well as the shannon entropy:
# h-loc = -sum(plnp) and hr-loc (relative entropy, from 0 to 1) as h-loc/ln(#land use categories = 5)
           
lnd <- lnd %>% group_by(COMID) %>% 
  mutate(plnp_urbn = if_else(p_urbn > 0,p_urbn*log(p_urbn, exp(1)),0)) %>% 
  mutate(plnp_frst = if_else(p_frst > 0,p_frst*log(p_frst, exp(1)),0)) %>% 
  mutate(plnp_wtnd = if_else(p_wtnd > 0,p_wtnd*log(p_wtnd, exp(1)),0)) %>% 
  mutate(plnp_agrc = if_else(p_agrc > 0,p_agrc*log(p_agrc, exp(1)),0)) %>% 
  mutate(plnp_shrb = if_else(p_shrb > 0,p_shrb*log(p_shrb, exp(1)),0)) %>% 
  mutate(h_loc = -(plnp_urbn + plnp_frst + plnp_wtnd + plnp_agrc + plnp_shrb)) %>% 
  mutate(hr_loc = h_loc/log(5,exp(1)))


# Let's finally calculate entropies for cumulative land use to the stream segment

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(p_urbn_t = if_else(tot_acm > 0, urbn_t / tot_acm, -9999)) %>% 
  mutate(p_frst_t = if_else(tot_acm > 0, frst_t / tot_acm, -9999)) %>%
  mutate(p_wtnd_t = if_else(tot_acm > 0, wtnd_t / tot_acm, -9999)) %>% 
  mutate(p_agrc_t = if_else(tot_acm > 0, agrc_t / tot_acm, -9999)) %>%
  mutate(p_shrb_t = if_else(tot_acm > 0, shrb_t / tot_acm, -9999)) %>% 
  mutate(plnp_urbn_t = if_else(p_urbn_t > 0,p_urbn_t*log(p_urbn_t, exp(1)),0)) %>% 
  mutate(plnp_frst_t = if_else(p_frst_t > 0,p_frst_t*log(p_frst_t, exp(1)),0)) %>% 
  mutate(plnp_wtnd_t = if_else(p_wtnd_t > 0,p_wtnd_t*log(p_wtnd_t, exp(1)),0)) %>% 
  mutate(plnp_agrc_t = if_else(p_agrc_t > 0,p_agrc_t*log(p_agrc_t, exp(1)),0)) %>% 
  mutate(plnp_shrb_t = if_else(p_shrb_t > 0,p_shrb_t*log(p_shrb_t, exp(1)),0)) %>% 
  mutate(h_acm = -(plnp_urbn_t + plnp_frst_t + plnp_wtnd_t + plnp_agrc_t + plnp_shrb_t)) %>% 
  mutate(hr_acm = h_acm/log(5,exp(1)))

p1 <- ggplot(lnd,aes(hr_loc,hr_acm))+
  geom_point()
p1


# Let's merge a subset of our entropy calculations with the biogeochemical dataset

# First, let's subset ent_lnd to extract the most relevant columns:

ent_lnd_m <- dplyr::select(ent_lnd,COMID,p_urbn,p_frst,p_wtnd,p_agrc,p_shrb,p_urbn_t,
                           p_frst_t,p_wtnd_t,p_agrc_t,p_shrb_t,hr_loc,hr_acm)

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

