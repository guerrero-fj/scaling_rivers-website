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

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl,tidyverse,entropy)

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
#stream segment. Let's first create a working data set, which right now is just
#a copy of the original lnd:

dat <- as.tibble(dat_o)
lnd <- as.tibble(lnd_o)
my_colors <- c("#F564E3","#00BA38","#F8766D","#B79F00","#619CFF","#00BFC4")

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
              
            
# Let's check that the proportions for both local and watershed scale add up to 100%
lnd <- lnd %>% group_by(COMID) %>% 
  mutate(tot_loc = urbn + frst + wtnd + agrc + shrb) %>% 
  mutate(tot_acm = urbn_t + frst_t + wtnd_t + agrc_t + shrb_t) 

# Local land use totals
p1 <- ggplot(lnd, aes(y = tot_loc))+
  geom_boxplot()
p1

# Cumulative land use totals
p2 <- ggplot(lnd, aes(y = tot_acm))+
  geom_boxplot()
p2


# Since these data set only contain the proportions of the largest categories, the 
# remaining percentage should correspond to "other" uses that are not explicitly 
# included. For the Yakima river basin this could be Hay and Pastures. We will assing
# the remaining percentages to "othr". 

lnd_e <- lnd %>% group_by(COMID) %>% 
  mutate(othr = 100 - tot_loc) %>% 
  mutate(othr_t = 100 - tot_acm) %>% 
  mutate(p_urbn = urbn/100) %>% 
  mutate(p_frst = frst/100) %>% 
  mutate(p_agrc = agrc/100) %>% 
  mutate(p_wtnd = wtnd/100) %>% 
  mutate(p_shrb = shrb/100) %>% 
  mutate(p_othr = othr/100) %>% 
  mutate(p_urbn_t = urbn_t/100) %>% 
  mutate(p_frst_t = frst_t/100) %>% 
  mutate(p_agrc_t = agrc_t/100) %>% 
  mutate(p_wtnd_t = wtnd_t/100) %>% 
  mutate(p_shrb_t = shrb_t/100) %>% 
  mutate(p_othr_t = othr_t/100) 


# # Let's examine how many samples are below 75% for total land cover use
# 
# lr_smp <- nrow(filter(dplyr::select(lnd,tot_acm),tot_acm < 70))
# lr_smp
# 
# # The percentage of samples for which less than 75% of the land cover is categorized is
# 
# lr_pct <- lr_smp/nrow(lnd)
# 
# # Only 5% of the segments. We will remove these segments from the analysis
# 
# lnd_r <- dplyr::filter(lnd, tot_acm > 70)
# 
# # Let's now check at the local level
# 
# lr_smp_l <- nrow(filter(dplyr::select(lnd_r,tot_loc),tot_loc < 70))
# lr_smp_l
# 
# # 266 additional samples with less than 70% of the local drainage area with categorized land cover. We will
# # remove these additional samples from the analysis
# 
# lnd_d <- dplyr::filter(lnd_r, tot_loc > 70)
# 
# #With this procedure we are still retaining 91% of the data for analysis
# 
# # Let's first re-calculate the relative proportions of the land use categories as
# #not all of them add up to 100%
# 
# lnd_d <- lnd_d %>% group_by(COMID) %>% 
#   mutate(p_urbn = urbn/tot_loc) %>% 
#   mutate(p_frst = frst/tot_loc) %>%
#   mutate(p_wtnd = wtnd/tot_loc) %>% 
#   mutate(p_agrc = agrc/tot_loc) %>%
#   mutate(p_shrb = shrb/tot_loc) %>% 
#   mutate(p_urbn_t = urbn_t/tot_acm) %>% 
#   mutate(p_frst_t = frst_t/tot_acm) %>%
#   mutate(p_wtnd_t = wtnd_t/tot_acm) %>% 
#   mutate(p_agrc_t = agrc_t/tot_acm) %>%
#   mutate(p_shrb_t = shrb_t/tot_acm)
# 
# #Let's now check that the sum of the land use categories add up to 100%
# 
# lnd_e <- lnd_d %>% select(COMID,p_urbn,p_frst,p_wtnd,p_agrc,p_shrb,
#                           p_urbn_t,p_frst_t,p_wtnd_t,p_agrc_t,p_shrb_t) %>% group_by(COMID) %>% 
#   mutate(tot_loc = p_urbn + p_frst + p_wtnd + p_agrc + p_shrb) %>% 
#   mutate(tot_acm = p_urbn_t + p_frst_t + p_wtnd_t + p_agrc_t + p_shrb_t)


p3_dat <- select(lnd_e,COMID,p_urbn,p_frst,p_wtnd,p_agrc,p_shrb, p_othr)
p3_dat <- gather(p3_dat,c(2:7),key = "use", value = "fraction")

p3 <- ggplot(p3_dat,aes(x = use, y = fraction, color = use, fill = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p3

p4_dat <- select(lnd_e,COMID,p_urbn_t,p_frst_t,p_wtnd_t,p_agrc_t,p_shrb_t, p_othr_t)
p4_dat <- gather(p4_dat,c(2:7),key = "use", value = "fraction")

p4 <- ggplot(p4_dat,aes(x = use, y = fraction, color = use, fill = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p4

          
################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Local entropies

lnd_e %>% group_by(COMID) %>% 
  mutate(plp_agrc = if_else(p_agrc > 0,p_agrc*log(p_agrc, exp(1)),0)) %>% 
  mutate(plp_frst = if_else(p_frst > 0,p_frst*log(p_frst, exp(1)),0)) %>% 
  mutate(plp_othr = if_else(p_othr > 0,p_othr*log(p_othr, exp(1)),0)) %>% 
  mutate(plp_shrb = if_else(p_shrb > 0,p_shrb*log(p_shrb, exp(1)),0)) %>% 
  mutate(plp_urbn = if_else(p_urbn > 0,p_urbn*log(p_urbn, exp(1)),0)) %>% 
  mutate(plp_wtnd = if_else(p_wtnd > 0,p_wtnd*log(p_wtnd, exp(1)),0)) %>% 
  mutate(h_loc = -(plp_agrc + plp_frst + plp_othr + plp_shrb + plp_urbn + plp_wtnd)) %>% 
  mutate(hr_loc = h_loc/log(6,exp(1)))






           
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


# Information content analysis

# Using Shannon's entropy calculations, we could identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variablity. 

# First, let's subset ent_lnd to extract the most relevant columns:

lnd_m <- dplyr::select(lnd,COMID,p_urbn,p_frst,p_wtnd,p_agrc,p_shrb,p_urbn_t,
                           p_frst_t,p_wtnd_t,p_agrc_t,p_shrb_t,hr_loc,hr_acm)



# From this part down, I will use re-sampling to estimate averages and standard
# deviations for the information contribution of each land use category

# Information contribution, local scale

loc_inf <- dplyr::select(lnd_m, COMID, p_urbn, p_frst, p_agrc, p_wtnd, p_shrb)

# The first step is to express land use proportions in the context of the grand
# total

g_tot <- sum(loc_inf)

loc_inf <- loc_inf %>% 
  mutate(pi_urbn = if_else(p_urbn > 0, p_urbn/g_tot, 0)) %>% 
  mutate(pi_frst = if_else(p_frst > 0, p_frst/g_tot, 0)) %>% 
  mutate(pi_wtnd = if_else(p_wtnd > 0, p_wtnd/g_tot, 0)) %>% 
  mutate(pi_agrc = if_else(p_agrc > 0, p_agrc/g_tot, 0)) %>% 
  mutate(pi_shrb = if_else(p_shrb > 0, p_shrb/g_tot, 0))

# The second step is to calculate the information contribution of each land use
# across all the stream segments. To do so, we need to normalize pi_use by the 
# total of each column.

loc_ifc <- loc_inf %>% 
  mutate(ic_urbn = pi_urbn/sum(pi_urbn)) %>% 
  mutate(ic_frst = pi_frst/sum(pi_frst)) %>% 
  mutate(ic_wtnd = pi_wtnd/sum(pi_wtnd)) %>% 
  mutate(ic_agrc = pi_agrc/sum(pi_agrc)) %>% 
  mutate(ic_shrb = pi_shrb/sum(pi_shrb))

glimpse(loc_ifc)


#Land-cover categories (2011)
thd <- 0.8


# Local drainage area
lnd_m$use_loc <- as.factor(with(lnd_m,ifelse(p_urbn>thd,"Urban",
                                             ifelse(p_frst>thd,"Forest",
                                                    ifelse(p_wtnd>thd,"Wetland",
                                                           ifelse(p_agrc>thd,"Agriculture",
                                                                  ifelse(p_shrb>thd,"Shrubland",
                                                                         "Mixed covers")))))))

# Total drainage area
lnd_m$use_acm <- as.factor(with(lnd_m,ifelse(p_urbn_t>thd,"Urban",
                                         ifelse(p_frst_t>thd,"Forest",
                                                ifelse(p_wtnd_t>thd,"Wetland",
                                                       ifelse(p_agrc_t>thd,"Agriculture",
                                                              ifelse(p_shrb_t>thd,"Shrubland",
                                                                     "Mixed covers")))))))


# Color scale

# Let's define a color scale for our plots using a basic density plot for local 
# entropies


colors <- ggplot_build(lnd_m %>% ggplot(aes(hr_loc, color = use_loc, fill = use_loc))+
               geom_density(alpha = 0.5))
unique(colors$data[[1]]$fill)

my_colors <- c("#F564E3","#00BA38","#F8766D","#B79F00","#619CFF","#00BFC4")

p1 <- ggplot(lnd_m,aes(hr_loc,hr_acm, color = use_acm))+
    geom_point()+
    scale_color_manual(values = my_colors)
p1

p2 <- ggplot(lnd_m, aes(hr_loc, color = use_loc, fill = use_loc))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = my_colors)+
  scale_color_manual(values = my_colors)+
  facet_wrap(~use_loc)
p2


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

