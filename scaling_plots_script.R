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

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl,tidyverse,entropy, purrr)

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

lnd <- lnd %>% group_by(COMID) %>% 
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


################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Local dataset
lnd_el <- select(lnd,
                 COMID,
                 p_urbn,
                 p_frst,
                 p_wtnd,
                 p_agrc,
                 p_shrb,
                 p_othr)

# Watershed dataset
lnd_et <- select(lnd,
                 COMID,
                 p_urbn_t,
                 p_frst_t,
                 p_wtnd_t,
                 p_agrc_t,
                 p_shrb_t, 
                 p_othr_t)


p3 <- gather(lnd_el,c(2:7),key = "use", value = "fraction") %>% 
  ggplot(aes(x = use, y = fraction, color = use, fill = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p3

p4 <- gather(lnd_et,c(2:7),key = "use", value = "fraction") %>% 
  ggplot(aes(x = use, y = fraction, color = use, fill = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p4

# Local entropies

nrows = nrow(lnd_el)
ncols = 3

lh = matrix(1:nrows,nrows,ncols, dimnames = list(NULL,c("COMID","h_loc","hr_loc")))

for(i in 1:nrows){
  comid =unlist(lnd_el[i,1], use.names = FALSE)
  y = unlist(lnd_el[i,c(2:7)],use.names = FALSE)
  h_loc = entropy(y)
  hr_loc = h_loc/log(length(y),exp(1))
  lh[i,1] = comid
  lh[i,2] = h_loc
  lh[i,3] = hr_loc
}

# Watershed entropies

nrows = nrow(lnd_et)
ncols = 3

th = matrix(1:nrows,nrows,ncols, dimnames = list(NULL,c("COMID","h_loc","hr_loc")))

for(i in 1:nrows){
  comid =unlist(lnd_et[i,1], use.names = FALSE)
  y = unlist(lnd_et[i,c(2:7)],use.names = FALSE)
  h_tot = entropy(y)
  hr_tot = h_tot/log(length(y),exp(1))
  th[i,1] = comid
  th[i,2] = h_tot
  th[i,3] = hr_tot
}

# Information content analysis

# Using Shannon's entropy calculations, we could identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variablity. 

# We are going to use resampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Creating a matrix for results

ncols = 5
ic_loc <- matrix(1:nrows,nrows,ncols, dimnames = list(NULL,c("COMID","Yjn_l","Hn_l","Hmaxn_l", "In_l")))

list_l = list()

# Number of iterations 
itn = 5

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- sample(as.data.frame(lnd_el))
  loc_im <- as.matrix(loc_im)[,order(colnames(loc_im))]
  iml <- loc_im[,c(2:7)]/sum(loc_im[,c(2:7)])
  for(j in 1:ncol(iml)){
    comid = unlist(lnd_el[i,1], use.names = FALSE)
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(lnd_el),exp(1))
    ic_loc[j,1]=comid
    ic_loc[j,2]=yjn
    ic_loc[j,3]=hn
    ic_loc[j,4]=hmaxn
    ic_loc[j,5]=yjn%*%(hmaxn-hn)
  }
  list_l[[i]] <- ic_loc
}

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

