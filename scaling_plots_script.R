###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# DATA PREPARATION
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(ggplot2,dplyr,plot3D,plot3Drgl,readr,rgl,tidyverse,entropy,GGally)

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

dat <- as_tibble(dat_o)
lnd <- as_tibble(lnd_o)
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
              
            
# This land use data set only contains percentage cover for the main land uses. So,
# not all the land uses add up to 100%. Let's double check for these cases, as well as
# other potential anomalies

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(tot_loc = round(urbn + frst + wtnd + agrc + shrb,2)) %>% 
  mutate(tot_acm = round(urbn_t + frst_t + wtnd_t + agrc_t + shrb_t,2)) 

summary(lnd$tot_loc)
summary(lnd$tot_acm)

# Although the mean for total land with a categorized use is between 91 -92% for 
# total watershed and local catchment respectively. We also observe values about 
# 100%.

zeroes_loc <- filter(lnd,tot_loc==0) # 21 cases
zeroes_tot <- filter(lnd,tot_acm==0) # 0 cases

above_loc <- filter(lnd,tot_loc > 100.000) #240 cases
above_tot <- filter(lnd,tot_acm > 100.000) #106 cases

# At a local level, 0's may represent situations in which land cover types, other 
# than those included in the dataset, were dominant (like pastures). Above 100 percent
# values are probably the result of rounding up or down percentages. The same could 
# be said about the above 100 percent values at the watershed scale. In those cases, we
# need to adjust land use percentages to add up to 100 exactly. 

lnd <- lnd %>% 
  mutate(agrc = if_else(tot_loc>100.00,agrc*(100/tot_loc),agrc)) %>% 
  mutate(frst = if_else(tot_loc>100.00,frst*(100/tot_loc),frst)) %>% 
  mutate(shrb = if_else(tot_loc>100.00,shrb*(100/tot_loc),shrb)) %>% 
  mutate(urbn = if_else(tot_loc>100.00,urbn*(100/tot_loc),urbn)) %>% 
  mutate(wtnd = if_else(tot_loc>100.00,wtnd*(100/tot_loc),wtnd)) %>%  
  mutate(tot_loc = if_else(tot_loc>100.00,agrc+frst+shrb+urbn+wtnd,tot_loc)) %>%
  mutate(agrc_t = if_else(tot_acm>100.00,agrc_t*(100/tot_acm),agrc_t)) %>% 
  mutate(frst_t = if_else(tot_acm>100.00,frst_t*(100/tot_acm),frst_t)) %>% 
  mutate(shrb_t = if_else(tot_acm>100.00,shrb_t*(100/tot_acm),shrb_t)) %>% 
  mutate(urbn_t = if_else(tot_acm>100.00,urbn_t*(100/tot_acm),urbn_t)) %>% 
  mutate(wtnd_t = if_else(tot_acm>100.00,wtnd_t*(100/tot_acm),wtnd_t)) %>% 
  mutate(tot_acm = if_else(tot_acm>100.00,agrc_t+frst_t+shrb_t+urbn_t+wtnd_t,tot_acm))

# Let's double check the presence of above 100% values:

above_loc <- filter(lnd,tot_loc > 100.000) #27 cases
above_tot <- filter(lnd,tot_acm > 100.000) #17 cases

# Since these data set only contain the proportions of the largest categories, the 
# remaining percentage should correspond to "other" uses that are not explicitly 
# included. For the Yakima river basin this could be Hay and Pastures. We will assing
# the remaining percentages to "othr". 

lnd <- lnd %>% group_by(COMID) %>% 
  mutate(othr = if_else(100-tot_loc>0,100-tot_loc,0)) %>% 
  mutate(othr_t = if_else(100-tot_acm>0,100-tot_acm,0)) %>% 
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

# Making row-wise operations (https://dplyr.tidyverse.org/articles/rowwise.html)
lnd <- lnd %>% rowwise() %>% 
  mutate(hl = entropy(c(p_agrc,
                        p_frst,
                        p_othr,
                        p_shrb,
                        p_urbn,
                        p_wtnd),unit = "log")) %>% 
  mutate(hrl = hl/log(6)) %>% 
  mutate(ht = entropy(c(p_agrc_t,
                        p_frst_t,
                        p_othr_t,
                        p_shrb_t,
                        p_urbn_t,
                        p_wtnd_t),unit = "log")) %>% 
  mutate(hrt = ht/log(6)) 
  
p4 <- ggplot(lnd,aes(hrl,hrt))+
  geom_point()
p4
  
# Information content analysis

# Using Shannon's entropy calculations, we could identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variablity. 

# We are going to use resampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Local dataset
lnd_el <- select(lnd,
                 p_agrc,
                 p_frst,
                 p_othr,
                 p_shrb,
                 p_urbn,
                 p_wtnd)

# Creating a matrix for results

ncols = 4
nrows = 6
ssz = 600
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Pastures","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
pt_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 5000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=600,replace = FALSE),]
  loc_im <- as.matrix(loc_im)[,order(colnames(loc_im))]
  iml <- loc_im[,c(2:7)]/sum(loc_im[,c(2:7)])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(lnd_el))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  pt_list[[i]] <- ic_loc[3,]
  sr_list[[i]] <- ic_loc[4,]
  ub_list[[i]] <- ic_loc[5,]
  wt_list[[i]] <- ic_loc[6,]

}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
pt_l = as_tibble(do.call("rbind",pt_list))
pt_l <- pt_l %>% mutate(use = "Pastures")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

local_im <- rbind(ag_l,fr_l,pt_l,sr_l,ub_l,wt_l)

# Watershed Scale

# Watershed dataset
lnd_et <- select(lnd,
                 p_agrc_t,
                 p_frst_t,
                 p_othr_t,
                 p_shrb_t,
                 p_urbn_t,
                 p_wtnd_t)

# Creating a matrix for results

ncols = 4
nrows = 6
ssz = 600
ic_tot <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Pastures","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

agt_list <- list()
frt_list <- list()
ptt_list <- list()
srt_list <- list()
ubt_list <- list()
wtt_list <- list()

# Number of iterations 
itn = 5000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  tot_im <- lnd_et[sample(nrow(lnd_et),size=600,replace = FALSE),]
  tot_im <- as.matrix(tot_im)[,order(colnames(tot_im))]
  imt <- tot_im[,c(2:7)]/sum(tot_im[,c(2:7)])
  for(j in 1:ncol(imt)){
    yjn = sum(iml[,j])
    hn = entropy(imt[,j], unit = "log")
    hmaxn = log(nrow(lnd_et))
    ic_tot[j,1]=yjn
    ic_tot[j,2]=hn
    ic_tot[j,3]=hmaxn
    ic_tot[j,4]=yjn%*%(hmaxn-hn)
  }
  agt_list[[i]] <- ic_tot[1,]
  frt_list[[i]] <- ic_tot[2,]
  ptt_list[[i]] <- ic_tot[3,]
  srt_list[[i]] <- ic_tot[4,]
  ubt_list[[i]] <- ic_tot[5,]
  wtt_list[[i]] <- ic_tot[6,]
  
}
agt_l = as_tibble(do.call("rbind",agt_list))
agt_l <- agt_l %>% mutate(use="Agriculture")
frt_l = as_tibble(do.call("rbind",frt_list))
frt_l <- frt_l %>% mutate(use = "Forests")
ptt_l = as_tibble(do.call("rbind",ptt_list))
ptt_l <- ptt_l %>% mutate(use = "Pastures")
srt_l = as_tibble(do.call("rbind",srt_list))
srt_l <- srt_l %>% mutate(use = "Shurblands")
ubt_l = as_tibble(do.call("rbind",ubt_list))
ubt_l <- ubt_l %>% mutate(use = "Urban")
wtt_l = as_tibble(do.call("rbind",wtt_list))
wtt_l <- wtt_l %>% mutate(use = "Wetlands")

wshd_im <- rbind(agt_l,frt_l,ptt_l,srt_l,ubt_l,wtt_l)


p5 <- ggplot(local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p5


p6 <- ggplot(wshd_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)
p6

# Information content analysis suggest the following groups (combining information-rich
# land use categories with information-poor land use categories). This allows for keeping
# the discriminating power of the categories, but reducing the variability that contribute
# less to the patterns

# We will calculate the entropy over the reduced groups and merge this dataset with 
# biogeochemical data

lnd_s0 <- select(lnd,COMID,p_agrc,p_frst,p_othr,p_shrb,p_urbn,p_wtnd,
                p_agrc_t,p_frst_t,p_othr_t,p_shrb_t,p_urbn_t,p_wtnd_t)

lnd_s0 <- lnd_s0 %>% group_by(COMID) %>% 
  mutate(p_ant = p_agrc + p_urbn + p_othr) %>% 
  mutate(p_frt = p_frst + p_wtnd) %>% 
  mutate(p_shb = p_shrb) %>% 
  mutate(p_ant_t = p_agrc_t + p_urbn_t + p_othr_t) %>% 
  mutate(p_frt_t = p_frst_t + p_wtnd_t) %>% 
  mutate(p_shb_t = p_shrb_t)

lnd_cvr <- select(lnd_s0,COMID,p_ant,p_frt,p_shb,p_ant_t,p_frt_t,p_shb_t)

lnd_cvr <- lnd_cvr %>% rowwise() %>% 
  mutate(hl = entropy(c(p_ant,
                        p_frt,
                        p_shb),unit = "log")) %>% 
  mutate(hrl = hl/log(3)) %>% 
  mutate(ht = entropy(c(p_ant_t,
                        p_frt_t,
                        p_shb_t),unit = "log")) %>% 
  mutate(hrt = ht/log(3)) 

# Let's remove the old land use columns from the biogeochemical dataset before we
# merge it with the "lnd_cvr" dataset

bgc <- select(dat,COMID,cum_totco2g_m2_day,CAT_STREAM_LENGTH,CAT_STREAM_SLOPE,
              TOT_STREAM_LENGTH,TOT_STREAM_SLOPE,CAT_BASIN_AREA,TOT_BASIN_AREA,
              totco2g_m2_day_fill,D50_m,StreamOrde,pred_annual_DOC,pred_annual_DO,
              no3_conc_mg_l,logRT_total_hz_s,logq_hz_total_m_s,totco2_o2g_m2_day,
              totco2_ang_m2_day)
bgc <- rename(bgc,
              acm_resp = cum_totco2g_m2_day,
              reach_lgt = CAT_STREAM_LENGTH, 
              reach_slp = CAT_STREAM_SLOPE,
              river_lgt = TOT_STREAM_LENGTH,
              river_slp = TOT_STREAM_SLOPE,
              reach_area = CAT_BASIN_AREA,
              wshd_area = TOT_BASIN_AREA,
              reach_resp = totco2g_m2_day_fill,
              d50m = D50_m,
              order = StreamOrde,
              doc_annual = pred_annual_DOC,
              do_annual = pred_annual_DO,
              nitrates = no3_conc_mg_l,
              res_time = logRT_total_hz_s,
              hz_exchng = logq_hz_total_m_s,
              aer_resp = totco2_o2g_m2_day,
              anb_resp = totco2_ang_m2_day)

#Merging bgc with lnd_cvr

bgc_lnd0 <- merge(bgc,lnd_cvr,by = "COMID") # There are duplicates COMIDs in both 
#datasets, to filter those out:

bgc_lnd <- unique(bgc_lnd0)

################################################################################
# Figures
################################################################################

# Let's start with a quick pairs plot to have a glimpse of the relationships among
# variables

ggpairs(bgc_lnd)



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

