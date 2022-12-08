###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(ggplot2,# for plotting
                 dplyr,# for data wrangling
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 tidyverse,#data wrangling and tidying
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 scales,# manipulating log scales
                 stringr)# editing text

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", size=1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", size=1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())
set.seed(2703)

#Data:

#header info

# pending!!!!!

#values
bgc_lnd <- read.csv("data/221206_scaling_lnd_bgc.csv", 
                  stringsAsFactors=TRUE)
bgc_lnd <- as_tibble(bgc_lnd)


################################################################################
# Figures
################################################################################

# Let's start with a quick pairs plot to have a glimpse of the relationships among
# variables

dat_pair <- as_tibble(select(bgc_lnd,acm_resp,river_lgt,reach_area,wshd_area,d50m,order,
                             hrt))

dat_pair <- dat_pair %>% 
  mutate(log_resp = log10(acm_resp)) %>% 
  mutate(log_lgt = log10(river_lgt)) %>% 
  mutate(log_r_area =log10(reach_area)) %>% 
  mutate(log_w_area = log10(wshd_area)) %>% 
  mutate(log_d50m = log10(d50m)) 

log_dat_pair <- select(dat_pair,c(6:12))

ggpairs(dat_pair)
ggpairs(log_dat_pair)

# Main chart (drafts)

# Creating breaks for logarithmic scale (see: https://r-graphics.org/recipe-axes-axis-log)

breaks = 10^(-2:5)
minor_breaks = rep(1:9,21)*(10^rep(-2:5,each=9))

# landscape heterogeneity
# Guiding lines

# option 1: using unique values
gl_dat <- select(bgc_lnd,wshd_area,acm_resp) %>% 
  group_by(wshd_area) %>% 
  summarise(r_mx = max(acm_resp),
            r_av = mean(acm_resp),
            r_mn = min(acm_resp))

gl_plot <- ggplot(gl_dat,aes(wshd_area,r_av))+
  geom_smooth(method="lm")+
  geom_smooth(aes(wshd_area,r_mx),method = "lm")+
  geom_smooth(aes(wshd_area,r_mn),method = "lm")+
  scale_x_log10()+
  scale_y_log10()
gl_plot

up_dat <- filter(bgc_lnd,acm_resp>10)
dw_dat <- filter(bgc_lnd,p_frt_t<0.1 & wshd_area>50)

# Option 2: using residuals from the 1:1 line

rgl_dat <- select(bgc_lnd,wshd_area,acm_resp) %>% 
  mutate(p_resp = wshd_area) %>% 
  mutate(rsd = acm_resp - p_resp)

p <- ggplot(filter(rgl_dat,rsd<0 & acm_resp>0.0001),aes(wshd_area,acm_resp))+
  geom_point(color="black")+
  geom_smooth()+
  geom_point(data = filter(rgl_dat,rsd>0 & acm_resp>0.0001),aes(wshd_area,acm_resp),
             color = "blue")+
  geom_smooth(data = filter(rgl_dat,rsd>0 & acm_resp>0.0001),aes(wshd_area,acm_resp))+
  scale_x_log10()+
  scale_y_log10()
p

# Option 3: Identifying covariates that could segregate the scaling behaviors

p <- ggplot(na.omit(bgc_lnd),aes(wshd_area,acm_resp,color=res_time))+
  geom_point()+
  geom_point(data=filter(na.omit(bgc_lnd),res_time>7),aes(wshd_area,acm_resp),
             color="black",inherit.aes = FALSE)+
  geom_smooth(data=filter(na.omit(bgc_lnd),res_time>7),aes(wshd_area,acm_resp),
             color="black",method="lm",inherit.aes = FALSE)+
  geom_point(data=filter(na.omit(bgc_lnd),res_time<4),aes(wshd_area,acm_resp),
             color="black",inherit.aes = FALSE)+
  geom_smooth(data=filter(na.omit(bgc_lnd),res_time<4),aes(wshd_area,acm_resp),
              color="green",method="lm",inherit.aes = FALSE)+
  geom_abline(slope=1)+
  scale_x_log10()+
  scale_y_log10()
p

###############################################################################
# REMOVING NAs FROM OUR DATA SET GLOBALLY
bgc_cln <- as_tibble(na.omit(bgc_lnd))
###############################################################################

# let's take a look at the distribution of values for residence time


rs_box <- ggplot(na.omit(bgc_lnd),aes(y=res_time))+
  geom_boxplot()
rs_box

summary(bgc_cln$res_time)

# let's now create a categorical variable for residence time using the quartiles

bgc_cln$rt_cat <- cut(bgc_cln$res_time,
                      breaks=c(1,4,4.6,4.8,5,5.2,5.4,5.6,5.8,6,7,8,9),
                      labels = c("a","b","c","d","e","f","g","h","i","j","k","l"))


p <- ggplot(bgc_cln,aes(wshd_area,acm_resp,color=rt_cat))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope=1.0)+
  facet_wrap(~rt_cat)
p


# relationship between residence time and slope
p <- ggplot(bgc_cln,aes(reach_slp,res_time))+
  geom_point()+
  geom_smooth()
p

# Calculating slopes, intercepts, and regression significance for multiple levels
# of residence time

# Creating a matrix for results

# Number of iterations 
rtc = levels(bgc_cln$rt_cat)
cls = 12
itn = 1:13

ncols = 11
nrows = length(itn)
ssz = 100
scl_exp <- matrix(1:nrows,nrows,ncols, 
                  dimnames = list(NULL, c("ant_avg",
                                          "frt_avg",
                                          "ht_avg",
                                          "p_exp", 
                                          "p_mod",                   
                                          "r_squared",
                                          "reach_slp",
                                          "res_median",
                                          "rt_cat",
                                          "s_exponent",
                                          "shb_avg")))

exp_list <- list()


for (i in rtc){
  for( j in itn){
    dat <- bgc_cln %>% filter(rt_cat==i)
    tst = dat[sample(nrow(dat),size=ssz,replace = TRUE),]
    sm <- lm(log(acm_resp)~log(wshd_area),data =tst)
    st <- summary(sm)
    rsq <- as.numeric(st$r.squared)
    slp <- as.numeric(sm$coefficients[2])
    mrs <- as.numeric(median(sm$residuals))
    p_int <- as.numeric(st$coefficients[2,4])
    p_mod <- as.numeric(pf(st$fstatistic[1],         
                           st$fstatistic[2], 
                           st$fstatistic[3], 
                           lower.tail = FALSE))
    ant <- mean(tst$p_ant_t)
    frt <- mean(tst$p_frt_t)
    shb <- mean(tst$p_shb_t)
    hrt <- mean(tst$hrt)
    rsp <- mean(tst$reach_slp)
    # crt <- i
    scl_exp[j,1] <- ant
    scl_exp[j,2] <- frt
    scl_exp[j,3] <- hrt
    scl_exp[j,4] <- p_int
    scl_exp[j,5] <- p_mod
    scl_exp[j,6] <- rsq
    scl_exp[j,7] <- rsp
    scl_exp[j,8] <- mrs
    scl_exp[j,9] <- mrs#fix this 
    scl_exp[j,10] <- slp
    scl_exp[j,11] <- shb
  }
  exp_list[[which(rtc==i)]] <- scl_exp
}





























###############################################################################
#FIGURES
###############################################################################

p1 <- ggplot(na.omit(bgc_cln),aes(wshd_area, acm_resp, color = rt_cat))+
  geom_point()+
  # geom_smooth(method = "lm")+
  # geom_point(aes(alpha = hrt), size = 2.5)+
  # geom_smooth(data = filter(rgl_dat,rsd>0 & acm_resp>100),aes(wshd_area,acm_resp),
  #             method = "lm")+
  # geom_smooth(data = filter(rgl_dat,rsd<0 & wshd_area>50),aes(wshd_area,acm_resp),
  #             method = "lm")+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  # scale_color_distiller("Landscape\nheterogeneity",palette = "Blues",direction = 1,
  #                       breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =0.85,yintercept = 10000, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none")+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())+
  facet_wrap(~rt_cat)
p1

ggsave(file="guerrero_etal_22_scaling_landscape_entropy.png",
       width = 10,
       height = 10,
       units = "in")

# Forestcapes

p2 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_frt_t))+
  geom_point(aes(alpha = p_frt_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Forestcapes\ncover",palette = "Greens",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p2

ggsave(file="guerrero_etal_22_scaling_forestcapes_cover.png",
       width = 10,
       height = 10,
       units = "in")


# Shurblandscapes

p3 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_shb_t))+
  geom_point(aes(alpha = p_shb_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Shurblandcapes\ncover",palette = "Oranges",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p3

ggsave(file="guerrero_etal_22_scaling_shrublandcapes_cover.png",
       width = 10,
       height = 10,
       units = "in")

# Humanscapes

p4 <- ggplot(filter(bgc_lnd,acm_resp>0.0015),aes(wshd_area, acm_resp, color = p_ant_t))+
  geom_point(aes(alpha = p_ant_t), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Humanscapes\ncover",palette = "Purples",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1, size = 0.75, linetype = "dashed", color="red")+
  geom_vline(xintercept = 0.01, linetype = "dotted")+
  geom_hline(yintercept = 0.01, linetype = "dotted")+
  guides(alpha = "none", reverse = TRUE)+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
p4

ggsave(file="guerrero_etal_22_scaling_humanscapes_cover.png",
       width = 10,
       height = 10,
       units = "in")


################################################################################
# Scaling exponents behavior
################################################################################

exp <- select(bgc_lnd,
              acm_resp,
              wshd_area,
              p_ant_t,
              p_frt_t,
              p_shb_t,
              hrt)

exp <- filter(exp,acm_resp>0 & wshd_area>0)

sm <- lm(log(acm_resp)~log(wshd_area),data =na.omit(exp))
st <- summary(sm)

# Extracting stats from linear regression 
# (see:https://statisticsglobe.com/extract-standard-error-t-and-p-value-from-regression-in-r )
# see: https://machinelearningmastery.com/degrees-of-freedom-in-machine-learning/#:~:text=can%20be%20determined.-,Total%20Degrees%20of%20Freedom%20for%20Linear%20Regression,model%20error%20degrees%20of%20freedom.&text=Generally%2C%20the%20degrees%20of%20freedom,used%20to%20fit%20the%20model.


rsq <- as.numeric(st$r.squared)# r squared
slp <- as.numeric(sm$coefficients[2])# scaling exponent (i.e. slope)
mrs <- as.numeric(median(sm$residuals))# median of residuals
p_int <- as.numeric(st$coefficients[2,4])#p-value intercept
p_mod <- as.numeric(pf(st$fstatistic[1], # F-statistic             
            st$fstatistic[2], # Model degrees of freedom (params -1)
            st$fstatistic[3], # Model error degrees of freedom (N-1)
            lower.tail = FALSE))# p-value overall model


# Creating a matrix for results

ncols = 9
nrows = 1
ssz = 100
scl_exp <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(NULL, c("ant_avg",
                                         "frt_avg",
                                         "ht_avg",
                                         "p_exp", 
                                         "p_mod",                   
                                         "r_squared",
                                         "res_median",
                                         "s_exponent",
                                         "shb_avg")))

exp_list <- list()


# Number of iterations 


itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  scl_st <- exp[sample(nrow(exp),size=600,replace = TRUE),]
  sm <- lm(log(acm_resp)~log(wshd_area),data =na.omit(scl_st))
  st <- summary(sm)
  rsq <- as.numeric(st$r.squared)
  slp <- as.numeric(sm$coefficients[2])
  mrs <- as.numeric(median(sm$residuals))
  p_int <- as.numeric(st$coefficients[2,4])
  p_mod <- as.numeric(pf(st$fstatistic[1],         
                           st$fstatistic[2], 
                           st$fstatistic[3], 
                           lower.tail = FALSE))
  ant <- mean(scl_st$p_ant_t)
  frt <- mean(scl_st$p_frt_t)
  shb <- mean(scl_st$p_shb_t)
  hrt <- mean(scl_st$hrt)
  scl_exp[,1]=ant
  scl_exp[,2]=frt
  scl_exp[,3]=hrt
  scl_exp[,4]=p_int
  scl_exp[,5]=p_mod
  scl_exp[,6]=rsq
  scl_exp[,7]=mrs
  scl_exp[,8]=slp
  scl_exp[,9]=shb
exp_list[[i]] <- scl_exp
}

scaling_exp <- as_tibble(do.call("rbind",exp_list))
scaling_exp <- select(scaling_exp,
                      ant_avg,
                      frt_avg,
                      shb_avg,
                      ht_avg,
                      s_exponent,
                      p_exp,
                      p_mod,
                      r_squared,
                      res_median)
glimpse(scaling_exp)
summary(scaling_exp$r_squared)
summary(scaling_exp$frt_avg)



























































# # P2 shows that for segments under land cover closer to 100% forest, the scaling
# # exponents would be >1, i.e. superlinear scaling. While in lands with no forest
# # cover, the scaling exponents seems to be sublinear.
# 
# p3 <- ggplot(na.omit(bgc_lnd),aes(p_ant_t,hrt,color = hrt))+
#   geom_point()
# p3
# 
# p4 <- ggplot(na.omit(bgc_lnd),aes(p_ant_t,p_frt_t,color=log(acm_resp)))+
#   geom_point()
# p4
# 
# # 
# 
# with(na.omit(bgc_lnd),scatter3D(p_frt_t,p_shb_t,hrl,bty ='b2',
#                    colvar = do_annual,
#                    clab=c("Dissolved Oxygen"), 
#                    theta = 15, phi =20, main = "Landscape Heterogeneity", 
#                    xlab = "Mixed cover", ylab = "Natural cover",
#                    zlab = "Relative entropy"))
# plotrgl()
# 
# 
# 
# 
# #Land-cover categories (2011)
# thd <- 0.8
# 
# 
# # Local drainage area
# lnd_m$use_loc <- as.factor(with(lnd_m,ifelse(p_urbn>thd,"Urban",
#                                              ifelse(p_frst>thd,"Forest",
#                                                     ifelse(p_wtnd>thd,"Wetland",
#                                                            ifelse(p_agrc>thd,"Agriculture",
#                                                                   ifelse(p_shrb>thd,"Shrubland",
#                                                                          "Mixed covers")))))))
# 
# # Total drainage area
# lnd_m$use_acm <- as.factor(with(lnd_m,ifelse(p_urbn_t>thd,"Urban",
#                                              ifelse(p_frst_t>thd,"Forest",
#                                                     ifelse(p_wtnd_t>thd,"Wetland",
#                                                            ifelse(p_agrc_t>thd,"Agriculture",
#                                                                   ifelse(p_shrb_t>thd,"Shrubland",
#                                                                          "Mixed covers")))))))
# 
# 
# # Color scale
# 
# # Let's define a color scale for our plots using a basic density plot for local 
# # entropies
# 
# 
# colors <- ggplot_build(lnd_m %>% ggplot(aes(hr_loc, color = use_loc, fill = use_loc))+
#                          geom_density(alpha = 0.5))
# unique(colors$data[[1]]$fill)
# 
# my_colors <- c("#F564E3","#00BA38","#F8766D","#B79F00","#619CFF","#00BFC4")
# 
# p1 <- ggplot(lnd_m,aes(hr_loc,hr_acm, color = use_acm))+
#   geom_point()+
#   scale_color_manual(values = my_colors)
# p1
# 
# p2 <- ggplot(lnd_m, aes(hr_loc, color = use_loc, fill = use_loc))+
#   geom_density(alpha = 0.5)+
#   scale_fill_manual(values = my_colors)+
#   scale_color_manual(values = my_colors)+
#   facet_wrap(~use_loc)
# p2
# 
# 
# #exploratory plots
# 
# # Slope distribution (segments)
# slp.p <- ggplot(dat,aes(TOT_STREAM_SLOPE))+
#   geom_vline(xintercept = 0.01425)+
#   geom_density()+
#   scale_x_log10()
# slp.p
# 
# # # Raw slope data
# # dat %>% ggplot(aes(CAT_STREAM_SLOPE, fill = lnd_cat))+
# #   geom_density(alpha = 0.5)+
# #   geom_vline(xintercept = 0.01425, linetype = "dashed")+
# #   scale_x_log10()+
# #   guides(fill = guide_legend(title="Land use type"))
# # 
# # #color scale
# # colors <- ggplot_build(dat %>% ggplot(aes(CAT_STREAM_SLOPE, fill = lnd_cat))+
# #                geom_density(alpha = 0.5)+
# #                geom_vline(xintercept = 0.01425, linetype = "dashed")+
# #                scale_x_log10()+
# #                guides(fill = guide_legend(title="Land use type")))
# # unique(colors$data[[1]]$fill)
# 
# # Raw slope data (color scale)
# dat %>% ggplot(aes(TOT_STREAM_SLOPE, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+#scale densities
#   geom_vline(xintercept = 0.01425, linetype = "dashed")+
#   scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3"))+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# 
# 
# # Area distribution (segments)
# are.p <- ggplot(dat,aes(CAT_BASIN_AREA, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+
#   scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# are.p
# 
# # Area distribution excluding urban and wetlands
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   ggplot(aes(CAT_BASIN_AREA, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+
#   scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# 
# 
# # Stream length distribution (cumulative at drainage point)
# lgt.p <- ggplot(dat,aes(length_m, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+
#   scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# lgt.p
# 
# # Stream length distribution excluding urban and wetlands
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   ggplot(aes(length_m, fill = lnd_cat))+
#   geom_density(alpha = 0.5)+
#   scale_fill_manual(values = c("#F8766D", "#00BF7D","#A3A500", "#00B0F6", "#E76BF3", "gray"))+
#   scale_x_log10()+
#   guides(fill = guide_legend(title="Land use type"))
# 
# # Entropy and area
# p2 <- ggplot(dat,aes(AreSqKM,h_rel_3))+
#   geom_point()+
#   scale_x_log10()
# p2
# 
# 
# 
# # Scaling relationships by land cover
# 
# ## Area and stream length 
# 
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   filter(cum_totco2g_m2_day>0)%>%
#   ggplot(aes(CAT_BASIN_AREA,length_m,color=lnd_cat))+
#   xlab("Watershed Area")+
#   ylab("Stream length")+
#   geom_point(alpha = 0.05)+
#   geom_smooth(method = "lm", se = FALSE)+
#   geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
#   scale_x_log10()+
#   scale_y_log10()+
#   guides(color = guide_legend(title="Land use type"))
# 
# 
# ## Area and Discharge
# 
# # Since predicted discharge at a single location is cumulative, it should correlate
# # with the watershed area up to that point
# 
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   filter(cum_totco2g_m2_day>0)%>%
#   ggplot(aes(TOT_BASIN_AREA,logQ_m3_div_s,color=lnd_cat))+
#   geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
#   xlab("Watershed Area")+
#   ylab("Log [Predicted Discharge]")+
#   geom_point(alpha = 0.05)+
#   geom_smooth(method = "lm", se = FALSE)+
#   scale_x_log10()+
#   # scale_y_log10()+
#   guides(color = guide_legend(title="Land use type"))+
#   facet_wrap(~lnd_cat)
# 
# ## Area and Slope
# 
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   filter(cum_totco2g_m2_day>0)%>%
#   ggplot(aes(TOT_BASIN_AREA,CAT_STREAM_SLOPE,color=lnd_cat))+
#   xlab("Watershed Area")+
#   ylab("Local Stream Slope")+
#   geom_point(alpha = 0.05)+
#   geom_smooth(method = "lm",se = FALSE, span = 0.5)+
#   scale_x_log10()+
#   scale_y_log10()+
#   guides(color = guide_legend(title="Land use type"))+
#   facet_wrap(~lnd_cat)
# 
# # Watershed area and cummulative metabolic rate
# 
# dat %>% 
#   filter(lnd_cat %in% c("Forest", "Agriculture", "Shrubland", "Mixed covers")) %>%
#   filter(cum_totco2g_m2_day>0)%>%
#   ggplot(aes(TOT_BASIN_AREA,cum_totco2g_m2_day,color=lnd_cat))+
#   xlab("Watershed Area")+
#   ylab("Cummulative respiration rates")+
#   geom_point(alpha = 0.05)+
#   geom_smooth(method = "lm")+
#   geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
#   scale_x_log10()+
#   scale_y_log10()+
#   facet_wrap(~lnd_cat)+
#   theme(legend.position = "none",
#         strip.background = element_blank(),
#         strip.text.x = element_text(size = 12, hjust = 0),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title = element_text(size=14))
# 
# # Role of landscape heterogeneity
# 
# dat %>% 
#   filter(lnd_cat %in% c("Mixed covers")) %>%
#   filter(cum_totco2g_m2_day>0)%>%
#   ggplot(aes(TOT_BASIN_AREA,cum_totco2g_m2_day, color = h_rel_3))+
#   xlab("Watershed Area")+
#   ylab("Cummulative respiration rates")+
#   # geom_point(alpha = 0.05)+
#   geom_point()+
#   geom_abline(slope = 1, color = "darkred",linetype = "longdash", size =0.75)+
#   scale_x_log10()+
#   scale_y_log10()
# 
# # Entropy plot
# 
# with(dat,scatter3D(tot_other_3,tot_ntrl_3,h_rel_3,bty ='b2',
#                    colvar = as.integer(lnd_cat),
#                    clab=c("Relative","Entropy"), 
#                    theta = 15, phi =20, main = "Landscape Heterogeneity", 
#                    xlab = "Mixed cover", ylab = "Natural cover",
#                    zlab = "Relative entropy"))
# plotrgl()