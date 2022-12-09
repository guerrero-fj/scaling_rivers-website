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

rm()

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
bgc_cln0 <- read.csv("data/221206_scaling_lnd_bgc.csv", 
                  stringsAsFactors=TRUE)
bgc_cln <- as_tibble(na.omit(bgc_cln0))# Removing all NA's 


################################################################################
# Figures
################################################################################

# Let's start with a quick pairs plot to have a glimpse of the relationships among
# variables

dat_pair <- as_tibble(select(bgc_cln,acm_resp,river_lgt,reach_area,wshd_area,d50m,order,
                             hrt))

paired_plot <- select(bgc_cln,
                       acm_resp,
                       river_lgt,
                       reach_area,
                       wshd_area,
                       d50m,order,
                       hrt) %>% 
  mutate(log_resp = log10(acm_resp)) %>% 
  mutate(log_lgt = log10(river_lgt)) %>% 
  mutate(log_r_area =log10(reach_area)) %>% 
  mutate(log_w_area = log10(wshd_area)) %>% 
  mutate(log_d50m = log10(d50m)) %>% 
  select(c(6:12)) %>% 
  ggpairs()
paired_plot


# Main chart (drafts)

# Creating breaks for logarithmic scale (see: https://r-graphics.org/recipe-axes-axis-log)

breaks = 10^(-2:5)
minor_breaks = rep(1:9,21)*(10^rep(-2:5,each=9))

# landscape heterogeneity
# Guiding lines


# let's now create a categorical variable for residence time using the quartiles

bgc_cln$rt_cat <- cut(bgc_cln$res_time,
                      breaks=c(1,4,4.6,4.8,5,5.2,5.4,5.6,5.8,6,7,8,9),
                      labels = c("HZ<","b","c","d","e","f","g","h","i","j","k","l"))

s_breaks <- c(2.0,  3.70, 3.95, 4.20, 4.45, 4.70, 4.95, 5.20, 5.45, 5.70, 5.95,
              6.20, 6.45, 6.70, 6.95, 7.20, 9.00)
              

s_labels0 <-c("Rt_","Rt_","Rt_","Rt_","Rt_","Rt_","Rt_","Rt_","Rt_","Rt_","Rt_",
              "Rt_","Rt_","Rt_","Rt_","Rt_","Rt_")
              

s_labels1 <- paste(s_labels0, s_breaks, sep = '')

s_labels <- s_labels1[2:17]

bgc_cln$srt_cat <- cut(bgc_cln$res_time,
                       breaks = s_breaks,
                       labels = s_labels)

# Creating a quasi-sequential color palette for discrete categories
# Source: https://www.ibm.com/design/language/color/

my_dcolors <- c("#edf5ff","#e5f6ff","#d0e2ff","#bae6ff","#a6c8ff","#82cfff","#78a9ff",
                "#33b1ff","#4589ff","#1192e8","#0f62fe","#0072c3","#0043ce","#00539a",
                "#002d9c","#003a6d","#001d6c","#012749","#001141","#061727")





p <- ggplot(bgc_cln,aes(wshd_area,acm_resp,color=rt_cat))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope=1.0)+
  facet_wrap(~rt_cat)
p

p <- ggplot(bgc_cln,aes(wshd_area,acm_resp,color=srt_cat))+
  geom_point(alpha=0.7)+
  geom_smooth(method="lm",se=FALSE, fullrange=TRUE,linetype="dashed",size=0.45)+
  scale_color_manual(values = my_dcolors)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope=1.0, color = "red")+
  facet_wrap(~srt_cat)
p

# relationship between residence time and slope
p <- ggplot(bgc_cln,aes(reach_slp,res_time))+
  geom_point()+
  geom_smooth()
p

# Calculating slopes, intercepts, and regression significance for multiple levels
# of residence time

# Creating a matrix for results

# Stratified Resampling

stratified <- bgc_cln %>% 
  group_by(srt_cat) %>% 
  slice_sample(n=ssz,replace=TRUE) %>% 
  mutate(slope = as.numeric(summary(lm(log(acm_resp)~log(wshd_area)))$coefficients[2]))

p <- ggplot(stratified,aes(hrt,slope))+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_point()
p

stratified_a <- bgc_cln %>% 
  group_by(rt_cat) %>% 
  slice_sample(n=ssz,replace=TRUE) %>% 
  mutate(slope = as.numeric(summary(lm(log(acm_resp)~log(wshd_area)))$coefficients[2])) %>% 
  ggplot(aes(hrt,slope))+
  # scale_x_log10()+
  # scale_y_log10()+
  geom_point()
stratified_a



p <- ggplot(stratified,aes(wshd_area,acm_resp,color = srt_cat))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",se=FALSE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope=1,color = "red", linetype = "dashed", size = 1.2)+
  facet_wrap(~srt_cat)
p

gb <- ggplot_build(p)

# Number of iterations 
rtc = levels(bgc_cln$srt_cat)
cls = 12
itn = 1:1000

ncols = 11
nrows = length(itn)
ssz = 100


# Make an empty tibble with all the column headers, plus the iteration.
scl_results <- tibble(category = as.character(),
                          iteratn = as.numeric(),
                          wsa_avg = as.numeric(),
                          rsp_avg = as.numeric(),
                          hzt_avg = as.numeric(),
                          ant_avg = as.numeric(),
                          frt_avg = as.numeric(),
                          shb_avg = as.numeric(),
                          hrt_avg = as.numeric(),
                          scl_exp = as.numeric(),
                          scl_int = as.numeric(),
                          pvl_exp = as.numeric(),
                          pvl_mod = as.numeric(),
                          r_sqard = as.numeric(),
                          rch_slp = as.numeric(),
                          riv_slp = as.numeric(),
                          res_mdn = as.numeric())

for (i in rtc){
  for( j in itn){
    dat <- bgc_cln %>% filter(srt_cat==i)
    tst = dat[sample(nrow(dat),size=ssz,replace = TRUE),]
    sm <- lm(log(acm_resp)~log(wshd_area),data =tst)
    st <- summary(sm)
    rsq <- as.numeric(st$r.squared)
    int <- as.numeric(sm$coefficients[1])
    slp <- as.numeric(sm$coefficients[2])
    mrs <- as.numeric(median(sm$residuals))
    p_slp <- as.numeric(st$coefficients[2,4])
    p_mod <- as.numeric(pf(st$fstatistic[1],         
                           st$fstatistic[2], 
                           st$fstatistic[3], 
                           lower.tail = FALSE))
    wsa <- mean(tst$wshd_area)
    rsp <- mean(tst$acm_resp)
    hzt <- mean(tst$res_time)
    ant <- mean(tst$p_ant_t)
    frt <- mean(tst$p_frt_t)
    shb <- mean(tst$p_shb_t)
    hrt <- mean(tst$hrt)
    ssl <- mean(tst$reach_slp)
    rsl <- mean(tst$river_slp)
    row <- tibble(category = i, 
                  iteratn =j, 
                  wsa_avg = wsa,
                  rsp_avg = rsp,
                  hzt_avg = hzt,
                  ant_avg = ant,
                  frt_avg = frt,
                  shb_avg = shb,
                  hrt_avg = hrt,
                  scl_int = int,
                  scl_exp = slp,
                  pvl_exp = p_slp,
                  pvl_mod = p_mod,
                  r_sqard = rsq,
                  rch_slp = rsp,
                  riv_slp = rsl,
                  res_mdn = mrs)
   scl_results <- scl_results %>% add_row(row)
  }
}

scl_lines <- scl_results %>% 
  select(category,scl_exp,scl_int,r_sqard,pvl_exp,pvl_mod,hzt_avg) %>% 
  filter(r_sqard>0.8 & pvl_mod < 0.001) %>% 
  group_by(category) %>% 
  summarise(m_scl_exp = mean(scl_exp),
            m_scl_int = mean(scl_int),
            m_rsq = mean(r_sqard),
            m_pex = mean(pvl_exp),
            m_pmd = mean(pvl_mod),
            m_hzt = mean(hzt_avg))

p_exp <- ggplot(scl_lines,aes(m_hzt,m_scl_exp))+
  geom_point()+
  geom_smooth(span = 0.95)+
  xlab("Average Residence time (1k-resampling)")+
  ylab("Average Scaling Exponent (1k-resampling)")+
  geom_hline(yintercept = 1.0,linetype = "dashed", size = 1.0, color = "red")
p_exp

p_int <- ggplot(scl_lines,aes(m_hzt,m_scl_int))+
  geom_point()+
  geom_smooth(span = 0.95)+
  xlab("Average Residence time (1k-resampling)")+
  ylab("Average Intercept (1k-resampling)")+
  geom_hline(yintercept = 0.0,linetype = "dashed", size = 1.0, color = "red")
p_int

p_int <- ggplot(scl_lines,aes(m_hzt,m_scl_exp))+
  geom_point()+
  geom_smooth()
p_int


p_mres <- ggplot(scl_results,aes(x = category, y = scl_exp))+
  geom_boxplot()+
  geom_point(aes(alpha=0.002))+
  geom_smooth()+
  scale_y_log10()+
  # scale_color_manual(values = my_dcolors)+
  geom_hline(yintercept = 1,color="red")
p_mres
  
  
  
###############################################################################
#FIGURES
###############################################################################
# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))


 p0 <- ggplot(bgc_cln,aes(wshd_area, acm_resp, color = rt_cat))+
     # geom_point()+
     geom_smooth(method = "lm", se=FALSE)+
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
     # scale_color_distiller("Hyporheic\nexchange",palette = "Blues",direction = 1,
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
                     plot.background = element_blank())
p0


p <- ggplot(scl_results,aes(hzt_avg,scl_exp))+
  geom_point()+
  geom_smooth()
p


p1 <- ggplot(bgc_cln,aes(wshd_area, acm_resp, color = hrt))+
  geom_point()+
  geom_point(aes(alpha = hrt), size = 2.5)+
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks, 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  annotation_logticks(size = 0.75, sides = "tblr")+
  scale_color_distiller("Landscape\nheterogeneity",palette = "Blues",direction = 1,
                        breaks = c(0.0,0.25,0.50, 0.75,1.0), limits =c(0,1))+
  geom_abline(slope =1.00, size = 0.75, linetype = "dashed", color="red")+
  geom_smooth()
  # geom_abline(slope =1.02, intercept =  2.63, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.33, intercept =  0.88, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.61, intercept = -0.70, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.77, intercept = -1.68, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =1.94, intercept = -4.18, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.04, intercept = -6.72, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.09, intercept = -5.32, size = 0.75, linetype = "dashed")+
  # geom_abline(slope =2.14, intercept = -7.85, size = 0.75, linetype = "dashed")+
  guides(alpha = "none")+
  theme_httn+
  theme(axis.text=element_text(colour="black",size=16),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 16),
        plot.background = element_blank())
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


# # Entropy plot
# 
# with(dat,scatter3D(tot_other_3,tot_ntrl_3,h_rel_3,bty ='b2',
#                    colvar = as.integer(lnd_cat),
#                    clab=c("Relative","Entropy"), 
#                    theta = 15, phi =20, main = "Landscape Heterogeneity", 
#                    xlab = "Mixed cover", ylab = "Natural cover",
#                    zlab = "Relative entropy"))
# plotrgl()