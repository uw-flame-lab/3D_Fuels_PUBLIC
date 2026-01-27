---
# title: "Biomass MS figures and tables"
# author: "Deborah Nemens"
# date: "2025-11-3"
# output: 
#   word_document: 
#   fig_width: 6.5
# fig_caption: true
# fig_height: 7
# always_allow_html: true
# reference_docx: "MD.template.docx"
# fontsize: 12pt
---

## Load dependencies 
library(tinytex)
library(corrplot)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(kableExtra)
library(rmarkdown)
library(webshot2)
library(magick)
library(deeptime)
library(patchwork)
library(gt)
 
#########################
### Set Dirs & load data
# Input folder path: 
input <- "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/global_inputs/"

# Output folder path: 
output <- "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/analysis/biomass_PC_comp/plots/"

# csv input - load biomass, tls & sfm data (with coarse woody debris plots removed)
data <- read.csv(paste0(input, "biomass_PC_comp//combined_Metrics.csv"))

#Remove POST plots for analysis
data <- data %>% filter(Visit == "PRE")
#remove Timber litter dominated plots, most of these are NA's
data <- filter(data, biomass_10to100cm > 0, !DominantFuel_Occ_10 %in% "Timber_Litter")
#remove 2 outliers (Methow plot with 667g biomass_10+, 429g Fort Stewart POST plot with small tree) 
data <- data %>% filter(biomass_10to100cm<400)
#remove shrub-dominated SYG plot outlier
data <- data %>% filter(!biomass_10to100cm %in% 124.70)

####################
### Figure 1: Correlation Matrix for seven TLS metrics, three field-measured metrics, and 2 measures of plot biomass

#create correlation matrix of TLS metrics vs biomass
ttab <- cor(data[,c(4:5, 13:22)], use = "complete.obs")
par(mar = c(.1, .1, .1, 1)) # Bottom, Left, Top, Right
#correlogram 
tlsCor <- corrplot(ttab, type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 1.5, cl.cex = 1.2, number.cex = 1.5, method = "number")
 

### Figure 2: Correlation Correlation Matrix for seven SfM metrics, three field-measured metrics, and 2 measures of plot biomass

#create correlation matrix of SFM metrics vs biomass
stab <- cor(data[,c(4:5, 6:12, 20:22)], use = "complete.obs")
par(mar = c(.1, .1, .1, 1)) # Bottom, Left, Top, Right
#correlogram 
sfmCor <- corrplot(stab, type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 1.5, cl.cex = 1.2, number.cex = 1.5, method = "number")
 

#####################
### basic plot params

#basic plot params for single categories
plottyA <- list(
  geom_point(size = 1), 
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.5, alpha = 0.1, linetype = "dashed"),
  stat_smooth(method = "lm", formula = y ~ x, col = "#9cc1db", alpha = 0.3), 
  stat_cor(aes(group = 1, label = paste(..rr.label.., ..p.label.., sep = "~','~")), label.x = 0, label.y = 350, size = 4),
  theme_bw(), ylab("Biomass (g)"),
  theme(axis.text = element_text(size = 10), legend.position = "none"))

#plot params for sites
plotty_a <- list(
  geom_point(size = 2, alpha = .5), 
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.5, alpha = 0.1, linetype = "dashed"),
  stat_smooth(method = "lm", formula = y ~ x, aes(group = 1), col = "#9cc1db", alpha = 0.3), 
  theme_bw()+theme(axis.title = element_text(size = 12), axis.text = element_text(size = 6), 
                   strip.text = element_text(size = 7, face="bold"), legend.position = "none"),
  stat_cor(aes(group = 1, label = paste(..rr.label..)), color = "black", label.x = .01, label.y = 370, size = 3, r.accuracy = 0.01), 
  facet_wrap(~factor(Site, levels = c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest", "Sycan Forest" , "Methow", "Sycan Grassland", "LANL Grassland", "Tenalquot",  "Glacial Heritage")), nrow = 1),
  coord_cartesian(ylim = c(0, 400))) 

layout <- "
AAAAAAAA
#BBBBBB#
#CCCCCC#
#DDDDDD#
"
layout2 <- "
AAAAAAAA
#BBBBBB#
#CCCCCC#
#DDDDDD#
EEEEEEEE
#FFFFFF#
#GGGGGG#
#HHHHHH#
"
#plot params for groups
plotty_g <- list(
  geom_point(size = 2, alpha = .5), 
  scale_fill_manual(values = c("darkgoldenrod", "blue", "darkred", "mediumorchid")),
  stat_smooth(method = "lm", formula = y ~ x, linewidth = 0.5, alpha = 0.1, linetype = "dashed"),
  stat_smooth(method = "lm", formula = y ~ x, aes(group = 1), col = "#9cc1db", alpha = 0.3), 
  theme_bw()+theme(axis.title = element_text(size = 10), axis.text = element_text(size = 7), 
                   strip.text = element_text(size = 7, face="bold"), legend.position = "none"),
  stat_cor(aes(group = 1, label = paste(..rr.label..)), color = "black", label.x = .01, label.y = 340, size = 8),
  facet_wrap(~factor(SiteType)),
  coord_cartesian(ylim = c(0, 400))) 
 
################
### Figure 3: TLS and SfM models of average height and field-collected plot biomass above 10 cm
## GlobAL MODELS ###

theight <- lm(biomass_10to100cm ~ tlsavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(theight)/df.residual(theight)), 2))
tHtAll <- ggplot(data, aes(y = biomass_10to100cm, x = tlsavheight))+
  xlab("TLS Average Height (m)")+ ylab("Biomass (g)")+
  plottyA

sheight <- lm(biomass_10to100cm ~ sfmavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(sheight)/df.residual(sheight)), 2))
sHtAll <- ggplot(data, aes(y = biomass_10to100cm, x = sfmavheight))+
  xlab("SfM Average Height (m)")+ ylab("Biomass (g)")+
  plottyA

HtAll <- grid.arrange(arrangeGrob(tHtAll, sHtAll, nrow = 2))
 
###
## Figure 4: TLS and SfM models of occupied volume and field-collected plot biomass above 10 cm ###

### occupied volume plot of all veg & fuel types ###
povA <-  lm(biomass_10to100cm ~ tlsov, data = data)
OV <- ggplot(data, aes(y = biomass_10to100cm, x = tlsov))+
  xlab("TLS occupied volume")+ ylab("Biomass (g)")+
  plottyA

spovA <-  lm(biomass_10to100cm ~ sfmov, data = data)
sOV <- ggplot(data, aes(y = biomass_10to100cm, x = sfmov,))+
  xlab("SfM occupied volume")+ ylab("Biomass (g)")+
  plottyA

OVAll <- grid.arrange(arrangeGrob(OV, sOV, nrow = 2))

 

### Figure 5: TLS and SfM models of hull surface area and field-collected plot biomass above 10 cm

### hull SA - plot of all veg & fuel types ###
tSA <-  lm(biomass_10to100cm ~ tlsSA, data = data)
SA <- ggplot(data, aes(y = biomass_10to100cm, x = tlsSA))+
  xlab("TLS Surface area")+ ylab("Biomass (g)")+
  plottyA

sSA <-  lm(biomass_10to100cm ~ sfmSA, data = data)
sSA <- ggplot(data, aes(y = biomass_10to100cm, x = sfmSA,))+
  xlab("SfM Surface area")+ ylab("Biomass (g)")+
  plottyA

sSA<- grid.arrange(arrangeGrob(SA, sSA, nrow = 2))
 

### Figure 6: TLS and SfM models of hull volume and field-collected plot biomass above 10 cm

## Hull vol tls vs sfm models ####
#vol - tls
tv <-  lm(biomass_10to100cm ~ tlshullvol, data = data)
tVol <- ggplot(data, aes(y = biomass_10to100cm, x = tlshullvol))+
  xlab("TLS hull volume")+ ylab("Biomass (g)")+ 
  plottyA

#vol - SfM
sv <-  lm(biomass_10to100cm ~ sfmhullvol, data = data)
svol <- ggplot(data, aes(y = biomass_10to100cm, x = sfmhullvol))+
  xlab("SfM hull volume")+ ylab("Biomass (g)")+ 
  plottyA

Vol <- grid.arrange(arrangeGrob(tVol, svol, nrow = 2))
 

### Figure 7: TLS and SfM models of average height and field-collected plot biomass above 10 cm, subsetted by site

### Height metrics modeled by plot ###

theight <- lm(biomass_10to100cm ~ tlsavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(theight)/df.residual(theight)), 2))
tHt_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = tlsavheight))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
tHt_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = tlsavheight))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
tHt_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = tlsavheight))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
tHt_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = tlsavheight))+  xlab("TLS Average Height (m)")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
#tHt1 <- grid.arrange(tHt_fw,tHt_ls,tHt_wp,tHt_wg,  widths = c(1, .5, 1, .6), nrow = 3)
HtT <- tHt_fw+tHt_ls+tHt_wp+tHt_wg+ plot_layout(design = layout, axes = "collect")
HtT

sheight <- lm(biomass_10to100cm ~ sfmavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(sheight)/df.residual(sheight)), 2))
sHt_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = sfmavheight))+ plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
sHt_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = sfmavheight))+  plotty_a + 
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
sHt_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = sfmavheight))+  plotty_a  +
  theme(strip.background = element_rect(fill = "#60CC52"), axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
sHt_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = sfmavheight))+xlab("SfM Average Height (m)")+plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"), axis.title.y = element_blank())
sHt1 <- sHt_fw+sHt_ls+sHt_wp+sHt_wg+ plot_layout(widths = c(1, .5, 1, .6), design = layout)
sHt1
HtS <- sHt_fw+sHt_ls+sHt_wp+sHt_wg+ plot_layout(design = layout, axes = "collect")

Hta1 <- tHt_fw+tHt_ls+tHt_wp+tHt_wg+sHt_fw+sHt_ls+sHt_wp+sHt_wg+ plot_layout(design = layout2, axes = "collect")
Hta1

ggsave(paste0(output, "/height.plots.png"), Hta1, width = 5, height = 10, units = "in")

### Figure 8: TLS and SfM models of occupied volume and field-collected plot biomass above 10 cm, subsetted by site

#### OV Models ####
#OV - tls
pov1 <-  lm(biomass_10to100cm ~ tlsov, data = data)
Ov1_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = tlsov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
Ov1_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = tlsov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
Ov1_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = tlsov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
Ov1_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = tlsov))+  xlab("TLS Occupied Volume (%)")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
Ov1 <- Ov1_fw+Ov1_ls+Ov1_wp+Ov1_wg+ plot_layout(widths = c(1, .5, 1, .6), design = layout)
Ov1

#OV - SfM
spov1 <-  lm(biomass_10to100cm ~ sfmov, data = data)
sOv1_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = sfmov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
sOv1_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = sfmov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
sOv1_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = sfmov))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
sOv1_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = sfmov))+  xlab("SfM Occupied Volume (%)")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
sOv1 <- sOv1_fw+sOv1_ls+sOv1_wp+sOv1_wg+ plot_layout(widths = c(1, .5, 1, .6), design = layout)
sOv1

Ova1 <- Ov1_fw+Ov1_ls+Ov1_wp+Ov1_wg+sOv1_fw+sOv1_ls+sOv1_wp+sOv1_wg+ plot_layout(design = layout2, axes = "collect")
Ova1

#Ova1 <- grid.arrange(arrangeGrob(Ov1, sOv1, nrow = 2, heights = c(4.5,4.8)))
 

### Figure 9: TLS and SfM models of hull surface area and field-collected plot biomass above 10 cm, subsetted by site

## Hull SA tls vs sfm Models ####
#SA - tls
tS1 <-  lm(biomass_10to100cm ~ tlsSA, data = data)
SA1_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = tlsSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
SA1_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = tlsSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
SA1_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = tlsSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
SA1_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = tlsSA))+  xlab("TLS Surface Area")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
SAt <- SA1_fw+SA1_ls+SA1_wp+SA1_wg+ plot_layout(design = layout)
SAt

#SA - SfM
sA1 <-  lm(biomass_10to100cm ~ sfmSA, data = data)
sSA1_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = sfmSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
sSA1_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = sfmSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
sSA1_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = sfmSA))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
sSA1_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = sfmSA))+  xlab("SfM Surface Area")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
SAs <- sSA1_fw+sSA1_ls+sSA1_wp+sSA1_wg+ plot_layout(design = layout)
SAs

SA1 <- SA1_fw+SA1_ls+SA1_wp+SA1_wg+sSA1_fw+sSA1_ls+sSA1_wp+sSA1_wg+ plot_layout(design = layout2, axes = "collect")
SA1
#SA1 <- grid.arrange(arrangeGrob(tSA1, sSA1, nrow = 2, heights = c(4.5,4.8)))
 

### Figure 10: TLS and SfM models of hull volume and field-collected plot biomass above 10 cm by site

#hull vol - tls
tv <-  lm(biomass_10to100cm ~ tlshullvol, data = data)
Hv_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = tlshullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
Hv_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = tlshullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
Hv_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = tlshullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
Hv_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = tlshullvol))+  xlab("TLS Hull Volume")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
HvT <- Hv_fw+ Hv_ls + Hv_wp + Hv_wg +plot_layout(design = layout, axes = "collect")
HvT

#hull vol - SfM
sv <-  lm(biomass_10to100cm ~ sfmhullvol, data = data)
sHv_fw <- ggplot(subset(data, SiteType == "SE flatwood"), aes(y = biomass_10to100cm, x = sfmhullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#BA9DC6", alpha=.5),axis.title = element_blank()) 
sHv_ls <- ggplot(subset(data, SiteType == "SE Loblolly-sweetgum"), aes(y = biomass_10to100cm, x = sfmhullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#7EB0CB"),axis.title = element_blank())
sHv_wp <- ggplot(subset(data, SiteType == "Western pine"), aes(y = biomass_10to100cm, x = sfmhullvol))+  plotty_a +
  theme(strip.background = element_rect(fill = "#60CC52"),axis.title.x = element_blank(), axis.title.y = element_text(size = 12, hjust = -1)) + ylab("Biomass (g)")
sHv_wg <- ggplot(subset(data, SiteType == "Western grassland"), aes(y = biomass_10to100cm, x = sfmhullvol))+  xlab("SfM Hull Volume")+  plotty_a +
  theme(strip.background = element_rect(fill = "#FFE099"),axis.title.y = element_blank()) 
HvS <- sHv_fw+ sHv_ls + sHv_wp + sHv_wg +plot_layout(design = layout)
HvS

Vol <- Hv_fw+ Hv_ls + Hv_wp + Hv_wg + sHv_fw+ sHv_ls + sHv_wp + sHv_wg +plot_layout(design = layout2, axes = "collect")
Vol
#Vol <- grid.arrange(arrangeGrob(tVol, svol, nrow = 2, heights = c(4.5,4.8)))
 

## Figure 11: Comparison plots of fuel height and occupied volume as measured in the field with average height and volume metrics calculated from TLS and SfM point clouds. Data are grouped by fuel type (herb-dominated and shrub-dominated) and vegetation type: SE Flatwood (FW), Loblloly-sweetgum (LS), Western Pine (WP), and Western grassland (WG) ####
### prep data for boxplots
data_b <- data[,c(7:8, 14:15, 21:22, 23, 25)]

### field Ht 
# Boxplot
data_ht <- pivot_longer(data_b, cols = c(tlsavheight, sfmavheight, FieldHeight_avg), names_to = "data_type", values_to = "height")
fHt <- ggplot(data=data_ht, aes(y = height, x = factor(SiteType), fill = data_type))+
  geom_boxplot()+
  labs(y = "Average height (cm)", x = "", colour = "")+
  theme_bw() + theme(axis.title = element_text(size = 20), axis.text = element_text(size = 18), strip.text = element_text(size = 18, face="bold"), 
                     legend.text = element_text(size = 20),legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 30, vjust = 0.6))+
    scale_x_discrete(labels = c("Flatwood", "Loblolly", "W Pine", "W Grassland"))+
  scale_fill_manual(values = c("#60CC52", "#BA9DC6", "#FFE099"), labels = c("Field", "SFM", "TLS"))+
  facet_wrap(~DominantFuel_Occ_10)
fHt

# smoothed line + scatterplot
data_ht2 <- pivot_longer(data_b, cols = c(tlsavheight, sfmavheight), names_to = "data_type", values_to = "height")
data_ht2$data_type <- factor(data_ht2$data_type, levels  = c("tlsavheight", "sfmavheight"))
data_ht2$SiteType <- factor(data_ht2$SiteType, levels = c("SE flatwood", "SE Loblolly-sweetgum", "Western pine", "Western grassland"))

b_ht <- ggplot(data_ht2, aes(x = FieldHeight_avg, y = height, color = data_type))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = after_stat(rr.label), color = data_type), label.x = 0, size = 3.5)+
  scale_color_manual(values = c(tlsavheight = "steelblue", sfmavheight = "purple4"))+
  labs(y = "RS height (m)", x = "Measured height (m)", color=NULL)+
  facet_grid(rows = vars(SiteType), cols = vars(DominantFuel_Occ_10))+
  coord_cartesian(y = c(0,1))+
  theme_bw() +
  theme(legend.position = "none")
b_ht
 
##plotty_a### field OV
data_ov <- pivot_longer(data_b, cols=c(tlsov, sfmov, fieldOv), names_to = "data_type", values_to = "volume")

fOv <- ggplot(data=data_ov, aes(y = volume, x = factor(SiteType), fill = data_type))+
  geom_boxplot()+
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())+
  labs(y = "Occupied volume (%)", x = "", colour = "")+
  scale_x_discrete(labels = c("Flatwood", "Loblolly", "W Pine", "W Grassland"))+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 18), strip.text = element_text(size = 18, face="bold"), 
        legend.text = element_text(size = 18), axis.text.x = element_text(angle = 30, vjust = 0.6))+
  scale_fill_manual(values = c("#60CC52", "#BA9DC6", "#FFE099"), labels = c("Field", "SFM", "TLS"))+
  facet_wrap(~DominantFuel_Occ_10)
fOv

data_ov2 <- pivot_longer(data_b, cols = c(tlsov, sfmov), names_to = "data_type", values_to = "volume")
data_ov2$data_type <- factor(data_ov2$data_type, levels  = c("tlsov", "sfmov"))
data_ov2$SiteType <- factor(data_ov2$SiteType, levels = c("SE flatwood", "SE Loblolly-sweetgum", "Western pine", "Western grassland"))

b_ov <- ggplot(data_ov2, aes(x = fieldOv, y = volume, color = data_type))+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = after_stat(rr.label), color = data_type), label.x = -.05, size = 3.5)+
  scale_color_manual(values = c(tlsov = "steelblue", sfmov = "purple4"), labels = c("TLS", "SfM"))+
  labs(y = "RS volume (%)", x = "Measured volume (%)", color=NULL)+
  facet_grid(rows = vars(SiteType), cols = vars(DominantFuel_Occ_10))+
  coord_cartesian(y = c(0,1.05))+
  theme_bw() 
b_ov

grid.arrange(arrangeGrob(fHt, fOv, nrow = 2, heights = c(15, 18)))

grid.arrange(arrangeGrob(b_ht, b_ov, ncol = 2))

## Anova 
ht <- aov(data_ov$height~data_ht$data_type)
summary(ht)
TukeyHSD(ht)

vol <- aov(data_ov$volume~data_ov$data_type) 
summary(vol)
TukeyHSD(vol)
##################
## Violin/Density plots of biomass 0-10/10-100
########
# prep biomass data for plots
data_bio <- data %>% 
  mutate(biomass0to10 = biomass0to100-biomass_10to100cm)  %>% 
  select (SiteType, Site, ClipPlot, SiteType, biomass0to10, biomass_10to100cm, biomass0to100) %>%
  rename(below10 = biomass0to10, above10 = biomass_10to100cm, total = biomass0to100) %>%
  as.data.frame()

#prep for table
db_sum <- data_bio %>% 
 mutate(perBelow=(below10/(above10+below10)), perAbove = above10/(above10+below10)) %>%
  group_by(Site) %>%
  summarise('Below10cm' = mean(perBelow*100), 'Above10cm' = mean(perAbove*100), 'TotalBiomass' = mean(total)) %>%
  arrange(factor(Site, levels = c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart",
        "Tates Hell B", "Aucilla" , "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest", 
        "Sycan Forest", "Methow", "Sycan Grassland", "LANL Grassland", "Tenalquot",  "Glacial Heritage")))

#### Table S1 ####

Table_S1 <- db_sum %>% gt() %>%
  tab_style(cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  fmt_number(decimals = 2) %>%
  cols_label(Site = "Site",Below10cm=html("Below 10cm (%)"),Above10cm = html("Above 10cm (%)"),TotalBiomass="Mean total biomass (g)")%>%
  tab_row_group(label = "Western grasslands", rows = 14:17)%>%
  tab_row_group(label = "Western pine", rows = 10:13) %>%
  tab_row_group(label = "Loblolly-sweetgum forests", rows = 7:9) %>%
  tab_row_group(label = "Southeastern flatwood forests", rows = 1:6) %>%
tab_style(style = cell_text(align = "center"), locations = cells_row_groups(everything()))
Table_S1

gtsave(Table_S1, "C:/Users/dnemens/Documents/3D Fuels/Analysis/Table.S1.png")

Table2 <- TLS.mods %>% gt() %>%
  tab_style(cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  fmt_number(decimals = 2, columns = c(2:3, 5:6, 8:9, 11:12)) %>%
  cols_label(Plot = "Site", r2="r\U00B2", RMSE = "RMSE",pvalue="pvalue", Vr2="r\U00B2", VRMSE="RMSE",Vpvalue = "p-value", 
             Sr2="r\U00B2",SRMSE="RMSE", Spvalue = "pvalue", Hr2 = "r\U00B2", HRMSE="RMSE",Hpvalue="p-value")%>%
  tab_spanner(label = "Average height", columns = c(2:4)) %>%
  tab_spanner(label = "Occupied volume", columns = c(5:7)) %>%
  tab_spanner(label = "Surface area", columns = c(8:10)) %>%
  tab_spanner(label = "Hull volume", columns = c(11:13)) %>%
  tab_row_group(label = "Western grasslands", rows = 14:17)%>%
  tab_row_group(label = "Western forests", rows = 10:13) %>%
  tab_row_group(label = "Loblolly-sweetgum forests", rows = 7:9) %>%
  tab_row_group(label = "Southeastern flatwood forests", rows = 1:6) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_row_groups(everything()))
Table2
#####

library(vioplot)

## split violin plots
  par(mar = c(1, 3, 2, 1.9))
  vioplot(below10~SiteType, data = data_bio, side= "left", col = "tan4", xlab = "Biomass (g)", ylab = " ", horizontal = T, tcl = 0)
  vioplot(above10~SiteType, data = data_bio, side="right", col = "#60CC52", add = T, horizontal = T)
  axis(side = 2, labels = c("SE flatwood","SE Loblolly sweetgum", "Western grassland", "Western pine"), at = c(1,2,3,4))
  title(xlab = "Biomass (g)")        
  legend("bottomright", legend = c("Above 10cm","Below 10cm"), fill = c("#60CC52","tan4"))

## Density plots
data_bio2 <- data_bio %>% pivot_longer(cols = 4:5, names_to = "location", values_to = "biomass")
ggplot(data_bio2, aes(x=biomass, fill = location))+
  geom_density(position = "fill") +
  theme_bw()+ theme(legend.text = element_text(size=13), strip.text = element_text(size=15))+
  xlab("Biomass (g)") + ylab("Density")+ theme(legend.title = element_blank(), 
                                                axis.ticks.y = element_blank(), legend.position = "bottom") +
  scale_fill_manual(values = c("#60CC52","tan4"), labels = c("Above 10cm","Below 10cm"))+
  facet_wrap(~SiteType)

ggsave(paste0(output, "bio.density.plot.png"), width = 8, height = 7, units = "in")

## Stacked violin plots
ggplot(data_bio2, aes(y=biomass, x = SiteType, fill = location))+
  geom_violin()+
  scale_fill_manual(values = c("#60CC52","tan4"), labels=c("Above 10cm","Below 10cm"))+
  ylab("Biomass (g)") + xlab("")+ theme(legend.title = element_blank()) +
  coord_flip()

############# TABLES ###############
## Function for calculating root mean square error ####
rmse_lm <- function(model) {
  if (!inherits(model, "lm")) {
    stop("The input must be a linear model object of class 'lm'.")
  }
  residuals <- resid(model)
  sqrt(mean(residuals^2, na.rm = TRUE))
}

## TLS Models ####
## HEIGHT MODELS ##
mod <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ tlsavheight, data = subset(data, Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest ", 
                           "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, 0), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, 
                          rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, 0), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8],
                            s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], 0))
TLS.Ht <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

### OccVol MODELS ###
mod <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ tlsov, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ tlsov, data = subset(data, Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest ", 
                           "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, 0), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, 
                          rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, 0), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8],
                            s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], 0))
TLS.Ov <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

### Hull volume MODELS ###
mod <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11) 
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ tlsSA, data = subset(data, Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest ", 
                           "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, 0), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, 
                          rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, 0), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8],
                            s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], 0))
TLS.SA <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))
####
### Hull volume MODELS ###
mod <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ tlshullvol, data = subset(data, Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest ", 
                           "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, 0), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, 
                          rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, 0), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8],
                            s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], 0))
TLS.Hv <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

#######
# combine all for lg table
TLS.mods <- cbind(TLS.Ht, TLS.Ov[2:4], TLS.SA[2:4], TLS.Hv[2:4])
 

## Table 2: TLS linear model outputs (n = 561 plots) ####
colnames(TLS.mods) <- c("Plot", "r2","RMSE","pvalue","Vr2","VRMSE","Vpvalue","Sr2","SRMSE","Spvalue",
                       "Hr2","HRMSE","Hpvalue")
Table2 <- TLS.mods %>% gt() %>%
  tab_style(cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  fmt_number(decimals = 2, columns = c(2:3, 5:6, 8:9, 11:12)) %>%
  cols_label(Plot = "Site", r2="r\U00B2", RMSE = "RMSE",pvalue="pvalue", Vr2="r\U00B2", VRMSE="RMSE",Vpvalue = "p-value", 
             Sr2="r\U00B2",SRMSE="RMSE", Spvalue = "pvalue", Hr2 = "r\U00B2", HRMSE="RMSE",Hpvalue="p-value")%>%
  tab_spanner(label = "Average height", columns = c(2:4)) %>%
  tab_spanner(label = "Occupied volume", columns = c(5:7)) %>%
  tab_spanner(label = "Surface area", columns = c(8:10)) %>%
  tab_spanner(label = "Hull volume", columns = c(11:13)) %>%
  tab_row_group(label = "Western grasslands", rows = 14:17)%>%
  tab_row_group(label = "Western pine", rows = 10:13) %>%
  tab_row_group(label = "Loblolly-sweetgum forests", rows = 7:9) %>%
  tab_row_group(label = "Southeastern flatwood forests", rows = 1:6) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_row_groups(everything()))
Table2

gtsave(Table2, ("C:/Users//dnemens//Documents//3D Fuels//Table2.TLS.mods.png"))
gtsave(Table2, "C:/Users/dnemens/Documents/3D Fuels/Analysis/Table2.TLS.models.png")

#########################
## SFM Models ####
## HEIGHT MODELS ##
mod <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ sfmavheight, data = subset(data,  Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest", "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, s15$r.squared), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, rmse.mod16), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8], s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], s16$coefficients[8]))

sfm.Ht <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

### OccVol MODELS ###
## simple linear model
mod <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ sfmov, data = subset(data,  Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest", "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, s15$r.squared), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, rmse.mod16), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8], s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], s16$coefficients[8]))

sfm.Ov <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

## Surface area MODELS ###
mod <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ sfmSA, data = subset(data,  Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest",  "Methow","Sycan Forest", "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, s15$r.squared), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, rmse.mod16), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8], s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], s16$coefficients[8]))

sfm.SA <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

### Hull volume MODELS ###
## simple linear model
mod <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Tates Hell A"))
rmse.mod <- rmse_lm(mod)
s <- summary(mod)
mod1 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Osceola"))
rmse.mod1 <- rmse_lm(mod1)
s1 <- summary(mod1)
mod2 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Blackwater"))
rmse.mod2 <- rmse_lm(mod2)
s2 <- summary(mod2)
mod3 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Fort Stewart"))
rmse.mod3 <- rmse_lm(mod3)
s3 <- summary(mod3)
mod4 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Tates Hell B"))
rmse.mod4 <- rmse_lm(mod4)
s4 <- summary(mod4)
mod5 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Aucilla"))
rmse.mod5 <- rmse_lm(mod5)
s5 <- summary(mod5)
mod6 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Hannah Hammock"))
rmse.mod6 <- rmse_lm(mod6)
s6 <- summary(mod6)
mod7 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Hitchiti B"))
rmse.mod7 <- rmse_lm(mod7)
s7 <- summary(mod7)
mod8 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Hitchiti A"))
rmse.mod8 <- rmse_lm(mod8)
s8 <- summary(mod8)
mod9 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Lubrecht"))
rmse.mod9 <- rmse_lm(mod9)
s9 <- summary(mod9)
mod10 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "LANL Forest"))
rmse.mod10 <- rmse_lm(mod10)
s10 <- summary(mod10)
mod11 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Methow"))
rmse.mod11 <- rmse_lm(mod11)
s11 <- summary(mod11)
mod12 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Sycan Forest"))
rmse.mod12 <- rmse_lm(mod12)
s12 <- summary(mod12)
mod13 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "LANL Grassland"))
rmse.mod13 <- rmse_lm(mod13)
s13 <- summary(mod13)
mod14 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Tenalquot"))
rmse.mod14 <- rmse_lm(mod14)
s14 <- summary(mod14)
mod15 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Glacial Heritage"))
rmse.mod15 <- rmse_lm(mod15)
s15 <- summary(mod15)
mod16 <- lm(biomass_10to100cm ~ sfmhullvol, data = subset(data,  Site == "Sycan Grassland"))
rmse.mod16 <- rmse_lm(mod16)
s16 <- summary(mod16)

df <- data.frame(Site= c(c("Tates Hell A", "Osceola", "Blackwater", "Fort Stewart","Tates Hell B", "Aucilla" , 
                           "Hannah Hammock", "Hitchiti B", "Hitchiti A", "Lubrecht", "LANL Forest", "Methow","Sycan Forest", "LANL Grassland", "Tenalquot",  "Glacial Heritage", "Sycan Grassland")), 
                 r2=c(s$r.squared, s1$r.squared, s2$r.squared, s3$r.squared, s4$r.squared, s5$r.squared, s6$r.squared, s7$r.squared, 
                      s8$r.squared, s9$r.squared, s10$r.squared, s11$r.squared, s12$r.squared, s13$r.squared, s14$r.squared, s15$r.squared, s15$r.squared), 
                 RMSE = c(rmse.mod, rmse.mod1, rmse.mod2, rmse.mod3, rmse.mod4, rmse.mod5, rmse.mod6, rmse.mod7, rmse.mod8, rmse.mod9, rmse.mod10, rmse.mod11, rmse.mod12, rmse.mod13, rmse.mod14, rmse.mod15, rmse.mod16), 
                 pvalue = c(s$coefficients[8],s1$coefficients[8],s2$coefficients[8], s3$coefficients[8], s4$coefficients[8],s5$coefficients[8],s6$coefficients[8], 
                            s7$coefficients[8],s8$coefficients[8],s9$coefficients[8],s10$coefficients[8], s11$coefficients[8], s12$coefficients[8], s13$coefficients[8],s14$coefficients[8], s15$coefficients[8], s16$coefficients[8]))

sfm.Hv <- data.frame(Site=df$Site, round(df[,2:3], 2), pvalue = round(df[,4], 3))

###
####
# combine all for lg table
SFM.mods <- cbind(sfm.Ht, sfm.Ov[2:4], sfm.SA[2:4], sfm.Hv[2:4])

## Table 3: SfM linear model outputs (n = 561 plots) ####
colnames(SFM.mods) <- c("Plot", "r2","RMSE","pvalue","Vr2","VRMSE","Vpvalue","Sr2","SRMSE","Spvalue",
                        "Hr2","HRMSE","Hpvalue")
Table3 <- SFM.mods %>% gt() %>%
  tab_style(cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  fmt_number(decimals = 2, columns = c(2:3, 5:6, 8:9, 11:12)) %>%
  cols_label(Plot = "Site", r2 = "r\U00B2", RMSE = "RMSE", pvalue="pvalue", Vr2="r\U00B2", VRMSE="RMSE",Vpvalue = "p-value", 
             Sr2="r\U00B2" ,SRMSE="RMSE", Spvalue = "pvalue", Hr2 = "r\U00B2", HRMSE="RMSE",Hpvalue="p-value")%>%
  tab_spanner(label = "Average height", columns = c(2:4)) %>%
  tab_spanner(label = "Occupied volume", columns = c(5:7)) %>%
  tab_spanner(label = "Surface area", columns = c(8:10)) %>%
  tab_spanner(label = "Hull volume", columns = c(11:13)) %>%
  tab_row_group(label = "Western grasslands", rows = 14:17)%>%
  tab_row_group(label = "Western pine", rows = 10:13) %>%
  tab_row_group(label = "Loblolly-sweetgum forests", rows = 7:9) %>%
  tab_row_group(label = "Southeastern flatwood forests", rows = 1:6) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_row_groups(everything()))
Table3

gtsave(Table3, paste0(output, "/Table3.SfM.models.png"))
gtsave(Table3, "C:/Users/dnemens/Documents/3D Fuels/Analysis/Table3.SfM.models.png")


#####
### Figure A1: TLS and SfM models of average height and field-collected plot biomass above 10 cm, subsetted by vegetation and fuel type

## metrics modeled by veg and fuel type ###
#### Av Height in groups #####

theight <- lm(biomass_10to100cm ~ tlsavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(theight)/df.residual(theight)), 2))
tHt <- ggplot(data, aes(y = biomass_10to100cm, x = tlsavheight, color = DominantFuel_Occ_10, group = DominantFuel_Occ_10))+
  xlab("TLS Average Height (m)")+ ylab("Biomass 10+ cm (g)")+
  plotty_g+
  facet_grid(~DominantFuel_Occ_10~SiteType, switch = "y")

sheight <- lm(biomass_10to100cm ~ sfmavheight, data = data)
rmse <- paste("RMSE: ", round(sqrt(deviance(sheight)/df.residual(sheight)), 2))
sHt <- ggplot(data, aes(y = biomass_10to100cm, x = sfmavheight, color = DominantFuel_Occ_10))+
  xlab("SfM Average Height (m)")+ ylab("Biomass 10+ cm (g)")+
  plotty_g+
  facet_grid(~DominantFuel_Occ_10~SiteType, switch = "y")

Ht <- grid.arrange(arrangeGrob(tHt, sHt, nrow = 2))
