library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrplot)
library(randomForest)
library(fastshap)
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)


df<-read.csv('D:\\tellsoilbio\\final datta\\TSB.csv')

summary(df)

comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Mites0_5"]
  group2 <- df[df$LanduseSimplified == pair[2], "Mites0_5"]
  p <- wilcox.test(group1, group2)$p.value
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 

ggplot(df, aes(x = LanduseSimplified, y = Mites0_5, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Mites 0~5 cm')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\mites0_5_ttest.tiff',dpi=300,height=8,width=3.5)


comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Mites5_10"]
  group2 <- df[df$LanduseSimplified == pair[2], "Mites5_10"]
  p <- wilcox.test(group1, group2)$p.value
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 

ggplot(df, aes(x = LanduseSimplified, y = Mites5_10, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Mites 5~10 cm')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\mites5_10_ttest.tiff',dpi=300,height=8,width=3.5)




comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "TotMites"]
  group2 <- df[df$LanduseSimplified == pair[2], "TotMites"]
  p <- wilcox.test(group1, group2)$p.value
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 

ggplot(df, aes(x = LanduseSimplified, y = TotMites, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Mites Total')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\mites_total_ttest.tiff',dpi=300,height=8,width=3.5)


comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Collem0_5"]
  group2 <- df[df$LanduseSimplified == pair[2], "Collem0_5"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Collem0_5, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Collembola 0~5 cm')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\collem_0_5_ttest.tiff',dpi=300,height=8,width=3.5)


comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Collem5_10"]
  group2 <- df[df$LanduseSimplified == pair[2], "Collem5_10"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Collem5_10, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Collembola 5~10 cm')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\collem_5_10_ttest.tiff',dpi=300,height=8,width=3.5)



comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "TotCollem"]
  group2 <- df[df$LanduseSimplified == pair[2], "TotCollem"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 



ggplot(df, aes(x = LanduseSimplified, y = TotCollem, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Collembola Total')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,25,100, 300,500,1000),limits = c(0,1800))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\collem_total_ttest.tiff',dpi=300,height=8,width=3.5)




comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)


sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Nematodes"]
  group2 <- df[df$LanduseSimplified == pair[2], "Nematodes"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Nematodes, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Nematodes')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,100,500,2000,5000),limits = c(0,11000))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\Nematodes_ttest.tiff',dpi=300,height=8,width=5)


comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Enchys"]
  group2 <- df[df$LanduseSimplified == pair[2], "Enchys"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Enchys, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Enchys')+
  ylab('Count')+
  labs(fill='Land use')+
  scale_y_continuous(trans='sqrt',breaks = c(0,10,40,100),limits = c(0,200))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\Enchys_ttest.tiff',dpi=300,height=8,width=5)


df<-read.csv('D:\\tellsoilbio\\final datta\\16S_diversity.csv')
                                                                                                                        
comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Shannon"]
  group2 <- df[df$LanduseSimplified == pair[2], "Shannon"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Shannon, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Shannon Index (16S)')+
  ylab('Count')+
  labs(fill='Land use')+
  #scale_y_continuous(trans='sqrt',breaks = c(0,10,40,100),limits = c(0,200))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\Shannon_16S_ttest.tiff',dpi=300,height=8,width=5)


df<-read.csv('D:\\tellsoilbio\\final datta\\ITS_diversity_Data_for_Map.csv')

comparisons <- combn(unique(df$LanduseSimplified), 2, simplify = FALSE)

sig_comparisons <- lapply(comparisons, function(pair) {
  group1 <- df[df$LanduseSimplified == pair[1], "Shannon"]
  group2 <- df[df$LanduseSimplified == pair[2], "Shannon"]
  p <- wilcox.test(group1, group2)$p.value
  
  if (p < 0.05) return(pair) else return(NULL)
}) %>% purrr::compact() 


ggplot(df, aes(x = LanduseSimplified, y = Shannon, fill = LanduseSimplified)) +
  geom_boxplot(width=0.8,size=0.3) +
  stat_compare_means(comparisons = sig_comparisons, 
                     method = "wilcox.test", 
                     label = "p.signif",
  ) +
  xlab('Shannon Index (ITS)')+
  ylab('Count')+
  labs(fill='Land use')+
  #scale_y_continuous(trans='sqrt',breaks = c(0,10,40,100),limits = c(0,200))+
  coord_cartesian(clip = "off") + 
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16)
  )+
  ggsave('D:\\tellsoilbio\\maps\\Shannon_ITS_ttest.tiff',dpi=300,height=8,width=5)


df<-read.csv('D:\\tellsoilbio\\final datta\\TSB.csv')
df<-na.omit(df)

rf<-randomForest(TotMites~LanduseSimplified+Ca+Cd+Cu+Fe+Na+Sand+Silt+Clay+
                  LOI+TN+BD+pH_CaCl2+CEC+Moisture,
                 ntree=300,
                 mtry=5,
                 data=df)

df_shap<-df[,c('LanduseSimplified','Ca','Cd','Cu','Fe','Na','Sand','Silt','Clay',
                 'LOI','TN','BD','pH_CaCl2','CEC','Moisture')]

shap<-fastshap::explain(rf,
                        X = df_shap,
                        nsim = 500,
                        baseline = 0,
                        adjust = TRUE,
                        pred_wrapper = function(model,newdata){
                          predict(model,newdata)
                        },
                        newdata = df_shap)

shap_df<-as.data.frame(shap)



merge<-cbind(df_shap,shap_df)

names(merge)<-c('LanduseSimplified','Ca','Cd','Cu','Fe','Na','Sand','Silt','Clay',
                'LOI','TN','BD','pH_CaCl2','CEC','Moisture','LanduseSimplified_s',
                'Ca_s','Cd_s','Cu_s','Fe_s','Na_s','Sand_s','Silt_s','Clay_s',
                'LOI_s','TN_s','BD_s','pH_CaCl2_s','CEC_s','Moisture_s')


plot(merge$Cd,merge$Cd_s)
plot(merge$Ca,merge$Ca_s)

ggplot(merge,aes(x=LanduseSimplified,y=LanduseSimplified_s))+
  geom_boxplot()
