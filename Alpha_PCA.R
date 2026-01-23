library(vegan)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggfortify)
library(sf)
library(randomForest)
library(fastshap)
library(mgcv)
library(gratia)

data <- read.csv("D:\\tellsoilbio\\PCA\\Counts_withElements_Shannon.csv")

df<-data[,6:75]
df1<-df[,c(60:67)]
df<-df[,c('Cu','Zn','Pb','Cd','Se','As','Ni','Fe','Al','Cr','Hg','Mn',
          'Sr','Mo','Co','Ba','U','Th','Ca','pH_CaCl2')]

env<-cbind(df1,df)
sp<-data[,6:11]

all<-cbind(sp,env)

all_pca<-prcomp(all,center = TRUE,scale. = TRUE)
summary(all_pca)


loading_scores <- all_pca$rotation
loading_scores

loadings<-as.data.frame(all_pca$rotation[,1:2])
loadings$variable<-rownames(loadings)

ggplot(data=loadings,aes(x=0,y=0,xend=PC1,yend=PC2,label=variable))+
  geom_segment(arrow = arrow(length=unit(0.25,'cm')),color='#d7483a')+
  geom_text_repel(data=loadings,aes(x=PC1,y=PC2),
                  color='#326d88',max.overlaps = 324)+
  xlab('PC1(23.76%)')+
  ylab('PC2(16.92%)')+
  xlim(-0.4,0.4)+
  ylim(-0.4,0.4)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\maps\\PCA\\pca.tiff',dpi=300,width=6.5,height=6)

data$PC1 <- all_pca$x[,1]
data$pc2 <- all_pca$x[,2]

coords<-data[,84:85]

pca_sf<-st_as_sf(data,coords=c('Centre_Long','Centre_Lat'),crs=4326)
st_write(pca_sf,'D:\\tellsoilbio\\PCA\\all_pca.shp',append=FALSE)

biplot(all_pca)
autoplot(all_pca,loadings=TRUE,loadings.label=TRUE)

species_hel<-decostand(sp,method='hellinger')
rda_result<-rda(species_hel~ .,data=env)
summary(rda_result)

anova(rda_result,permutations = 999)

anova(rda_result,by='term',permutation=999)

species_scores<-scores(rda_result,display='species',choices=1:2)
species_df<-as.data.frame(species_scores)
species_df$species<-rownames(species_df)

env_scores<-scores(rda_result,display='bp',choices=1:2)
env_df<-as.data.frame(env_scores)
env_df$env<-rownames(env_df)

ggplot()+
  geom_point(data=species_df,aes(x=RDA1,y=RDA2),color='#326d88',alpha=0.6,size=4)+
  ggrepel::geom_text_repel(data=species_df,aes(x=RDA1,y=RDA2,label=species),
                                                 color='#0f2b3a',size=3)+
  geom_segment(data=env_df,aes(x=0,y=0,xend=RDA1,yend=RDA2),
               arrow=arrow(length=unit(0.3,'cm')),color='#d7483a')+
  ggrepel::geom_text_repel(data=env_df,aes(x=RDA1,y=RDA2,label=env),
                           color='#0f2b3a',size=3)+
  xlim(-0.7,0.7)+
  ylim(-0.7,0.7)+
  theme_minimal()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\maps\\PCA\\rda.tiff',dpi=300,width=6.5,height=6)

plot(rda_result,scaling=2)



#RF
all$lc<-data$LanduseSimplified
rf<-randomForest(Collembola~lc+Sand+Silt+Clay+TC+TOC+TN+LOI+BD+Cu+Zn+Pb+Cd+Se+As+Ni+Fe+Al+Cr
                   +Hg+Mn+Sr+Mo+Co+Ba+U+Th+Ca+pH_CaCl2,
                 ntree=500,
                 mtry=15,
                 data=all)
rf[["rsq"]]
imp<-importance(rf)
imp_df<-data.frame(feature=rownames(imp),importance=imp[,1])
imp_df<-imp_df[order(imp_df$importance,decreasing = TRUE),]
top_15<-head(imp_df,15)
top<-rownames(top_15)

rf_data_collem<-all[,colnames(all)%in%top]
rf_data_collem<-cbind(all$Collembola,mod_data_collem)
colnames(mod_data_collem)[colnames(mod_data_collem)=='all$Collembola']<-'Collembola'

rf<-randomForest(Collembola~.,
                 ntree=500,
                 mtry=15,
                 data=rf_data_collem)
rf[["rsq"]]


#GAM
all$Silt[all$Silt<0]<-1
all[,7:33]<-log(all[,7:33])

#CV
folds<-sample(rep(1:10,length.out=188))
cv_error<-numeric(10)

for (i in 1:10) {
  train_data<-all[folds!=i,]
  test_data<-all[folds==i,]
  
  gam<-gam(Collembola~factor(lc)+s(Sand)+s(Silt)+s(Clay)+s(TC)+s(TOC)+s(TN)+s(LOI)+s(BD)
           +s(Cu)+s(Zn)+s(Pb)+s(Cd)+s(Se)+s(As)+s(Ni)+s(Fe)+s(Al)+s(Cr)
           +s(Hg)+s(Mn)+s(Sr)+s(Mo)+s(Co)+s(Ba)+s(U)+s(Th)+s(Ca)+s(pH_CaCl2),
           data=train_data,
           select = FALSE,
           method = 'REML')
  preds<-predict(gam,newdata=test_data)
  
  cv_error[i]<-mean((test_data$Collembola-preds)^2)
}

error<-mean(cv_error)


gam_collem<-gam(Collembola~factor(lc)+s(Sand)+s(Silt)+s(Clay)+s(TC)+s(TOC)+s(TN)+s(LOI)+s(BD)
                +s(Cu)+s(Zn)+s(Pb)+s(Cd)+s(Se)+s(As)+s(Ni)+s(Fe)+s(Al)+s(Cr)
                +s(Hg)+s(Mn)+s(Sr)+s(Mo)+s(Co)+s(Ba)+s(U)+s(Th)+s(Ca)+s(pH_CaCl2),
                data=all,
                select = FALSE,
                method = 'REML'
)
summary(gam_collem)
pred<-predict(gam_collem,newdata=all)
mse<-mean((all$Collembola-pred)^2)


gam_collem<-gam(Collembola~factor(lc)+s(Sand)+s(Silt)+s(Clay)+s(TC)+s(TOC)+s(TN)+s(LOI)+s(BD)
                +s(Cu)+s(Zn)+s(Pb)+s(Cd)+s(Se)+s(As)+s(Ni)+s(Fe)+s(Al)+s(Cr)
                +s(Hg)+s(Mn)+s(Sr)+s(Mo)+s(Co)+s(Ba)+s(U)+s(Th)+s(Ca)+s(pH_CaCl2),
                data=all,
                select = FALSE,
                method = 'REML'
                )
summary(gam_collem)

draw(gam_collem,select = 's(pH_CaCl2)')
p_smooth<-smooth_estimates(gam_collem,smooth='s(Cd)')
p_smooth

ggplot()+
  geom_point(data=all,aes(x=Cd,y=Collembola),alpha=0.5)+
  geom_line(data=p_smooth,aes(x=Cd,y=.estimate),color='blue',size=1)

gam_mites<-gam(Mites~factor(lc)+s(Sand)+s(Silt)+s(Clay)+s(TC)+s(TOC)+s(TN)+s(LOI)+s(BD)
                +s(Cu)+s(Zn)+s(Pb)+s(Cd)+s(Se)+s(As)+s(Ni)+s(Fe)+s(Al)+s(Cr)
                +s(Hg)+s(Mn)+s(Sr)+s(Mo)+s(Co)+s(Ba)+s(U)+s(Th)+s(Ca)+s(pH_CaCl2),
                data=all,
                select = FALSE,
                method = 'REML')
summary(gam_mites)

draw(gam_mites,select = 's(Sr)')
p_smooth<-smooth_estimates(gam_mites,smooth='s(LOI)')
p_smooth

ggplot()+
  geom_point(data=all,aes(x=LOI,y=Mites),alpha=0.5)+
  geom_line(data=p_smooth,aes(x=LOI,y=.estimate),color='blue',size=1)
