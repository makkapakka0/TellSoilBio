library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrplot)
library(randomForest)
library(fastshap)
library(tidyverse)
library(ggbeeswarm)

df<-read.csv(paste0('D:\\tellsoilbio\\all\\dataall.csv'))

summary(df)

sd<-apply(df, 2, sd, na.rm=FALSE)

options(scipen = 999)
sd

box<-df[,c(2:54)]

setwd('D:\\tellsoilbio\\maps\\boxplot')

for (i in c(1:53)) {
  p<-ggplot(box,aes(x='',y=box[,i]))+
      stat_boxplot(geom='errorbar',width=0.1)+
      geom_boxplot(fill='lightblue')+
      labs(x=colnames(box)[i])+
      theme(axis.text = element_text(size=16),
            axis.title = element_text(size=24),
            axis.title.y = element_blank(),
            panel.border = element_rect(colour = 'black',fill=NA,size=1),
            panel.grid.major = element_line(colour = 'gray80'),
            panel.grid.minor = element_line(colour = 'gray80'),
            axis.line = element_line(colour = 'black'))
    ggsave(filename = paste0('boxplot_',colnames(box)[i],'.png'),plot = p,dpi=300,width=2,height=6)
}

setwd('D:\\tellsoilbio\\maps\\barplot')

for (i in c(1:53)) {
  p<-ggplot(box,aes(x=box[,(i)]))+
    geom_histogram(aes(y=..count..),fill='steelblue',bins=30,alpha=0.7)+
    labs(x=colnames(box)[i])+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=24),
          axis.title.y = element_blank(),
          panel.border = element_rect(colour = 'black',fill=NA,size=1),
          panel.grid.major = element_line(colour = 'gray80'),
          panel.grid.minor = element_line(colour = 'gray80'),
          axis.line = element_line(colour = 'black'))
  ggsave(filename = paste0('histogram_',colnames(box)[i],'.png'),plot = p,dpi=300,width=6,height=4)
}

df<-read.csv('D:\\tellsoilbio\\bioresults1\\data1.csv')
names(df)<-c('site','Al','As','Ba','Ca','Cd','Co','Cr','Cu','Fe','Hg','K','Mn',
             'Mo','Na','Ni','P','Pb','S','Se','Sr','Th','U','Zn','lc','lat','long',
             'Acari','Collembola','Total')

df1<-df[,-c(1,25,26,27)]
cor_matrix1<-cor(df1,method = 'spearman')

result<-rcorr(as.matrix(df1),type='spearman')
cor_matrix<-result$r

p_matrix<-result$P

col_pal<-colorRampPalette(c('blue','white','#F05B3C'))(200)

png('D:\\tellsoilbio\\bioresults1\\spearman.png',width=800,height = 800)
corrplot(cor_matrix,
         method = 'color',
         col=col_pal,
         type = 'upper',
         tl.cex = 1,
         tl.col = 'black',
         number.cex = 0.8,
         addCoef.col = 'black',insig = 'blank',
         p.mat = p_matrix,sig.level = 0.05
         )
dev.off()


rf<-randomForest(Acari~Al+As+Ba+Cd+Co+Cr+Cu+Fe+K+Mn+Na+Ni+P+S+Th+U+Zn,
                 ntree=500,
                 mtry=4,
                 data=df1)
shap_Acari<-df1[,c('Al','As','Ba','Cd','Co','Cr','Cu','Fe','K','Mn',
                   'Na','Ni','P','S','Th','U','Zn')]
shap1<-fastshap::explain(rf,
                         X=shap_Acari,
                         nsim=500,
                         baseline=0,
                         adjust = TRUE,
                         pred_wrapper = function(model,newdata){
                           predict(model,newdata)
                         },
                         newdata = shap_Acari)
df_shap_Acari<-as.data.frame(shap1)
names(df_shap_Acari)<-c('Als','Ass','Bas','Cds','Cos','Crs','Cus','Fes','Ks','Mns',
                        'Nas','Nis','Ps','Ss','Ths','Us','Zns')
df_shap_Acari$ID<-seq_len(nrow(df_shap_Acari))
data1<-gather(df_shap_Acari,key='name',value = 'shap',-ID)


df_Acari_scaled<-as.data.frame(scale(shap_Acari))

df_Acari_scaled$ID<-seq_len(nrow(df_Acari_scaled))
data2<-gather(df_Acari_scaled,key='name',value = 'value',-ID)
data1<-cbind(data1,data2)
names(data1)<-c('ID','names','shap','ID2','name','value')

ggplot(data1,aes(x=shap,y=name,color=value))+
  geom_quasirandom(method='pseudorandom',alpha=0.2,size=0.8)+
  labs(x='Acari (counts)',y='Variable',color='Value')+
  scale_color_gradient2(
    low = 'blue',
    mid='black',
    high='red',
    midpoint=2
  )+
  xlim(-20,75)+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=24),
        panel.border = element_rect(colour = 'black',fill=NA),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))+
  ggsave('D:\\tellsoilbio\\bioresults1\\shapsum.tiff',dpi=300,width = 10, height=6)

merge<-cbind(shap_Acari,df_shap_Acari)

ggplot(merge,aes(x=Al,y=Als))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,3.5)+
  xlab('Al (%)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Alshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=As,y=Ass))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,62)+
  xlab('As (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Asshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Ba,y=Bas))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,580)+
  xlab('Ba (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Bashap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Cd,y=Cds))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,5.5)+
  xlab('Cd (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Cdshap.tiff',dpi=300,width=10,height=6)


ggplot(merge,aes(x=Co,y=Cos))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,27)+
  xlab('Co (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Coshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Cr,y=Crs))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,50)+
  xlab('Cr (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Crshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Cu,y=Cus))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,120)+
  xlab('Cu (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Cushap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Fe,y=Fes))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,12)+
  xlab('Fe (%)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Feshap.tiff',dpi=300,width=10,height=6)


ggplot(merge,aes(x=K,y=Ks))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,0.24)+
  xlab('K (%)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Kshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Mn,y=Mns))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,3000)+
  xlab('Mn (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Mnshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Na,y=Nas))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,0.1)+
  xlab('Na (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Nashap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Ni,y=Nis))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,100)+
  xlab('Ni (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Nishap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=P,y=Ps))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,0.2)+
  xlab('P (%)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Pshap.tiff',dpi=300,width=10,height=6)


ggplot(merge,aes(x=S,y=Ss))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,0.42)+
  ylim(-1,25)+
  xlab('S (%)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Sshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Th,y=Ths))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,3)+
  xlab('Th (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Thshap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=U,y=Us))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,7)+
  xlab('U (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Ushap.tiff',dpi=300,width=10,height=6)

ggplot(merge,aes(x=Zn,y=Zns))+
  geom_point(size=0.8,alpha=0.5)+
  geom_smooth(method='loess',span=0.5,se=TRUE,color='red',linetype='solid',size=1.5)+
  xlim(0,200)+
  xlab('Zn (mg/kg)')+
  ylab('SHAP value (counts)')+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28),
        panel.border = element_rect(colour = 'black',fill=NA,size=2),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = 'gray80'),
        panel.grid.minor = element_line(colour = 'gray80'),
        axis.line = element_line(colour = 'black'))+
  ggsave('D:\\tellsoilbio\\bioresults1\\Znshap.tiff',dpi=300,width=10,height=6)

df<-read.csv(paste0('D:\\tellsoilbio\\tellsoilbio.csv'))



