library(spdep)
library(gstat)
library(sp)

df<-read.csv('D:\\tellsoilbio\\fulldata\\fulldata.csv')
data <- read.csv("D:\\tellsoilbio\\PCA\\Counts_withElements_Shannon.csv")
Div<-data[,c(2,10,11)]

df<-merge(df,Div,by='ID',all.x=TRUE)

na_col<-names(df)[colSums(is.na(df))>0]
na_col<-na_col[-1]


df_to_fill<-df[,names(df) %in% na_col]
coords<-df[,c('ITM_x','ITM_y')]
df_to_fill<-cbind(df_to_fill,coords)


coords <- df_to_fill[,c("ITM_x","ITM_y")]

knn <- knearneigh(coords, k=4)

nb <- knn2nb(knn)

lw <- nb2listw(nb, style="W")

fill_column <- function(df, colname, lw){
  
  values <- df[[colname]]
  
  # 只用非NA计算 Moran
  non_na_index <- which(!is.na(values))
  
  values_non_na <- values[non_na_index]
  
  lw_subset <- subset(lw, subset = !is.na(values))
  
  moran <- moran.test(values_non_na, lw_subset)
  
  print(paste(colname, "Moran I p-value:", moran$p.value))
  
  # 如果显著空间自相关
  if(moran$p.value < 0.05){
    
    print(paste(colname, "→ Kriging"))
    
    coordinates(df) <- ~ITM_x+ITM_y
    
    formula = as.formula(paste(colname,"~1"))
    
    vg<-variogram(formula,df[!is.na(values),])
    
    fit<-fit.variogram(vg,model = vgm('Sph'))
    
    krig<-krige(formula,df[!is.na(values),],df[is.na(values),],model=fit)
    
    
    df[[colname]][is.na(values)] <- krig$var1.pred
    
    df <- as.data.frame(df)
    
  }else{
    
    print(paste(colname, "→ Median"))
    
    median_value <- median(values, na.rm=TRUE)
    
    df[[colname]][is.na(values)] <- median_value
    
  }
  
  return(df)
  
}

cols <- names(df_to_fill)[!names(df_to_fill) %in% c("ITM_x","ITM_y")]

for(col in cols){
  
  df_to_fill <- fill_column(df_to_fill, col, lw)
  
}


df[,names(df_to_fill)]<-df_to_fill

write.csv(df,'D:\\tellsoilbio\\fulldata\\fulldata_interpolated.csv')


