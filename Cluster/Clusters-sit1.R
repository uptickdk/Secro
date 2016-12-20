# Optimal number of clusters

# methods:
# 1. Minimum number of clusters that explain at least 90% of variance
# 2. Minimum number of clusters such that correlation among all components in each cluster is at least 40%
# 3. Elbow method

#LOAD in SIT, quantmod, 
con = gzcon(file('sit.gz', 'rb')) #from same folder
source(con)
close(con)


#*****************************************************************
# Load historical data for ETFs
#****************************************************************** 
load.packages('quantmod')


####################### Model and thier symbols  ################
# Alt Full portfolio
#tickers <- spl('GLD,UUP,USO,PCY,TLT,VIIZ,VYM,TBF,SPY,GNR,GCC,
 #              DLN,DVY,KBE,DHS,SDIV,FXE,DIV,WOOD,EPP')
# Macro  
#tickers<- spl('SPY,SLY,EEM,EFA,GLD,QQQ,ZIV,TLO,VNQ,ILF,IEV,EPP')  #MACRO PORT
#Alts
#tickers<- spl('SPY,UUP,TBF,TLT,PCY,KBE,GNR,WOOD,EPP')
# Broad Sec ETF
#tickers<- spl('FXD,FXH,FXR,FXL,FXG,FXO,FXN,FXU,FXZ,ZIV')
# Spec Sit- DK
# spl('SCHC,QTEC,SPHQ,RYT,PEY,GSY,FPX,FBT,PPA,CQQQ,FRN,LQD,TLT')
# Jones 6491 tickers<- spl('AGG,EFV,EWJ,OAKMX,PRDSX,SPY,JMVSX,VSMIX,DHLSX,TMDIX,MLAIX,GJRTX,MINT,THDIX,AQMIX,CCCNX,HYG,TIP')
# Jones all taxable tickers<- spl('AGG,EFV,EWJ,OAKMX,PRDSX,SPY,JMVSX,VSMIX,DHLSX,TMDIX,MLAIX,GJRTX,MINT,THDIX,AQMIX,CCCNX,HYG,TIP,VUG,VBR,VBK,VTV,BSV,KNPCX,PYSCX,UBVCX,ONCE,LPTCX,HFLAX,HLACX,NEFZX,NTHEX,PYSCX,VAPCX')
# Jones after sales tickers<- spl('AGG,EFV,PRDSX,SPY,JMVSX,DHLSX,TMDIX,MINT,THDIX,AQMIX,CCCNX,HYG,TIP,VUG,VBR,VBK,VTV,BSV,PYSCX,UBVCX,ONCE,LPTCX,HFLAX,NTHEX, EFV,GJRTX')
##############################################################

tickers<- spl('AGG,EFV,EWJ,OAKMX,PRDSX,SPY,JMVSX,VSMIX,DHLSX,TMDIX,MLAIX,GJRTX,MINT,THDIX,AQMIX,CCCNX,HYG,TIP,VUG,VBR,VBK,VTV,BSV,KNPCX,PYSCX,UBVCX,ONCE,LPTCX,HFLAX,HLACX,NEFZX,NTHEX,PYSCX,VAPCX')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] <- adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na')


#*****************************************************************
# Create Clusters
#****************************************************************** 
# compute returns
ret <- data$prices / mlag(data$prices) - 1
ret <- na.omit(ret)    

# setup period and method to compute correlations
dates <- '2014::2016'
method <- 'pearson'  # kendall, spearman

correlation <- cor(ret[dates], method = method)    
dissimilarity <- 1 - (correlation)
distance <- as.dist(dissimilarity)

# get first 2 pricipal componenets
xy <- cmdscale(distance)

#  iterate from 2 clusters to 2/3N clusters (where N is the number of securities) 
# and in each case compute the percentage of variance explained by clusters and minimum correlation among all components in each cluster.

#*****************************************************************
# Determine number of clusters
#****************************************************************** 
n <- ncol(data$prices)
n1 <- ceiling(n*2/3) # maximun number of possible clusters

# percentage of variance explained by clusters
p.exp <- rep(0,n1)

# minimum correlation among all components in each cluster  
min.cor <- matrix(1,n1,n1)  

for (i in 2:n1) {
      fit <- kmeans(xy, centers=i, iter.max=100, nstart=100)
      p.exp[i] <- 1- fit$tot.withinss / fit$totss # variance explained
      
      for (j in 1:i) {
            index <- fit$cluster == j
            min.cor[i,j] <- min(correlation[index,index])
      }
}

# minimum number of clusters that explain at least 90% of variance
min(which(p.exp > 0.9))

# minimum number of clusters such that correlation among all components in each cluster is at least 40%
# will not always work
min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1

# number of clusters based on elbow method
#  look at the marginal gain of adding each additional cluster. 
# And set the number of clusters equal to the largest K that has positive marginal gain
find.maximum.distance.point(p.exp[-1]) + 1


# visually see the difference between 4 and 5 clusters
load.packages('cluster')
fit <- kmeans(xy, 4, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Major Market Clusters over', dates, ', 4 Clusters'), sub='')

fit <- kmeans(xy, 5, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Clusters over', dates, ', 5 Clusters'), sub='')

fit <- kmeans(xy, 6, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Clusters over', dates, ', 6 Clusters'), sub='')
