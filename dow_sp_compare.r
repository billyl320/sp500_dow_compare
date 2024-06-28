# column median function

colMeds<-function(mat){

  num_cols<-dim(mat)[2]
  ultima<-matrix(nrow=1,
                 ncol=num_cols,
                 data=0
                 )

  colnames(ultima)<-colnames(mat)

  for(i in 1:num_cols){

    ultima[1,i]<-median(mat[,i])

  }

  return(ultima)

}

# column variance function

colVars<-function(mat){

  num_cols<-dim(mat)[2]
  ultima<-matrix(nrow=1,
                 ncol=num_cols,
                 data=0
                 )

  colnames(ultima)<-colnames(mat)

  for(i in 1:num_cols){

    ultima[1,i]<-var(mat[,i])

  }

  return(ultima)

}

# moving averages comparison

# source: https://stackoverflow.com/questions/743812/calculating-moving-average
ma <- function(x, n = 5){
      temp = filter(x, rep(1 / n, n), sides = 2)
      ultima = as.numeric(na.omit(temp))
      return(ultima)
  }

# obtains the proportion of values less than a value for each column
colProp<-function(mat, threshold){

  num_cols<-dim(mat)[2]
  ultima<-matrix(nrow=1,
                 ncol=num_cols,
                 data=0
                 )

  colnames(ultima)<-colnames(mat)

  for(i in 1:num_cols){

    ultima[1,i]<-sum((mat[,i])<threshold)/dim(mat)[1]

  }

  return(ultima)

}

# reading the data in

# from https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average
dow<-read.table('data/dow.csv', sep=',', header=TRUE)

# from https://www.macrotrends.net/2526/sp-500-historical-annual-returns
sp<-read.table('data/sp500.csv', sep=',', header=TRUE)
sp<-sp[dim(sp)[1]:1,]

# subset of dow
dow_subset<-dow[33:128,]

# reformatted percent changes for dow
dow_change<-gsub(" ", "", dow_subset[,4], fixed = TRUE)
dow_change<-gsub("−", "-", dow_change, fixed = TRUE)

dow_change<-1+as.numeric(dow_change)/100

# reformatted percent changes for sp
sp_change<-gsub(" ", "", sp[,7], fixed = TRUE)
sp_change<-gsub("%", "", sp_change, fixed = TRUE)
sp_change<-gsub("−", "-", sp_change, fixed = TRUE)

sp_change<-1+as.numeric(sp_change)/100

# summary statistics
summary_stats<-matrix(nrow=length(summary(sp_change)),
                      ncol=2,
                      data=0)
index_names<-c("DOW", "S&P 500")
index_colors<-c(
              rgb(0, 0, 1, 1/3),
              rgb(1, 0, 0, 1/3)
               )

colnames(summary_stats)<-index_names
rownames(summary_stats)<-names(summary(sp_change))

summary_stats[,1]<-summary(dow_change)
summary_stats[,2]<-summary(sp_change)

summary_stats

# return on each over same peroid
prod(dow_change)
prod(sp_change)

#unifying histograms

hist_breaks<-seq(min(dow_change, sp_change),
                 max(dow_change, sp_change),
                 length.out = 15)

# histograms of changes break=10

h1<-hist(dow_change,
         breaks=10
        )

h2<-hist(sp_change,
         breaks=10
        )

png(file='plots/historical/hist_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h1,
     col=index_colors[1],
     xlim = c(0.4, 1.75),
     xlab="Proportion Change",
     main="Histograms of Proportion Change\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h2,
     col=index_colors[2],
     xlim = c(0.4, 1.75),
     add=TRUE
    )
legend(x=0.4,
       y=20,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()


# histograms of changes breaks unified

h1<-hist(dow_change,
         breaks=hist_breaks
        )

h2<-hist(sp_change,
         breaks=hist_breaks
        )

length(h1$counts)
length(h2$counts)

png(file='plots/historical/hist_breaks_unified_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h2,
     col=index_colors[2],
     xlim = c(0.4, 1.75),
     xlab="Proportion Change; Breaks=Unified",
     main="Histograms of Proportion Change\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h1,
     col=index_colors[1],
     xlim = c(0.4, 1.75),
     add=TRUE
    )
legend(x=0.4,
       y=17,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()

# 10 year moving average

sp_10_change<-ma(sp_change, n=10)
dow_10_change<-ma(dow_change, n=10)

png(file='plots/historical/ten_year_moving_ave_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(dow_10_change,
     col=index_colors[1],
     ylab="10 Year Moving Average",
     xlab ='Time',
     main="10 Year Moving Average\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(sp_10_change,
     col=index_colors[2],
     lwd=10
    )
legend(x=1,
       y=1.15,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


# 20 year moving average

sp_20_change<-ma(sp_change, n=20)
dow_20_change<-ma(dow_change, n=20)


png(file='plots/historical/twenty_year_moving_ave_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(dow_20_change,
     col=index_colors[1],
     ylab="20 Year Moving Average",
     xlab ='Time',
     main="20 Year Moving Average\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(sp_20_change,
     col=index_colors[2],
     lwd=10
    )
legend(x=1,
       y=1.15,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


# 30 year moving average

sp_30_change<-ma(sp_change, n=30)
dow_30_change<-ma(dow_change, n=30)


png(file='plots/historical/thirty_year_moving_ave_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(dow_30_change,
     col=index_colors[1],
     ylab="30 Year Moving Average",
     xlab ='Time',
     main="30 Year Moving Average\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(sp_30_change,
     col=index_colors[2],
     lwd=10
    )
legend(x=1,
       y=1.11,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


hist_breaks<-seq(min(dow_30_change, sp_30_change),
                 max(dow_30_change, sp_30_change),
                 length.out = 15)

# histograms of changes break=10

h1<-hist(dow_30_change,
         breaks=10
        )

h2<-hist(sp_30_change,
         breaks=10
        )

png(file='plots/historical/hist_compare_30_ma.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h1,
     col=index_colors[1],
     xlim = c(0.4, 1.75),
     xlab="Proportion Change",
     main="Histograms of 30 Year MA\nProportion Change from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h2,
     col=index_colors[2],
     xlim = c(0.4, 1.75),
     add=TRUE
    )
legend(x=0.4,
       y=20,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()



# 40 year moving average

sp_40_change<-ma(sp_change, n=40)
dow_40_change<-ma(dow_change, n=40)


png(file='plots/historical/fourty_year_moving_ave_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(dow_40_change,
     col=index_colors[1],
     ylab="40 Year Moving Average",
     xlab ='Time',
     main="40 Year Moving Average\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(sp_40_change,
     col=index_colors[2],
     lwd=10
    )
legend(x=1,
       y=1.11,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()



# 50 year moving average

sp_50_change<-ma(sp_change, n=50)
dow_50_change<-ma(dow_change, n=50)


png(file='plots/historical/fifty_year_moving_ave_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(sp_50_change,
     col=index_colors[2],
     ylab="50 Year Moving Average",
     xlab ='Time',
     main="50 Year Moving Average\nfrom 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(dow_50_change,
     col=index_colors[1],
     lwd=10
    )
legend(x=1,
       y=1.10,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()

################################################
# Bootstrap estimation of returns for 30 Years
################################################

set.seed(2547)

runs<-1000000
num_years = 30

dow_boot<-list()
sp_boot<-list()

dow_boot_summary<-matrix(nrow=length(summary(sp_change)),
                          ncol=runs,
                          data=0)
sp_boot_summary<-matrix(nrow=length(summary(sp_change)),
                         ncol=runs,
                         data=0)

rownames(sp_boot_summary)<-names(summary(sp_change))
rownames(dow_boot_summary)<-names(summary(sp_change))


for(i in 1:runs){

  #sampling 30 years
  vals<-sample(1:length(sp_change), num_years, replace=TRUE)

  # getting values for those sampled years
  sp_boot[[i]]<-sp_change[vals]
  dow_boot[[i]]<-dow_change[vals]

  # getting summary statistics for those sampled years
  sp_boot_summary[,i]<-summary(sp_change[vals])
  dow_boot_summary[,i]<-summary(dow_change[vals])

}


# histograms of averages

h1<-hist(dow_boot_summary[4,],
         breaks=19
        )

h2<-hist(sp_boot_summary[4,],
         breaks=19
        )

png(file='plots/thirty/hist_mean_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.20),
     xlab="Mean Proportion Change",
     main="Histograms of Mean Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.20),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()


# histograms of medians

h1<-hist(dow_boot_summary[3,],
         breaks=21
        )

h2<-hist(sp_boot_summary[3,],
         breaks=21
        )

png(file='plots/thirty/hist_med_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.25),
     xlab="Median Proportion Change",
     main="Histograms of Median Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.25),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()

# histograms of medians with unified breaks

hist_breaks<-seq(min(dow_boot_summary[3,], sp_boot_summary[3,]),
                 max(dow_boot_summary[3,], sp_boot_summary[3,]),
                 length.out = 20)

h1<-hist(dow_boot_summary[3,],
         breaks=hist_breaks
        )

h2<-hist(sp_boot_summary[3,],
         breaks=hist_breaks
        )

png(file='plots/thirty/hist_med_unified_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.25),
     xlab="Median Proportion Change; Breaks Unified",
     main="Histograms of Median Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.25),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()


colMeans(t(dow_boot_summary))
colMeans(t(sp_boot_summary))

colMeds(t(dow_boot_summary))
colMeds(t(sp_boot_summary))

colVars(t(dow_boot_summary))
colVars(t(sp_boot_summary))


rate_thresh<-seq(from=0.85, to=1.50, by=0.01)

sp_prop_mat<-matrix(nrow=6,
                 ncol=length(rate_thresh),
                 data=0
                )

rownames(sp_prop_mat)<-rownames(dow_boot_summary)
colnames(sp_prop_mat)<-rate_thresh

dow_prop_mat<-sp_prop_mat+0.0

for(i in 1:length(rate_thresh)){

  dow_prop_mat[,i]<-colProp(t(dow_boot_summary), rate_thresh[i])
  sp_prop_mat[,i]<-colProp(t(sp_boot_summary), rate_thresh[i])

}


png(file='plots/thirty/median_threshes_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(x=rate_thresh,
     y=dow_prop_mat[3,],
     col=index_colors[1],
     ylab="Proportion Less than Return",
     xlab ='Return',
     main="Simulated Medians Comparing\nDOW and S&P 500 from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(x=rate_thresh,
      y=sp_prop_mat[3,],
     col=index_colors[2],
     lwd=10
    )
legend(x=0.85,
       y=1.0,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


png(file='plots/thirty/mean_threshes_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(x=rate_thresh,
     y=dow_prop_mat[4,],
     col=index_colors[1],
     ylab="Proportion Less than Return",
     xlab ='Return',
     main="Simulated Means Comparing\nDOW and S&P 500 from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(x=rate_thresh,
      y=sp_prop_mat[4,],
     col=index_colors[2],
     lwd=10
    )
legend(x=0.85,
       y=1.0,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()

png(file='plots/thirty/median_threshes_compare_zoomedin.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(x=rate_thresh,
     y=dow_prop_mat[3,],
     xlim=c(1.05, 1.15),
     col=index_colors[1],
     ylab="Proportion Less than Return",
     xlab ='Return',
     main="Simulated Medians Comparing\nDOW and S&P 500 from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(x=rate_thresh,
      y=sp_prop_mat[3,],
      xlim=c(1.05, 1.15),
     col=index_colors[2],
     lwd=10
     )

legend(x=1.05,
       y=1.0,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


#################################################
# Bootstrap estimation of returns for 100 years
#################################################

set.seed(4920730)

runs<-1000000
num_years = 100

dow_boot<-list()
sp_boot<-list()

dow_boot_summary<-matrix(nrow=length(summary(sp_change)),
                          ncol=runs,
                          data=0)
sp_boot_summary<-matrix(nrow=length(summary(sp_change)),
                         ncol=runs,
                         data=0)

rownames(sp_boot_summary)<-names(summary(sp_change))
rownames(dow_boot_summary)<-names(summary(sp_change))


for(i in 1:runs){

  #sampling 100 years
  vals<-sample(1:length(sp_change), num_years, replace=TRUE)

  # getting values for those sampled years
  sp_boot[[i]]<-sp_change[vals]
  dow_boot[[i]]<-dow_change[vals]

  # getting summary statistics for those sampled years
  sp_boot_summary[,i]<-summary(sp_change[vals])
  dow_boot_summary[,i]<-summary(dow_change[vals])

}


# histograms of averages

h1<-hist(dow_boot_summary[4,],
         breaks=19
        )

h2<-hist(sp_boot_summary[4,],
         breaks=19
        )

png(file='plots/onehundred/hist_mean_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.20),
     xlab="Mean Proportion Change",
     main="Histograms of Mean Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.20),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()


# histograms of medians

h1<-hist(dow_boot_summary[3,],
         breaks=21
        )

h2<-hist(sp_boot_summary[3,],
         breaks=21
        )

png(file='plots/onehundred/hist_med_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.25),
     xlab="Median Proportion Change",
     main="Histograms of Median Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.25),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()

# histograms of medians with unified breaks

hist_breaks<-seq(min(dow_boot_summary[3,], sp_boot_summary[3,]),
                 max(dow_boot_summary[3,], sp_boot_summary[3,]),
                 length.out = 20)

h1<-hist(dow_boot_summary[3,],
         breaks=hist_breaks
        )

h2<-hist(sp_boot_summary[3,],
         breaks=hist_breaks
        )

png(file='plots/onehundred/hist_med_unified_boot_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(h2,
     col=index_colors[2],
     xlim = c(0.9, 1.25),
     xlab="Median Proportion Change; Breaks Unified",
     main="Histograms of Median Proportion Change\nfrom Bootstrap Simulation",
     cex.main=4,
     cex.lab=3,
     cex.axis=2
    )

plot(h1,
     col=index_colors[1],
     xlim = c(0.9, 1.25),
     add=TRUE
    )
legend(x=0.9,
       y=175,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=3
      )
dev.off()


colMeans(t(dow_boot_summary))
colMeans(t(sp_boot_summary))

colMeds(t(dow_boot_summary))
colMeds(t(sp_boot_summary))

colVars(t(dow_boot_summary))
colVars(t(sp_boot_summary))

rate_thresh<-seq(from=0.85, to=1.50, by=0.01)

sp_prop_mat<-matrix(nrow=6,
                 ncol=length(rate_thresh),
                 data=0
                )

rownames(sp_prop_mat)<-rownames(dow_boot_summary)
colnames(sp_prop_mat)<-rate_thresh

dow_prop_mat<-sp_prop_mat+0.0

for(i in 1:length(rate_thresh)){

  dow_prop_mat[,i]<-colProp(t(dow_boot_summary), rate_thresh[i])
  sp_prop_mat[,i]<-colProp(t(sp_boot_summary), rate_thresh[i])

}


png(file='plots/onehundred/median_threshes_compare.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(x=rate_thresh,
     y=dow_prop_mat[3,],
     col=index_colors[1],
     ylab="Proportion Less than Return",
     xlab ='Return',
     main="Simulated Medians Comparing\nDOW and S&P 500 from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(x=rate_thresh,
      y=sp_prop_mat[3,],
     col=index_colors[2],
     lwd=10
    )
legend(x=0.85,
       y=1.0,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()



png(file='plots/onehundred/median_threshes_compare_zoomedin.png',
    width=1000,
    height=1000,
    )
par(mar=c(7, 7, 7, 3),
    mgp=c(4, 1, 0)
   )
plot(x=rate_thresh,
     y=dow_prop_mat[3,],
     xlim=c(1.05, 1.15),
     col=index_colors[1],
     ylab="Proportion Less than Return",
     xlab ='Return',
     main="Simulated Medians Comparing\nDOW and S&P 500 from 1928-2023",
     cex.main=4,
     cex.lab=3,
     cex.axis=2,
     cex = 5,
     type='l',
     lwd=10
    )

lines(x=rate_thresh,
      y=sp_prop_mat[3,],
      xlim=c(1.05, 1.15),
     col=index_colors[2],
     lwd=10
     )

legend(x=1.05,
       y=1.0,
       legend=index_names,
       fill=index_colors,
       lty=0,
       cex=2
      )
dev.off()


#
