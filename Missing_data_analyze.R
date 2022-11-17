miss <- function(x){sum(is.na(x))/length(x)}
apply(tmp_missing,2,miss)

md.pattern(tmp_missing)
tmp_m<-data.frame(HR=tmp_missing[1:nrow(tmp_missing),1],RR=tmp_missing[1:nrow(tmp_missing),2],BPs=tmp_missing[1:nrow(tmp_missing),3],BPd=tmp_missing[1:nrow(tmp_missing),4],BPm=tmp_missing[1:nrow(tmp_missing),5],T=tmp_missing[1:nrow(tmp_missing),6],W=tmp_missing[1:nrow(tmp_missing),7],PaCO2=tmp_missing[1:nrow(tmp_missing),8],PaO2=tmp_missing[1:nrow(tmp_missing),9],FiO2=tmp_missing[1:nrow(tmp_missing),10],SpO2=tmp_missing[1:nrow(tmp_missing),11],GCS=tmp_missing[1:nrow(tmp_missing),12],WBC=tmp_missing[1:nrow(tmp_missing),13],TBI=tmp_missing[1:nrow(tmp_missing),14],PC=tmp_missing[1:nrow(tmp_missing),15],ALB=tmp_missing[1:nrow(tmp_missing),16],PH=tmp_missing[1:nrow(tmp_missing),17],Ca=tmp_missing[1:nrow(tmp_missing),18],AC=tmp_missing[1:nrow(tmp_missing),19],Hb=tmp_missing[1:nrow(tmp_missing),20],Mg=tmp_missing[1:nrow(tmp_missing),21],PTT=tmp_missing[1:nrow(tmp_missing),22],K=tmp_missing[1:nrow(tmp_missing),23],BUN=tmp_missing[1:nrow(tmp_missing),24],Cl=tmp_missing[1:nrow(tmp_missing),25],INR=tmp_missing[1:nrow(tmp_missing),26],Na=tmp_missing[1:nrow(tmp_missing),27],TCO2=tmp_missing[1:nrow(tmp_missing),28],Cr=tmp_missing[1:nrow(tmp_missing),29],PT=tmp_missing[1:nrow(tmp_missing),30])
mice_plot <- aggr(tmp_missing, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(tmp_m), cex.axis=.7,gap=3, ylab=c("Missing data","Pattern"))
matrixplot(tmp_missing)

#mice
tmp_mice <- mice(tmp_m, m=5, maxit = 50, method = "pmm", seed = 500)
export(complete(tmp_mice, 2), "tmp_mice_cart.csv")
#pmm #cart #rf #mean
summary(tmp_mice)

fit=with(tmp_mice,lm(HR ~ RR+BPs+BPd+BPm+T+W+PaCO2+PaO2+FiO2+SpO2+GCS+WBC+TBI+PC+ALB+PH+Ca+AC+Hb+Mg+PTT+K+BUN+Cl+INR+Na+TCO2+Cr+PT))
summary(fit)
export(summary(fit), "mice_pmm_fit_summary.csv")
pooled=pool(fit)
pooled
export(summary(fit), "mice_cart_pool.csv")
pool.r.squared(fit)

#knn
tmp_knn <- kNN(tmp_m, k = 5, numFun = weightedMean, weightDist=TRUE)








