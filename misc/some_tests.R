##########################################################
# Test-File for record swapping
# tests for all functions in recordSwap.cpp
#

library(wrswoR)
library(microbenchmark)
library(recordSwapping)


############################
# test order data
# data will be ordered by hid per default
B <- 100
orderTRUE <- replicate(B,{
  dat <- recordSwapping:::create.dat(100000)
  
  dat_order <- dat[sample(.N)]
  dat_transpose <- transpose(dat_order)
  dat_order <- as.data.table(recordSwapping:::orderData_cpp(dat_transpose,4))
  dat_order <- transpose(dat_order)
  setnames(dat_order,colnames(dat_order),colnames(dat))
  all.equal(dat$hid,dat_order$hid)
})

table(orderTRUE)

############################
# test setRisk
# if not supplied by user this can be done by using k anonymity
# risk will be setup
# then sampling probability ~ risk
dat <- recordSwapping:::create.dat(50000)
dat_t <- transpose(dat)
hierarchy <- 0:3
risk_variables <- 5:8
hid <- 4
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk <- as.data.table(transpose(risk))
risk <- cbind(hid=dat$hid,risk)

risk_direct <- dat[,.(hid)]
for(i in 1:length(hierarchy)){
  by.cols <- colnames(dat)[c(risk_variables,hierarchy)+1]
  by.cols <- by.cols[1:(length(risk_variables)+i)]
  risk_i <- dat[,.(hid,1/.N),by=c(by.cols)]
  risk_i <- unique(risk_i[,.(max(V2)),by=hid])
  setnames(risk_i,"V1",paste0("V",i))
  risk_direct <- merge(risk_direct,risk_i)
}
all.equal(risk_direct,risk,check.attributes=FALSE)

microbenchmark(cpp={
  risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
#  risk <- as.data.table(transpose(risk))
#  risk <- cbind(hid=dat$hid,risk)
},
R={
  risk_direct <- dat[,.(hid)]
  for(i in 1:length(hierarchy)){
    by.cols <- colnames(dat)[c(risk_variables,hierarchy)+1]
    by.cols <- by.cols[1:(length(risk_variables)+i)]
    risk_i <- dat[,.(hid,1/.N),by=c(by.cols)]
    risk_i <- unique(risk_i[,.(max(V2)),by=hid])
    setnames(risk_i,"V1",paste0("V",i))
    risk_direct <- merge(risk_direct,risk_i)
  }
})
############################
# test setLevels
# set swap levels for person which does not fullfill risk threshold
# again this is either supplied by the user with risk_threshold or k_anonymity
#
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:3
risk_variables <- 5:8
hid <- 4
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk_threshold <- 1/3
level_cpp <- recordSwapping:::setLevels_cpp(risk,risk_threshold)


hierarchy <- c("nuts1","nuts2","nuts3","nuts4")
risk <- c("hsize","ageGroup","gender","national")
th <- 3
dat[,level:=0]
for(i in 1:4){
  dat[level==0,N:=.N,by=c(hierarchy[1:i],risk)]
  dat[level==0,level:=as.numeric(i*(N<=th))]
}
dat[level==0,level:=5]
dat[,level:=min(level),by=hid]
dat <- dat[order(hid)]

all((dat$level-1)==level_cpp)

############################
# test sampling procedure
# test this also for partial sampling e.g. if some units have already been
# drawn skipping thos in the procedure should result in the same density function
#
#
n <- 100000
N <- 1000
Nused <- 1000
b <- 1e4
ID <- 1:n
seed.base <- 1:1e6
IDused <- rep(0,n)
startpoint <- sample(ID[1:(max(ID)-Nused)],1)
IDused[startpoint:(startpoint+Nused-1)] <- 1

prob <- sample(c(rnorm(n/2,mean=10,sd=2),rnorm(n/2,mean=100,sd=20)))

microbenchmark(cpp_samp = recordSwapping:::randSample_cpp(1:n,N,prob,IDused,seed=sample(seed.base,1)))

out_mat <- replicate(b,{
  cpp_samp <- recordSwapping:::randSample_cpp(1:n,N,prob,IDused,seed=sample(seed.base,1))
  R_samp <- sample(ID[IDused==0],N,prob=prob[IDused==0])
  wrswoR_samp <- wrswoR::sample_int_crank(n-Nused,N,prob[IDused==0])
  wrswoR_samp <- ID[IDused==0][wrswoR_samp]
  cbind(cpp_samp,
        R_samp,wrswoR_samp)
})


out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:3,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))

############################
# test distribution of swaprate  
# at lowest level
# 
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:3
swaprate <- .1
hid <- 4

seed.base <- 1:1e6
distdraw <- as.data.table(recordSwapping:::distributeDraws_cpp(dat_t,hierarchy,hid,swaprate,sample(seed.base,1)))
distdraw <- transpose(distdraw)

# compare with R solution
hier <- colnames(dat)[hierarchy+1]
distdrawR <- dat[!duplicated(hid),.(ngroup=.N),by=c(hier)]
distdrawR[,draw:=recordSwapping:::randomRound(ngroup*(swaprate/2))]

setnames(distdraw,colnames(distdraw),colnames(distdrawR))

comp_dist <- distdraw[distdrawR,,on=c(hier)]

cond1 <- nrow(comp_dist[ngroup!=i.ngroup])==0
cond2 <- comp_dist[,sum(draw)==sum(i.draw)]
cond3 <- nrow(comp_dist[abs(draw-i.draw)>1])==0

cond1&cond2&cond3

############################
# distribute draws2
Nh <- 60000
dat <- recordSwapping:::create.dat(Nh)
dat_t <- transpose(dat)
hierarchy <- 0:3
swaprate <- .1
hid <- 4

risk <- matrix(rnorm(4*nrow(dat),mean=sample(50:100,10,replace=TRUE),sd=sample(5:10,10,replace=TRUE)),ncol=4,nrow=nrow(dat))
risk <- as.data.table(risk)

risk_t <- transpose(risk)

seed.base <- 1:1e6

distdraw2 <- as.data.table(recordSwapping:::distributeDraws2_cpp(dat_t,risk_t,hierarchy,hid,swaprate,sample(seed.base,1)))

# build same in R
datComp <- cbind(dat,risk)
totalSwaps <- recordSwapping:::randomRound(Nh*swaprate/2)

# get ratios for lowest level hierarchies
riskSum <- colSums(risk)
riskRatio <- datComp[,.(ratio=max(sum(V4)/riskSum[4],uniqueN(hid)/Nh)),by=c(paste0("nuts",1:4))]
riskRatio[,ratio:=ratio/sum(ratio)]
riskRatio[,n:=recordSwapping:::randomRound(totalSwaps*ratio)]

riskRatio



setorderv(riskRatio,paste0("nuts",1:4))



############################
# test sampling of donor
# make this more modular and more efficient
# sampling in different hierarchies should be easy when using just the std::vector<std::vector<double>> risk
# 
dat <- recordSwapping:::create.dat(10000)
dat[,hid:=.I]
dat_t <- transpose(dat)
hierarchy <- 0
risk_variables <- 5:8
hid <- 4
similar <- list(c(5))
risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
dat$prob <- unlist(transpose(risk))

nsamp <- 3000
swap_pool <- sample(unique(dat$nuts1),1)
IDswap_vec <- dat[nuts1==swap_pool,sample(.I,nsamp)]-1 
IDswap_pool_vec <- dat[nuts1==swap_pool,which=TRUE]-1

sim_names <- colnames(dat)[unlist(similar)+1]
n_by_similar <- dat[IDswap_vec+1,.N,by=c(sim_names)]
dat <- n_by_similar[dat,,on=c(sim_names)]


b <- 1000
out_mat <- replicate(b,{
  seed <- sample(1:1e6,1)
  set.seed(seed)
  IDdonor <- recordSwapping:::sampleDonor_cpp(dat_t,similar,hid,IDswap_vec,IDswap_pool_vec, dat$prob,seed)
  IDdonor_R <- dat[nuts1!=swap_pool,sample(.I-1,N[1],prob=prob),by=c(sim_names)][,V1]
  cbind(IDdonor,
        IDdonor_R)
})
out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:2,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")

library(ggplot2)
ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))


############################
# check recordSwap as a whole
# 
dat <- recordSwapping:::create.dat(100000)
dat_t <- transpose(dat)
hierarchy <- 0:2
risk_variables <- 5:6
k_anonymity <- 3
swaprate <- .1
hid <- 4
similar <- list(c(5))

risk <- recordSwapping:::setRisk_cpp(dat_t,hierarchy,risk_variables,hid)
risk_threshold <- 1/k_anonymity
level_cpp <- recordSwapping:::setLevels_cpp(risk,risk_threshold)
table(level_cpp)

# run recordSwap()
dat_swapped <-recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])


# run recordSwap() with
# k_anonymity=0
k_anonymity <- 0
dat_swapped <-recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.2
swaprate <- .2
dat_swapped <-recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# and 1 hierarchy level
hierarchy <- 0
swaprate <- 0.1
dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# with multiple similarity profiles
hierarchy <- 0:2
k_anonymity <- 1
similar <- list(c(0,5,9,10),c(5,9))

dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])

# run recordSwap() with 
# k_anonymity=0 and swaprate=0.1
# where donor cannot be found
hierarchy <- 0:1
k_anonymity <- 1
similar <- list(c(0:1))

dat_swapped <- recordSwap(dat,similar,hierarchy,risk_variables,hid,k_anonymity,swaprate)

dat_compare <- merge(dat[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],
                     dat_swapped[,.(paste(geo=nuts1[1],nuts2[1],nuts3[1],nuts4[1],sep="_")),by=hid],by="hid")
# number of swapped households
nrow(dat_compare[V1.x!=V1.y])


############################
# plot benchmarks
library(ggplot2)
load("R/benchmark_cpp.RData")
str(mb_all)
mb_all <- rbindlist(mb_all)

p1 <- ggplot(mb_all,aes(factor(npop),time))+
  geom_boxplot(aes(fill=factor(hier.levels)))
plot(p1)

############################
# some additional tests
N <- 10000
n <- 500
x <- sample(1:N)
x <- 1:N
mustSwap <- c(200:300)
prob <- c(rnorm(N-n,mean=5),rnorm(n,mean=30))
seed <- sample(1:1e6,1)
recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
seed <- sample(1:1e6,1)
recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)
c <- sample(x,n,prob=prob)
a <- recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
b <- recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)

microbenchmark(comp=recordSwapping:::test_comparator(x,prob,mustSwap,n,seed),
               queue=recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed),
               R=sample(x,n,prob=prob)
               # wrswoR = x[wrswoR::sample_int_crank(N,n,prob)]
               )

b <- 1000
out_mat <- replicate(b,{
  seed <- sample(1:1e6,1)
  comp <- recordSwapping:::test_comparator(x,prob,mustSwap,n,seed)
  queue <- recordSwapping:::test_prioqueue(x,prob,mustSwap,n,seed)
  R <- sample(x[-mustSwap],n-length(mustSwap),prob=prob[-mustSwap])
  R <- c(mustSwap,R)
  wrswoR <-  x[-mustSwap][wrswoR::sample_int_crank(N-length(mustSwap),n-length(mustSwap),prob[-mustSwap])]
  wrswoR <- c(mustSwap,wrswoR)
  cbind(comp,queue,R,wrswoR)
})

out_data <- list()
for(i in 1:b){
  out_tmp <- out_mat[,1:4,i]
  out_tmp <- data.table(value=out_tmp,run=i)
  out_data <- c(out_data,list(out_tmp))
}

out_data <- rbindlist(out_data)
out_data <- melt(out_data,id.vars="run")
out_data
library(ggplot2)
p1 <- ggplot(out_data,aes(value))+
  geom_density(aes(color=variable))
plot(p1)
p1 <- ggplot(out_data[value>9200],aes(value))+
  geom_bar(aes(color=variable,fill=variable))
plot(p1)

library(plotly)
ggplotly(p1)



##### test stuff
seed <- sample(1:1e6,1)
ratios <- runif(100)
ratios <- ratios/sum(ratios)
totalDraws <- sample(100:1e6,1)

output <- recordSwapping:::distributeRandom_cpp(ratios,totalDraws,seed)
sum(output)-totalDraws

B <- 1000
check <- rep(FALSE,B)
for(i in 1:B){
  
  seed <- sample(1:1e6,1)
  ratios <- runif(sample(10:1000,1))
  ratios <- ratios/sum(ratios)
  totalDraws <- sample(0:10000,1)
  
  output <- recordSwapping:::distributeRandom_cpp(ratios,totalDraws,seed)
  check[i] <- abs(sum(output)-totalDraws)<1e-10
  if(!check[i]){
    stop()
  }
}
all(check)

microbenchmark(recordSwapping:::distributeRandom_cpp(ratios,totalDraws,seed))

############
# test loop
library(data.table)
library(recordSwapping)

B <- 1000
check <- rep(FALSE,B)
for(i in 1:B){
  
  N <- sample(1000:1e6,1)
  dat <- recordSwapping:::create.dat(1000)
  dat <- dat[,mget(paste0("nuts",1:4))]
  
  risk <- matrix(rnorm(prod(dim(dat)),mean=sample(50:100,10,replace=TRUE),sd=sample(5:10,10,replace=TRUE)),ncol=ncol(dat),nrow=nrow(dat))
  risk <- as.data.table(risk)
  
  risk_t <- transpose(risk)
  dat_t <- transpose(dat)
  
  Cppres <- recordSwapping:::testLoop_cpp(dat_t,  risk_t)
  
  Rcomp <- cbind(dat,risk)
  Rres <- list()
  for(h in 1:4){
    Rcomp_h <- Rcomp[,sum(get(paste0("V",h))),by=c(paste0("nuts",1:h))]
    Rres <- c(Rres,list(Rcomp_h))
  }
  
  Rres <- rbindlist(Rres,use.names=TRUE,fill=TRUE)
  setorderv(Rres,paste0("nuts",1:4))
  Rres[,cpp:=Cppres]
  check[i] <- nrow(Rres[abs(V1-cpp)>1e-9])==0
}

table(check)
