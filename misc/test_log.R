
library(data.table)
devtools::load_all("~/recordSwapping")
# devtools::install_github("sdcTools/recordSwapping", force=TRUE, build_vignettes = TRUE, build_opts=c("--no-resave-data", "--no-manual"))
# library(recordSwapping)
# read data
dat <- fread("~/sdctestdata/data/testdata/test_data_10k.csv.gz")

# 1.b convert variables  
dat[,Y_coord:=as.integer(substr(L001000,5,8))]
dat[,X_coord:=as.integer(substr(L001000,10,13))]
dat[,L001000:=NULL]

dat[,.N,by=.(Size)] # number of persons per household size
dat[,Size:=pmin(5,Size)] # truncate Size with 5
dat[,.N,by=.(Size)] # number of persons per household size after transformation

age_levels <- levels(cut(1:101,breaks=c(seq(0,100,5),Inf),include.lowest = TRUE,right=FALSE))
dat[,AGE.M:=as.integer(factor(AGE.M,levels=age_levels))]
check_non_swap <- rep(FALSE,100)
for(i in 1:100){
  dat_test <- dat[Size==1,.SD[sample(.N,100)],by=.(NUTS1)]
  dat_test <- rbind(dat_test,dat[Size==2 & NUTS1==2][HID %in% sample(unique(HID),1)])
  dat_test[,RISK_VAR:=1]
  dat_test[Size==2,RISK_VAR:=2]
  # 
  # dat_test[,risk1:=1/.N,by=c("NUTS1",risk_variables)]
  # dat_test[,risk1:=max(risk1),by=.(HID)]
  # 
  # index_risk <- recordSwapping:::checkIndexString(x = "RISK_VAR",cnames=colnames(dat_test),minLength=1)
  # direct_risk <- recordSwapping:::setRisk_cpp(transpose(dat_test),hierarchy = c(0),risk_variables = index_risk,hid=4)
  # direct_risk <- transpose(direct_risk)
  # direct_risk <- as.data.table(direct_risk)
  
  
  hierarchy <- c("NUTS1")
  hid <- "HID"
  risk_variables <- c("RISK_VAR")
  k_anonymity <- 3
  swaprate <- 0
  similar <- "Size"
  seed <- 202103
  carry_along = c("NUTS3",
                  "LAU2","Y_coord",
                  "X_coord")
  data = copy(dat_test)
  risk=NULL
  risk_threshold=0
  dat_swapped_5 <- recordSwap(data = dat_test, hid = hid,
                              hierarchy = hierarchy,
                              similar = similar,
                              risk_variables = risk_variables,
                              k_anonymity = k_anonymity,
                              carry_along = carry_along,
                              swaprate = 0.05,
                              return_swapped_id = TRUE,
                              seed = seed)
  
  log_id <- readLines("TRS_logfile.txt")[-c(1:2)]
  log_id <- as.numeric(trimws(log_id))
  check_non_swap[i] <- all(dat_test[Size==2]$HID %in% log_id & log_id %in% dat_test[Size==2]$HID)
}
table(check_non_swap)

# test custom risk measures
dat <- fread("~/sdctestdata/data/testdata/test_data_10k.csv.gz")

# 1.b convert variables  
dat[,Y_coord:=as.integer(substr(L001000,5,8))]
dat[,X_coord:=as.integer(substr(L001000,10,13))]
dat[,L001000:=NULL]

dat[,.N,by=.(Size)] # number of persons per household size
dat[,Size:=pmin(5,Size)] # truncate Size with 5
dat[,.N,by=.(Size)] # number of persons per household size after transformation

age_levels <- levels(cut(1:101,breaks=c(seq(0,100,5),Inf),include.lowest = TRUE,right=FALSE))
dat[,AGE.M:=as.integer(factor(AGE.M,levels=age_levels))]

hierarchy <- c("NUTS1","NUTS2")
hid <- "HID"
risk_variables <- c("COC.M","POB.M")
k_anonymity <- 3
swaprate <- 0.05
similar <- "Size"
seed <- 202103
similar <- c("Size","HST")
carry_along = c("NUTS3",
                "LAU2","Y_coord",
                "X_coord")


# define set of parameters
seed_set <- sample(202100:222000,10)
risk_variables_set <- list(c("COC.M","POB.M"),
                           c("COC.L","POB.L","AGE.M"),
                           c("INCOME_Q","SEX","COC.M","POB.M"))
k_anonymity <- c(2,3,5,8)

params <- expand.grid(seed_set,risk_variables_set,k_anonymity)
risk_equal <- output_equal <- rep(FALSE,nrow(params))

for(i in 1:nrow(params)){
  
  seed <- as.integer(params[i,1])
  risk_variables <- params[i,2][[1]]
  k_anonymity <- as.integer(params[i,3])
  
  dat[,risk1:=1/.N,by=c(hierarchy[1],risk_variables)]
  dat[,risk1:=max(risk1),by=.(HID)]
  dat[,risk2:=1/.N,by=c(hierarchy[1:2],risk_variables)]
  dat[,risk2:=max(risk2),by=.(HID)]
  risk <- c("risk1","risk2")

  index_risk <- recordSwapping:::checkIndexString(x = risk_variables,cnames=colnames(dat),minLength=1)
  direct_risk <- recordSwapping:::setRisk_cpp(transpose(dat),hierarchy = c(0,1),risk_variables = index_risk,hid=4)
  direct_risk <- transpose(direct_risk)
  direct_risk <- as.data.table(direct_risk)

  risk_equal[i] <- all.equal(direct_risk,dat[,.(risk1,risk2)],check.attributes = FALSE)
  
  dat_swapped_cr <- recordSwap(data = dat, hid = hid,
                               hierarchy = hierarchy,
                               similar = similar,
                               risk= risk,
                               risk_threshold = 1/k_anonymity,
                               carry_along = carry_along,
                               swaprate = 0.05,
                               return_swapped_id = TRUE,
                               seed = seed)
  
  dat_swapped_cr[!duplicated(HID),.N,by=.(id_swapped = HID!=HID_swapped)]
  
  dat_swapped <- recordSwap(data = dat, hid = hid,
                            hierarchy = hierarchy,
                            similar = similar,
                            risk_variables = risk_variables,
                            k_anonymity = k_anonymity,
                            carry_along = carry_along,
                            swaprate = 0.05,
                            return_swapped_id = TRUE,
                            seed = seed)
  
  
  dat_swapped[!duplicated(HID),.N,by=.(id_swapped = HID!=HID_swapped)]
  
  output_equal[i] <- all.equal(dat_swapped,dat_swapped_cr)
  
  
}
table(output_equal)
table(risk_equal)

