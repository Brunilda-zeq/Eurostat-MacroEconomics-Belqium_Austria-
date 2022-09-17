
# Start from scratch
rm(list=ls(all=TRUE))

# Set the working directory!

# load libraries
library("eurostat")   # package to download data from EUROSTAT
library("lubridate")  # package to deal with dates easily
library("tempdisagg") # package for disaggregating Quarterly to Monthly variables
library("zoo")        # package to use na.approx() -- if needed!

# Load some extra necessary functions
source("Aux_Functions.R")

# Set the country, or countries, 
# cous <- c("BE", "DE", "EL", "ES", "FR", "IT", "NL")
cous <- "BE"

# Do you want to manually construct ESI?
# LEAVE IT ALWAYS FALSE
esi.fotis <- FALSE  # if true, then we manually construct ESI

# Nothing else to check beyond this point... unless you want
# to add more data!!!
##################################################################################
##################################################################################
##################################################################################
for(icou in 1:NROW(cous))
{
  # clean cache (in case it's not empty)
  clean_eurostat_cache();
  
  # loop across cous
  cou <- cous[icou]
  
  # start the data extraction
  xall <- list()
  
  # HD1: Industrial Production Index (Including Construction)
  xxnam <- "HD1"
  xtemp <- get_eurostat("sts_inpr_m")
  xtemp <- subset(xtemp, s_adj=="SCA" & geo==cou & indic_bt=="PROD"
                  & unit=="I15" & nace_r2=="B-D")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD2: Exports
  xxnam <- "HD2"
  xtemp <- get_eurostat("ei_etea19_m")
  xtemp <- subset(xtemp, stk_flow=="EXP" & geo==cou & unit=="IVOL-SA"
                  & partner=="EXT_EA19" & indic=="ET-T")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD3: Imports
  xxnam <- "HD3"
  xtemp <- get_eurostat("ei_etea19_m")
  xtemp <- subset(xtemp, stk_flow=="IMP" & geo==cou & unit=="IVOL-SA"
                  & partner=="EXT_EA19" & indic=="ET-T")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD4: Production in construction - monthly data (apart from Greece)
  xxnam <- "HD4"
  if(cou=="BE"){
    xtemp <- get_eurostat("sts_copr_q")
  }else{
    xtemp <- get_eurostat("sts_copr_m")
  }
  xtemp <- subset(xtemp, indic_bt=="PROD" & geo==cou & nace_r2=="F"
                  & s_adj=="SCA" & unit=="I15")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  if(cou=="BE"){xtemp <- QtoM(xtemp)}
  xall <- c(xall, list(xtemp))
  
  # HD5: Unemployment Rate
  xxnam <- "HD5"
  xtemp <- get_eurostat("une_rt_m")
  xtemp <- subset(xtemp, s_adj=="SA" & age=="TOTAL" & unit=="PC_ACT"
                  & sex=="T" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD6: Short-term yields
  xxnam <- "HD6"
  xtemp <- get_eurostat("irt_st_m")
  xtemp <- subset(xtemp, int_rt=="IRT_M3" & geo=="EA")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD7: long-term yields
  xxnam <- "HD7"
  xtemp <- get_eurostat("irt_euryld_m")
  xtemp <- subset(xtemp, yld_curv=="SPOT_RT" & bonds=="CGB_EA"
                  & maturity=="Y10" & geo=="EA")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD8: retail sales
  xxnam <- "HD8"
  xtemp <- get_eurostat("sts_trtu_m")
  if(cou=="BE"){
    xtemp <- subset(xtemp, indic_bt=="TOVV" & nace_r2=="G472" & s_adj=="SCA"
                    & unit=="I15" & geo==cou)
  }else{
    xtemp <- subset(xtemp, indic_bt=="TOVV" & nace_r2=="G47" & s_adj=="SCA"
                    & unit=="I15" & geo==cou)
  }
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # HD9: eurodollar
  xxnam <- "HD9"
  xtemp <- get_eurostat("ert_bil_eur_m")
  xtemp <- subset(xtemp, statinfo=="AVG" & unit=="NAC" & currency=="USD")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD1: Production development observed over the past 3 months
  xxnam <- "SD1"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-IPT" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD2: Production expectations over the next 3 months
  xxnam <- "SD2"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-IPE" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD3: Employment expectations over the next 3 months (Retail Survey)
  xxnam <- "SD3"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-IEME" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD4: Assessment of order-book levels  [Industry]
  xxnam <- "SD4"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-IOB" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD5: Assessment of export order-book levels [Industry]
  xxnam <- "SD5"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-IEOB" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD6: Assessment of the current level of stocks of finished products [Industry]
  xxnam <- "SD6"
  xtemp <- get_eurostat("ei_bsin_m_r2")
  xtemp <- subset(xtemp, indic=="BS-ISFP" & s_adj=="SA"
                  & unit=="BAL" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD7: Building activity development over the past 3 months [Construction]
  xxnam <- "SD7"
  xtemp <- get_eurostat("ei_bsbu_m_r2")
  xtemp <- subset(xtemp, indic=="BS-CTA-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD8: Evolution of the current overall order books [Construction]
  xxnam <- "SD8"
  xtemp <- get_eurostat("ei_bsbu_m_r2")
  xtemp <- subset(xtemp, indic=="BS-COB-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD9: Employment expectations over the next 3 months [Construction]
  xxnam <- "SD9"
  xtemp <- get_eurostat("ei_bsbu_m_r2")
  xtemp <- subset(xtemp, indic=="BS-CEME-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD10: Price expectations over the next 3 months [Construction]
  xxnam <- "SD10"
  xtemp <- get_eurostat("ei_bsbu_m_r2")
  xtemp <- subset(xtemp, indic=="BS-CPE-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD11: Business activity (sales) development over the past 3 months [Retail]
  xxnam <- "SD11"
  xtemp <- get_eurostat("ei_bsrt_m_r2")
  xtemp <- subset(xtemp, indic=="BS-RPBS" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD12: Volume of stocks currently hold [Retail]
  xxnam <- "SD12"
  xtemp <- get_eurostat("ei_bsrt_m_r2")
  xtemp <- subset(xtemp, indic=="BS-RAS" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD13: Expectations of the number of orders over the next 3 months [Retail]
  xxnam <- "SD13"
  xtemp <- get_eurostat("ei_bsrt_m_r2")
  xtemp <- subset(xtemp, indic=="BS-ROP" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD14: Business activity expectations over the next 3 months [Retail]
  xxnam <- "SD14"
  xtemp <- get_eurostat("ei_bsrt_m_r2")
  xtemp <- subset(xtemp, indic=="BS-REBS" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD15: Employment expectations over the next 3 months [Retail]
  xxnam <- "SD15"
  xtemp <- get_eurostat("ei_bsrt_m_r2")
  xtemp <- subset(xtemp, indic=="BS-REM" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD16: Business Situation over the past 3 months [Services]
  xxnam <- "SD16"
  xtemp <- get_eurostat("ei_bsse_m_r2")
  xtemp <- subset(xtemp, indic=="BS-SABC" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD17: Evolution of Demand over the past 3 months [Services]
  xxnam <- "SD17"
  xtemp <- get_eurostat("ei_bsse_m_r2")
  xtemp <- subset(xtemp, indic=="BS-SARM" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD18: Expectation of Demand over the next 3 months [Services]
  xxnam <- "SD18"
  xtemp <- get_eurostat("ei_bsse_m_r2")
  xtemp <- subset(xtemp, indic=="BS-SAEM" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD19: Evolution of Employment over the past 3 months [Services]
  xxnam <- "SD19"
  xtemp <- get_eurostat("ei_bsse_m_r2")
  xtemp <- subset(xtemp, indic=="BS-SERM" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD20: Expectation of Employment over the next 3 months [Services]
  xxnam <- "SD20"
  xtemp <- get_eurostat("ei_bsse_m_r2")
  xtemp <- subset(xtemp, indic=="BS-SEEM" & s_adj=="SA" & geo==cou & unit=="BAL")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD21: Euro-zone Business Climate Indicator - monthly data
  xxnam <- "SD21"
  xtemp <- get_eurostat("ei_bsci_m_r2")
  xtemp <- subset(xtemp, indic=="BS-BCI" & s_adj=="SA" & geo=="EA19")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD22: Construction confidence indicator
  xxnam <- "SD22"
  xtemp <- get_eurostat("ei_bssi_m_r2")
  xtemp <- subset(xtemp, indic=="BS-CCI-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD23: Economic sentiment indicator
  xxnam <- "SD23"
  if(esi.fotis==TRUE){
    xtemp <- calculate.ESI(cou, xxnam)
  }else{
    xtemp <- get_eurostat("ei_bssi_m_r2")
    xtemp <- subset(xtemp, indic=="BS-ESI-I" & s_adj=="SA" & geo==cou)
    xtemp <- extract.data.matrix(xtemp, xxnam)
  }
  xall <- c(xall, list(xtemp))
  
  # SD24: Industrial confidence indicator
  xxnam <- "SD24"
  xtemp <- get_eurostat("ei_bssi_m_r2")
  xtemp <- subset(xtemp, indic=="BS-ICI-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD25: Retail Confidence Indicator
  xxnam <- "SD25"
  xtemp <- get_eurostat("ei_bssi_m_r2")
  xtemp <- subset(xtemp, indic=="BS-RCI-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD26: Consumer Confidence Indicator
  xxnam <- "SD26"
  xtemp <- get_eurostat("ei_bssi_m_r2")
  xtemp <- subset(xtemp, indic=="BS-CSMCI-BAL" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD27: Financial situation over the last 12 Months [Consumer]
  xxnam <- "SD27"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-FS-LY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD28: Financial situation over next 12 Months [Consumer]
  xxnam <- "SD28"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-FS-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD29: General economic situation over the last 12 months [Consumer]
  xxnam <- "SD29"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-GES-LY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD30: General economic situation over the next 12 months [Consumer]
  xxnam <- "SD30"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-GES-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD31: Price Trends over the last 12 months [Consumer]
  xxnam <- "SD31"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-PT-LY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD32: Price Trends over the next 12 months [Consumer]
  xxnam <- "SD32"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-PT-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD33: Unemployment Expectations over the next 12 months [Consumer]
  xxnam <- "SD33"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-UE-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD34: Major purchases over  next 12 months [Consumer]
  xxnam <- "SD34"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-MP-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # SD35: Savings over the next 12 months  [Consumer]
  xxnam <- "SD35"
  xtemp <- get_eurostat("ei_bsco_m")
  xtemp <- subset(xtemp, indic=="BS-SV-NY" & s_adj=="SA" & geo==cou)
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # Y: GDP and main components (chain linked volumes (2010), mill. eur)
  xxnam <- "Y"
  xtemp <- get_eurostat("namq_10_gdp")
  xtemp <- subset(xtemp, unit=="CLV10_MEUR" & s_adj=="SCA"
                  & geo==cou & na_item=="B1GQ")
  xtemp <- extract.data.matrix(xtemp, xxnam)
  xall <- c(xall, list(xtemp))
  
  # As in Giannone et al., create a monthly version
  # of the quarterly GDP which has the same value for all
  # three months of the quarter
  Y <- xall[[NROW(xall)]]
  fdt <- as.Date(rownames(Y)[1])
  Yts <- ts(as.numeric(Y), frequency=4, start=c(year(fdt), quarter(fdt)))
  YL <- td(Yts~1, to="monthly", method="denton-cholette")
  Ym <- YL$values
  Ymv <- as.numeric(Ym)
  Ymd <- as.Date(Ym)
  for(i in 1:NROW(Ymd))
  {
    day(Ymd[i]) <- days_in_month(Ymd[i])
  }
  Ymv <- as.matrix(Ymv)
  rownames(Ymv) <- as.character(Ymd)
  colnames(Ymv) <- "Ym"
  
  # So, using the above method we can obtain a monthly series
  # which is disaggregated - from that we can use the dates
  # but in the Giannone paper, they apply the same value 
  # for each month in the quarter. And then they choose the
  # third month. This seems to be more robust as unnecessary uncertainty
  # is removed.
  Yfinal <- Ymv
  for(i in 1:NROW(Yfinal))
  {
    Ydt <- as.Date(rownames(Yfinal)[i])
    Ydy <- year(Ydt)
    Ydq <- quarter(Ydt)
    Yqq <- as.Date(rownames(Y))
    Yposi <- which((year(Yqq)==Ydy)&(quarter(Yqq)==Ydq))
    Yfinal[i] <- Y[Yposi]
  }
  
  xall <- c(xall, list(Yfinal))
  xallsave <- xall
  
  # Go through each variable and delete the NAs (if any)
  for(i in 1:NROW(xall))
  {
    xtemp <- xall[[i]]
    xall[[i]] <- na.omit(xtemp)
  }
  
  # Check the starting, end date for each variable and export them
  start.date <- {}
  end.date <- {}
  cnames <- {}
  for(i in 1:NROW(xall))
  {
    xtemp <- xall[[i]]
    cnames <- c(cnames, colnames(xtemp))
    start.date <- c(start.date, as.character(rownames(xtemp)[1]))
    end.date <- c(end.date, as.character(rownames(xtemp)[NROW(xtemp)]))
  }
  write.csv(cbind(cnames,start.date, end.date),
            paste("from_to_dates_", cou,".csv", sep=""))
  
  # Go through each variable using today's month/quarter and 
  # add NA values if appropriate
  tdy <- today()
  day(tdy) <- days_in_month(tdy)
  tqyd <- today() #+30
  tqyq <- quarter(tqyd)
  tqy <- tqyd
  if(tqyq==1){ month(tqy) <- 1 }
  if(tqyq==2){ month(tqy) <- 4 }
  if(tqyq==3){ month(tqy) <- 7 }
  if(tqyq==4){ month(tqy) <- 10 }
  day(tqy) <- days_in_month(tqy)
  
  for(i in 1:NROW(xall))
  {
    xtemp <- xall[[i]]
    xds <- as.Date(rownames(xtemp))
    xdsm <- max(xds)
    freq <- interval(xds[NROW(xds)-1], xds[NROW(xds)]) %/% months(1)
    if(freq==1){
      if(xdsm<tdy){
        newds <- xds
        while(max(newds)<tdy)
        {
          newd <- max(newds)%m+%months(1)
          day(newd) <- days_in_month(newd)
          newds <- c(newds, newd)
        }
        newd <- newds
        xtemp2 <- matrix(NA, NROW(newd), NCOL(xtemp))
        rownames(xtemp2) <- as.character(newd)
        colnames(xtemp2) <- colnames(xtemp)
        xtemp2[rownames(xtemp),] <- xtemp
      }else{
        xtemp2 <- xtemp
      }
      xall[[i]] <- xtemp2
    }
    if(freq==3){
      if(xdsm<tqy){
        newds <- xds
        while(max(newds)<tqy)
        {
          newd <- max(newds)%m+%months(3)
          newds <- c(newds, newd)
        }
        newd <- newds
        xtemp2 <- matrix(NA, NROW(newd), NCOL(xtemp))
        rownames(xtemp2) <- as.character(newd)
        colnames(xtemp2) <- colnames(xtemp)
        xtemp2[rownames(xtemp),] <- xtemp
      }else{
        xtemp2 <- xtemp
      }
      xall[[i]] <- xtemp2
    }
  }

  # Use common information of all variables
  # including 10Y [HD7]
  # that is from "2004-09-30" until the end of the series
  
  # get all dates from 2004-09-30
  dwc <- rownames(xall[[NROW(xall)]])
  dwccheck <- which(dwc=="2004-09-30")
  if(NROW(dwccheck)==0){
    dwc <- rownames(xall[[NROW(xall)]])
    dwc <- dwc[which(dwc=="2007-04-30"):NROW(dwc)]
  }else{
    dwc <- dwc[which(dwc=="2004-09-30"):NROW(dwc)]
  }
  
  # data matrices
  # dm1: GDP-M
  # dm2: Survey
  # dm3: Hard
  # dm4: GDP-Q
  dm1 <- matrix(NA, NROW(dwc), 1)
  rownames(dm1) <- dwc; colnames(dm1) <- "GDP"
  
  dm2 <- matrix(NA, NROW(dwc), 9)
  rownames(dm2) <- dwc; colnames(dm2) <- paste("HD", 1:NCOL(dm2), sep="")
  
  dm3 <- matrix(NA, NROW(dwc), 35)
  rownames(dm3) <- dwc; colnames(dm3) <- paste("SD", 1:NCOL(dm3), sep="")
  
  dm4 <- xall[[46]]; colnames(dm4) <- colnames(dm1)
  
  dm1 <- align.data.dates(dm1, xall[46])
  dm2 <- align.data.dates(dm2, xall[1:9])
  dm2 <- cbind(dm2, dm2[,"HD7"]-dm2[,"HD6"])
  colnames(dm2) <- paste("HD", 1:NCOL(dm2), sep="")
  dm3 <- align.data.dates(dm3, xall[10:44])
  
  save(dm1, dm2, dm3, dm4, file=paste("Step1_Data_", cou, ".RData", sep=""))
  
  cat("Now done country ", icou, "of ", NROW(cous), "\n")
}
rm(list=ls(all=TRUE))

# Set the working directory!
setwd("C:/Users/Bru/Desktop/metaptuxiako/2o/papailias")

# load libraries
library("lubridate")  # package to deal with dates easily
library("seastests")  # package to check for seasonalities

library("zoo")        # package to use na.approx() -- if needed!
library("strucchange")
# what is the data file you created in Step 1?
# put your file here keeping "" for character strings
wfile <- "Step1_Data_BE.RData"

# what is the country you use?
wcou <- "BE"

# load the data
load(wfile)


# Load some extra necessary functions
source("Aux_Functions.R")

# significance level used in tests later
sig.lvl <- 0.1

# Outliers: we define outliers as any value "out.K" sd's away from "out.m"
# out.m: can either be "mean" or "median"
out.K <- 4
out.m <- "median"

# We have the following matrices
# dm1: a vector (T x 1) which has the GDP expressed in monthly terms
# dm2: a matrix (T x K1) which has the "hard" variables
# dm3: a matrix (T x K2) which has the "soft" variables
# dm4: a vector (Tq x 1) which has the GDP expressed in quarterly terms

# Data transformations
# 0: no change
# 1: period-to-period growth rate
# 2: first difference
tm1 <- c(1)
tm2 <- c(1,1,1,1,2,2,2,1,1,2)
tm3 <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2)

# Step 1: check that there are no seasonalities
# for this, we do not have any proper methodology
# only recently we have the "seastests" package
#
# in case there are, remove only the seasonal part based on STL

# start with the target in monthly
z <- na.omit(dm1)               # remove NAs
z <- ts(z, frequency=12)         # express it as ts with the corresponding frequency
seas.test <- combined_test(z)              # apply the test
seas.check <- seas.test$stat
# if(seas.check==TRUE){ stop("Your Quarterly Target is Seasonal! Fix it manually because it is very important!")}

# continue with the target in monthly
z <- na.omit(dm4)               # remove NAs
z <- ts(z, frequency=12)         # express it as ts with the corresponding frequency
seas.test <- combined_test(z)              # apply the test
seas.check <- seas.test$stat
# if(seas.check==TRUE){ stop("Your Monthly Target is Seasonal! Fix it manually because it is very important!")}

# Check your hard variables, and if there is evidence of seasonality
# remove it using STL - this what the function based on "seastests" does
dm2 <- DR.seas(dm2, 12)
dm3 <- DR.seas(dm3, 12)

# Now, we have removed seasonalities (if any)
# make the variables stationary using the appropriate transformations
sdm2 <- transform.series(dm2, tm2)
sdm3 <- transform.series(dm3, tm3)

# Perhaps, we have made a mistake somewhere
# and did not use a proper transformation
# therefore, some of the variables might still be non-stationary
# so, we need to test for unit root and -if detected- to take
# the first difference as our last resort
sdm2 <- CRR.nonstat(sdm2, sig.lvl)
sdm3 <- CRR.nonstat(sdm3, sig.lvl)

# After the transformation, we will have some NAs
# in the first rows, so let's delete these NAs
rmNAs <- 2

# Finally, we might have outliers induced by the stationarity
# transformation or due to the source
# so, we need to remove all outliers
# for what follows, we define outliers as any value above/below
# K sd's away from the median
sdm2 <- FF.outliers(sdm2, out.K, out.m)
sdm3 <- FF.outliers(sdm3, out.K, out.m)

# And, we are now ready to export this "clean" data and continue
save(dm1, dm2, dm3, dm4, sdm2, sdm3, file=paste("Step2_Data_", wcou, ".RData", sep=""))
rm(list=ls(all=TRUE))

# Set the working directory!
setwd("C:/Users/Bru/Desktop/metaptuxiako/2o/papailias")

# load the clean dataset
load("Step2_Data_BE.RData")
plot (sdm3[,2],type = "l",col="maroon",main="Time Series ",
      xlab="Date", ylab="Production in industry")

# load necessary libraries
library("moments")
library("tsoutliers")

# load your custom functions
source("custom-functions.R")

# Table with descriptives
d1 <- getDesc(sdm2)
d2 <- getDesc(sdm3)

write.csv(d1, "D_S_hd.csv")
write.csv(d2, "D_S_sd.csv")

# Rolling AR(1) estimation

y <- sdm2[,"HD1"]
yd <- as.Date(rownames(sdm2))


plot(yd, y, type="l",col="maroon",main="Time Series ",
     xlab="Date", ylab="Production in industry")

legend("topleft",lty=1,col=c("maroon"),
       legend=c("Production in industry"),cex = 0.5)

y1 <- sdm2[,"HD5"]
yd1<- as.Date(rownames(sdm2))


plot(yd1, y1, type="l",col="maroon4",main="Time Series ",
     xlab="Date", ylab="Unemployment rate ")

legend("topleft",lty=1,col=c("maroon"),
       legend=c("Unemployment rate"),cex = 0.5)
y2 <- sdm3[,"SD17"]
yd <- as.Date(rownames(sdm3))
boxplot(y2,main = "boxplot",ylab = "Evolution of Demand ",col = "plum4",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
y3<- sdm2[,"HD4"]
yd <- as.Date(rownames(sdm2))
hist(y3,
     main="	Histogram",
     xlab="Production in construction ",
     col="indianred4",
     freq=FALSE
)

x1=sdm2[,"HD1"]
x2=sdm2[,"HD2"]
x3=sdm2[,"HD3"]
x4=sdm2[,"HD4"]
x5=sdm2[,"HD5"]
x6=sdm2[,"HD6"]

x7=sdm2[,"HD7"]
x8=sdm2[,"HD8"]
x9=sdm2[,"HD9"]
x10=sdm2[,"HD10"]

y1<- sdm3[,"SD26"]
fitnull<-lm(y1 ~ 1)
FS<-step(fitnull, 
         scope=list(lower = ~ 1, upper = ~ x1 + x2 + x3 + x4 + x5 + x6 + x7+x8),
         direction="forward")
FS

fs <- Fstats(y~x5)
plot(fs,col="wheat4",main="breakpoints")
breakpoints(fs)
lines(breakpoints(fs),col="slateblue4")
legend("topright",lty=1,col=c("wheat4","red","wheat4"),
       legend=c("Employment expectations over the next 3 months","upper bound","Breakpoint"),cex = 0.4)
fs$Fstats
fs$breakpoint

yd <- as.Date(rownames(sdm2))
N <- NROW(y)
y <- sdm3[,"SD3"]
x=sdm2[,"HD1"]
yd <- as.Date(rownames(sdm2))

wl <- 50

est_c <- matrix(NA, NROW(y), 2)
rho_c <- NULL

for(i in wl:NROW(y))
{
  # Recursive Estimation
  out <- arima(y[1:i], order=c(1,0,0))
  
  # Rolling Estimation
  out <- arima(y[(i-wl+1):i], order=c(1,0,0))
  
  out_c <- out$coef
  est_c[i,] <- out_c
  
  # Recursive correlation
  xy <- na.omit(cbind(x[1:i], y[1:i]))
  
  # rolling correlation
  xy <- na.omit(cbind(x[(i-wl+1):i], y[(i-wl+1):i]))
  rho <- cor(xy[,1], xy[,2])
  rho_c <- c(rho_c, rho)
  
}

plot(est_c[,1], type="l",col="tomato4",main="AR1",
     xlab="Time", ylab="Employment expectations ")


legend("topleft",lty=1,col=c("tomato4"),
       legend=c("Employment expectations"),cex = 0.5)

plot(rho_c, type="l",col="lightpink4",main="Correlation SD3-HD1",
xlab="Time", ylab="Correlation ")
legend("topleft",lty=1,col=c("lightpink4"),
       legend=c("Employment expectations"),cex = 0.5)

source("custom-functions.R")

# Split your target and your X's
Y <- sdm2[,"HD1"]
X <- cbind(sdm2[,2:NCOL(sdm2)], sdm3)

# remove any missing values
xy <- cbind(Y, X)
xy <- na.omit(xy)

#
# dfrom <- "2010-01-31"
# dto <- "2019-10-31"
# dseqq <- which(rownames(xy)==dfrom):which(rownames(xy)==dto)
# xy <- xy[dseqq,]

# reset
Y <- xy[,1]
X <- xy[,2:NCOL(xy)]


library("glmnet")
# Ridge Regression
outR <- cv.glmnet(x=X, y=Y, type.measure="mse",
                  nfolds=NROW(X), alpha=0,
                  standardize=TRUE)

# Check your lambda sequence and the lambda which minimises your MSE
outR$lambda.min
outR$lambda
lmin <- outR$lambda.min

# Check all lambdas and the MSE



plot(outR)
cbind(outR$lambda, log(outR$lambda))

# Check the fit
plot(outR$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(lmin), col="red")

# Extract the beta_hat for the lamda_min
b <- coef(outR, lmin)
b <- as.matrix(b)
write.csv(b, "b_Ridge.csv")


######################## LASSO

# Lasso Regression
outL <- cv.glmnet(x=X, y=Y, type.measure="mse",
                  nfolds=NROW(X), alpha=1,
                  standardize=TRUE)

# Check your lambda sequence and the lambda which minimises your MSE
outL$lambda.min
lmin <- outL$lambda.min

# Check all lambdas and the MSE
plot(outL)
cbind(outL$lambda, log(outL$lambda))

# Check the fit
plot(outL$glmnet.fit, xvar="lambda", label = TRUE)
abline(v=log(lmin), col="red")

# Extract the beta_hat for the lamda_min
b <- coef(outL, lmin)
b <- as.matrix(b)
write.csv(b, "b_Lasso.csv")

plot(b, type="b", pch=15)
abline(h=0, col="red")
### assiment 4
library("tseries")
library("pls")

# load your custom functions
source("custom-functions.R")
Y=0
X=0
# Split your target and your X's
Y <- sdm2[,"HD1"]
X <- cbind(sdm2[,1:NCOL(sdm2)], sdm3)

# X[,"HD5"] <- NA
# Identify variables with ALL NAs
# ibad <- which(colSums(is.na(X))==NROW(X))
# X <- X[,-ibad]

# remove any missing values
xy <- cbind(Y, X)
xy <- na.omit(xy)



# reset
Y <- xy[,1]
X <- xy[,1:NCOL(xy)]
kfac <- NCOL(X)
# outPCA <- pcr(Y~X, center=TRUE, scale=TRUE, ncomp=kfac)
outPCA <- pcr(Y~X, center=TRUE, scale=TRUE, ncomp=kfac, validation="CV")
# Y(t) = a + b*F(t) + e(t)
outPCAcv <- RMSEP(outPCA)
plot(outPCAcv)
outPCAcv
ve <- explvar(outPCA)
plot(ve, type="b", pch=15, col="#377eb8", main="PCA Factors, Expl. Var.",
     xlab="# Component", ylab="% Expl. Var.")

f <- as.matrix(outPCA$scores)
write.csv(f[,1], "f1.csv")
d <- as.Date(rownames(f))

plot(d, f[,1], type="l",col="mediumorchid4", main="PCA",xlab="Time",ylab=" component")
legend("topleft",lty=1,col=c("mediumorchid4"),
       legend=c("First component"),cex = 0.6)
plot(d, f[,1], type="l",col="mediumorchid4", main="PCA",xlab="Time",ylab=" component")
lines(d, f[,2], col="lime green")
lines(d, f[,3], col="orangered")
legend("topleft",lty=1,col=c("mediumorchid4","lime green","orangered"),
       legend=c("Comp 1","Comp 2","Comp 3"),cex = 0.7)

# extract the loadings
fl <- as.matrix(outPCA$loadings)
write.csv(fl, "flbe.csv")

summary(outPCA)
explvar(outPCA)
# extract the first factor and use it as your "indicator"!
z <- f[,1]
plot(d, z, type="n", main="Economic Expectations Index", xlab="Time",
     ylab="Values")
grid(col="grey")
lines(d, z, col="hotpink4", lwd=2)


plot(density(z))

jarque.bera.test(z)

# Rolling Estimation
w <- 24
wsd <- 1
wout <- matrix(NA, NROW(z), 2)
colnames(wout) <- c("Mean", "SD")
for(i in w:NROW(z))
{
  zi <- z[(i-w+1):i]
  wout[i,1] <- mean(zi)
  wout[i,2] <- sd(zi)
}

plot(d, z, type="n", main="Early Warning Indicator", xlab="Time",
     ylab="Values")
grid(col="grey")
lines(d, z, col="#e41a1c", lwd=2)
lines(d, wout[,1], col="#377eb8", lwd=2)
lines(d, wout[,1]-wsd*wout[,2], col="#377eb8", lwd=2, lty=2)
lines(d, wout[,1]+wsd*wout[,2], col="#377eb8", lwd=2, lty=2)
legend("topleft",lty=1:2,col=c("#e41a1c","#377eb8","#377eb8"),
       legend=c("Comp 1","Bounds","Average"),cex = 0.7)
new =  prcomp (X, center = TRUE, scale=TRUE)
new
importance =  summary(new)
barplot(importance$importance)
importance

plot(new$x [,1], (-1)* new$x [,2], xlab = "PC1", ylab ="PC2")
library(factoextra)
fviz_pca_var(new, col.var = "contrib", repel = TRUE)

fviz_nbclust(X, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")


library(Mcomp)
library(tsfeatures)
library(cluster)
library(mclust)
fit_kmeans = kmeans(X, centers=4, nstart=10)
clusplot(X, fit_kmeans$cluster, color=TRUE, shade=TRUE, lines=0, main="K-means")
par(mfrow=c(2,2))



fit_kmeans = kmeans(X, centers=4, nstart=10)
clusplot(X, fit_kmeans$cluster, color=TRUE, shade=TRUE, lines=0, main="K-means")
fit_agglomerative <- hclust(dist(X, method = "euclidean") , method="ward.D")
clusplot(X, cutree(fit_agglomerative, k=3) , color=TRUE, shade=TRUE, lines=0,
         main="Agglomerative")
fit_divisive <- diana(X, metric = "euclidean", stand = TRUE)
clusplot(X, cutree(as.hclust(fit_divisive), k = 3), color=TRUE, shade=TRUE, lines=0, main="Divisive")
fit_bayes <- Mclust(X, 3)
clusplot(X, fit_bayes$classification, color=TRUE, shade=TRUE, lines=0, main="Bayesian")
##########################################################################
#####
##### Section II: PLS
#####
#####  F1-PLS = w1*X1 + w2*X2 + ... + wk*Xk
#####
#####  w1 = COV(Y, X1)
#####  w2 = COV(Y, X2)
#####  .....
#####  wK = COV(Y, Xk)
#####
#####
##### F2-PLS: which part of each X is NOT explained by F1
##### X1 = gammaF1 + u1
##### X2 = gammaF1 + u2
##### .....
##### Xk = gammaF1 + uk
#####
##### w1 = COV(Y, u1), w2 = COV(Y, u2), ...., wk = COV(Y, uk)
##### F2-PLS = w1*u1 + w2*u2 + ... + wk*uk
##########################################################################
# outPLS <- plsr(Y~X, center=TRUE, scale=TRUE, ncomp=kfac)
outPLS <- plsr(Y~X, center=TRUE, scale=TRUE, ncomp=kfac, validation="CV")
plot(RMSEP(outPLS))

summary(outPLS)
ve <- explvar(outPLS)

fPLS <- as.matrix(outPLS$scores)
d <- as.Date(rownames(f))
plot(d, fPLS[,1], type="l",col="mediumorchid4", main="PLS",xlab="Time",ylab=" component")
legend("topleft",lty=1,col=c("mediumorchid4"),
       legend=c("First component"),cex = 0.6)
plot(d, fPLS[,1], type="l",col="mediumorchid4", main="PLS",xlab="Time",ylab=" component")
lines(d, fPLS[,2], col="lime green")
lines(d, fPLS[,3], col="orangered")
legend("bottomleft",lty=1,col=c("mediumorchid4","lime green","orangered"),
       legend=c("Comp 1","Comp 2","Comp 3"),cex = 0.5)




plot(d, f[,1], type="l", main="PCA vs PLS", lwd=2)
lines(d, fPLS[,1], col="blue", lwd=2)
legend("topleft",lty=1,col=c("black","blue"),
       legend=c("pca","pls"),cex = 0.6)

library("tseries")
library("pls")
library("forecast")
library("glmnet")

# load your custom functions
source("custom-functions.R")

# Split your target and your X's
Y <- sdm2[,"HD1"]
X <- cbind(sdm2[,2:NCOL(sdm2)], sdm3)

# remove any missing values
xy <- cbind(Y, X)
xy <- na.omit(xy)

# reset
Y <- xy[,1]
X <- xy[,2:NCOL(xy)]

write.csv(cbind(Y,X), "YX.csv")

# Set your desired out-of-sample size
Nout <- 60  # out-of-sample for the forecasting exercise
Nout2 <- 12# when do you want to start evaluating the forecast error of models
# to be used in the forecast combination

# Set your forecasting horizon
h <- 12
# Do you want recursive or rolling estimation?
rest <- 1  # 1: recursive, 2: rolling

# In case you decide to use rolling window estimation (and not a recursive one)
wf <- 60     # rolling window length
##########################################################################
##########################################################################
N <- NROW(Y) # total sample size

# matrix to store the results
nmodels <- c("AR(1)", "AR(2)", "Naive", "LR_ESI", "LR_AllConf", "Ridge",
             "Lasso", "PCA(1)", "PCA(2)", "PCA(3)", "PLS(1)", "PLS(2)", "PLS(3)")
allf <- matrix(NA, NROW(Y), NROW(nmodels))
colnames(allf) <- nmodels
rownames(allf) <- rownames(X)

nmodels2 <- c("AVG-ALL", "AVG-PCA", "AVG-PLS", "AVG-MAE")
allf2 <- matrix(NA, NROW(Y), NROW(nmodels2))
colnames(allf2) <- nmodels2
rownames(allf2) <- rownames(X)

for(i in (N-Nout-h+1):(N-h))
{
  # present time is "i"
  if(rest==1){
    iin <- 1:i         # Recursive
  }
  if(rest==2){
    iin <- (i-wf+1):i  # Rolling
  }
  
  Yin <- Y[iin]
  Xin <- X[iin,]
  
  # Model 1: AR(1)
  imodel <- 1
  out <- arima(Yin, order=c(1,0,0))
  fout <- forecast(out, h)
  fout <- as.numeric(fout$mean)
  fout <- fout[h]
  allf[i+h,imodel ] <- fout
  
  # Model 2: AR(2)
  imodel <- imodel +1
  out <- arima(Yin, order=c(2,0,0))
  fout <- forecast(out, h)
  fout <- as.numeric(fout$mean)
  fout <- fout[h]
  allf[i+h,imodel] <- fout
  
  # Model 3: Naive
  imodel <- imodel +1
  fout <- Yin[NROW(Yin)]
  allf[i+h,imodel] <- fout
  
  # Prepare the predictive regression
  n <- NROW(Xin)
  yn <- (h+1):n
  xn <- 1:(n-h)
  
  # Model 5: Y(t) = a + bXt-h + e
  # X: SD23, this is ESI and i am using ONLY as an example
  xvar <- "SD20"
  imodel <- imodel +1
  out <- lm(Yin[yn]~Xin[xn,xvar])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%Xin[n,xvar]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 6: Y(t) = a + b1X1t-h + b2X2t-h + ... + b5X5t-h e
  xvar <- c("SD22", "SD23", "SD24", "SD25", "SD26")
  imodel <- imodel+1
  out <- lm(Yin[yn]~Xin[xn,xvar])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%Xin[n,xvar]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 7: Ridge
  imodel <- imodel +1
  Xins <- xstd(Xin)  # manual standardisation
  out <- cv.glmnet(x=Xins[xn,], y=Yin[yn], type.measure="mse",
                   nfolds=NROW(xn), alpha=0,
                   standardize=FALSE)
  lmin <- out$lambda.min
  b <- as.numeric(coef(out, lmin))
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <-  a_hat + b_hat%*%Xins[n,]
  allf[i+h,imodel] <- fout
  
  # Model 8: Lasso
  imodel <- imodel +1
  out <- cv.glmnet(x=Xins[xn,], y=Yin[yn], type.measure="mse",
                   nfolds=NROW(xn), alpha=1,
                   standardize=FALSE)
  lmin <- out$lambda.min
  b <- as.numeric(coef(out, lmin))
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <-  a_hat + b_hat%*%Xins[n,]
  allf[i+h,imodel] <- fout
  # which are based on PCA factors. So, let's first extract those factors!
  outPCA <- pcr(Yin~Xin, center=TRUE, scale=TRUE, ncomp=3)
  f <- as.matrix(outPCA$scores)
  rownames(f) <- rownames(Xin)
  
  # Model 9: Predictive PCA(1) Factor Regression 
  # Y(t) = a + bF(t-1) + e(t)
  imodel <- imodel+1
  ifactor <- 1
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 10: Predictive PCA(2) Factor Regression 
  # Y(t) = a + b1F1(t-1) + b2F2(t-1) + e(t)
  imodel <- imodel+1
  ifactor <- 1:2
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 11: Predictive PCA(3) Factor Regression 
  # Y(t) = a + b1F1(t-1) + b2F2(t-1) + b3F3(t-1) + e(t)
  imodel <- imodel+1
  ifactor <- 1:3
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # In the next lines, we are going to build some forecasting models
  # which are based on PLS factors. So, let's first extract those factors!
  outPLS <- plsr(Yin~Xin, center=TRUE, scale=TRUE, ncomp=3)
  f <- as.matrix(outPLS$scores)
  rownames(f) <- rownames(Xin)
  
  # Model 12: Predictive PLS(1) Factor Regression 
  # Y(t) = a + b1F1(t-1) + e(t)
  imodel <- imodel+1
  ifactor <- 1
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 13: Predictive PLS(2) Factor Regression 
  # Y(t) = a + b1F1(t-1) + b2F2(t-1)  + e(t)
  imodel <- imodel+1
  ifactor <- 1:2
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # Model 14: Predictive PLS(3) Factor Regression 
  # Y(t) = a + b1F1(t-1) + b2F2(t-1) + b3F3(t-1) + e(t)
  imodel <- imodel+1
  ifactor <- 1:3
  out <- lm(Yin[yn]~f[xn,ifactor])
  b <- out$coefficients
  a_hat <- b[1]
  b_hat <- b[2:NROW(b)]
  fout <- a_hat + b_hat%*%f[n,ifactor]
  fout <- as.numeric(fout)
  allf[i+h,imodel] <- fout
  
  # From this point onwards, deal with forecast combinations!
  imodel2 <- 1
  allf2[i+h,imodel2] <- mean(allf[i+h,])
  
  imodel2 <- imodel2+1
  allf2[i+h,imodel2] <- mean(allf[i+h,c("PCA(1)", "PCA(2)", "PCA(3)")])
  
  imodel2 <- imodel2+1
  allf2[i+h,imodel2] <- mean(allf[i+h,c("PLS(1)", "PLS(2)", "PLS(3)")])
  
   # let's now try to do some combination using weights based on the forecast
  # error
  xcheck <- na.omit(allf)
  if(NROW(xcheck)>=Nout2){
    z <- cbind(Y, allf)
    z <- na.omit(z)
    ztrue <- z[,1]
    zall <- z[,2:NCOL(z)]
    zfe <- matrix(ztrue, NROW(ztrue), NCOL(zall)) - zall
    zmae <- colMeans(abs(zfe))
    zmaeinv <- 1/zmae
    zww <- zmaeinv/sum(zmaeinv)
    
    imodel2 <- imodel2+1
    allf2[i+h,imodel2] <- zww%*%allf[i+h,]
   
  }
  
  
  
  cat("Now done", i, "of ", (N-h), "\n")
}

save.image("Forecasting_OutBE.RData")



load("Forecasting_OutBE.RData")

library("forecast")

##########################################################################
## First, calculate and export your statistics
##########################################################################
allfor <- cbind(Y, allf,allf2)
allfor <- na.omit(allfor)
allfor
write.csv(allfor,"allforBE12.csv")
ytrue <- allfor[,1]
par(mfrow=c(2,2))

mfor=allfor[,"AR(1)"]
plot(ytrue,type="l",main="AR(1)")
lines(mfor,col="blue", )

mfor=allfor[,"AR(2)"]
plot(ytrue,type="l",main="AR(2)")
lines(mfor,col="blue")

mfor=allfor[,"Naive"]
plot(ytrue,type="l", main="Naive")
lines(mfor,col="blue")

mfor=allfor[,"LR_Esi"]
plot(ytrue,type="l", main="LR_Esi")
lines(mfor,col="blue")
par(mfrow=c(2,2))
mfor=allfor[,"LR_Allconf"]
plot(ytrue,type="l", main="LR_Allconf")
lines(mfor,col="blue")

mfor=allfor[,"Ridge"]
plot(ytrue,type="l",main="Ridge")
lines(mfor,col="blue", )

mfor=allfor[,"Lasso"]
plot(ytrue,type="l", main="Lasso")
lines(mfor,col="blue")

mfor=allfor[,"PCA(1)"]
plot(ytrue,type="l", main="PCA(1)")
lines(mfor,col="blue")
par(mfrow=c(2,2))
mfor=allfor[,"PCA(2)"]
plot(ytrue,type="l", main="PCA(2)")
lines(mfor,col="blue")

mfor=allfor[,"PCA(3)"]
plot(ytrue,type="l", main="PCA(3)")
lines(mfor,col="blue")

mfor=allfor[,"PLS(1)"]
plot(ytrue,type="l", main="PLS(1)")
lines(mfor,col="blue")

mfor=allfor[,"PLS(2)"]
plot(ytrue,type="l", main="PLS(2)")
lines(mfor,col="blue")
par(mfrow=c(2,2))
mfor=allfor[,"PLS(3)"]
plot(ytrue,type="l", main="PLS(3)")
lines(mfor,col="blue")

mfor=allfor[,"AVG-ALL"]
plot(ytrue,type="l", main="ALL AVERAGE")
lines(mfor,col="blue")

mfor=allfor[,"AVG-PCA"]
plot(ytrue,type="l", main="PCA AVERAGE")
lines(mfor,col="blue")

mfor=allfor[,"AVG-PLS"]
plot(ytrue,type="l", main="PLS AVERAGE")
lines(mfor,col="blue")





# strip the true from the rest


allfor <- allfor[,2:NCOL(allfor)]

# Calculate SSR // side forecasting
ssr <- matrix(sign(ytrue), NROW(ytrue), NCOL(allfor))==sign(allfor)

ssr
ssr <- apply(ssr, 2, as.double)
write.csv(ssr,"ssrBE12.csv")
ssr <- colMeans(ssr)
ssr
write.csv(ssr,"ssrBEall12.csv")
# Create the forecast error
fe <- matrix(ytrue, NROW(ytrue), NCOL(allfor))-allfor
fe
# Statistics // point forecasting

MAE <- colMeans(abs(fe))
MSFE <- colMeans(fe^2)
RMSFE <- sqrt(MSFE)

allst <- cbind(ssr, MAE, MSFE, RMSFE)
write.csv(allst, "F_AllstatBE12.csv")

# Choose your model and make some plots
par(mfrow=c(2,2))
mmodel <- "AR(1)"

mfor <- allfor[,mmodel]
ss <- cbind(ytrue, mfor)
dss <- as.Date(rownames(ss))
ylims <- c(min(ss), max(ss))
plot(dss, ytrue, type="n", ylim=ylims, xlab="Time", ylab="Values", main=mmodel)
grid(col="grey")
lines(dss, ss[,1], col="#377eb8", lwd=2)
lines(dss, ss[,2], col="#e41a1c", lwd=2)

mmodel <- "PLS(1)"

mfor <- allfor[,mmodel]
ss <- cbind(ytrue, mfor)
dss <- as.Date(rownames(ss))
ylims <- c(min(ss), max(ss))
plot(dss, ytrue, type="n", ylim=ylims, xlab="Time", ylab="Values", main=mmodel)
grid(col="grey")
lines(dss, ss[,1], col="#377eb8", lwd=2)
lines(dss, ss[,2], col="#e41a1c", lwd=2)
mmodel <- "PCA(1)"

mfor <- allfor[,mmodel]
ss <- cbind(ytrue, mfor)
dss <- as.Date(rownames(ss))
ylims <- c(min(ss), max(ss))
plot(dss, ytrue, type="n", ylim=ylims, xlab="Time", ylab="Values", main=mmodel)
grid(col="grey")
lines(dss, ss[,1], col="#377eb8", lwd=2)
lines(dss, ss[,2], col="#e41a1c", lwd=2)

mmodel <- "PCA(2)"

mfor <- allfor[,mmodel]
ss <- cbind(ytrue, mfor)
dss <- as.Date(rownames(ss))
ylims <- c(min(ss), max(ss))
plot(dss, ytrue, type="n", ylim=ylims, xlab="Time", ylab="Values", main=mmodel)
grid(col="grey")
lines(dss, ss[,1], col="#377eb8", lwd=2)
lines(dss, ss[,2], col="#e41a1c", lwd=2)

# choose the models you want to compare
imodel1 <- "LR_ESI"
imodel2 <- ""

# MAE
dmMAE <- dm.test(fe[,imodel1], fe[,imodel2],
                 alternative="two.sided", h, power=1)
dmMAE$p.value

# MSFE
dmMSFE <- dm.test(fe[,imodel1], fe[,imodel2],
                  alternative="two.sided", h, power=2)
dmMSFE$p.value

