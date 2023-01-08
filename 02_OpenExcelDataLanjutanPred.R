#!/usr/local/bin/Rscript

library(readxl)
rm(list = ls())
InitTimeX = "2023-01-01"
LastTime = gsub(substr(as.Date(InitTimeX) - 2*28, 1, 7), pattern = "\\-", replacement = "_")
InitTime = gsub(substr(as.Date(InitTimeX) - 1*28, 1, 7), pattern = "\\-", replacement = "_")
SOFT = readxl::read_excel(paste0("../data/BMKGSOFT/Data Hujan Desember DKI dan Banten_",
                                 InitTime, ".xls"))
load(paste0("../data/CH_SPILast/InputSPI_",InitTime,".Rda"))
NamSOFT = names(SOFT[,-1])


NAMESPI = names(InputSPI)
# load("../input/INPUTSPI/HasilSPI.Rda")
# =================== INPUT ================
# load(paste0("../data/Forecast/Ensemble/DFMonAllMean_",InitTime,".Rda")) # Nanti Dibuka
load(paste0("../data/Forecast/Ensemble/DFMonAllMean_2022_11.Rda"))
# DFMonAllMean$Jan2023
# colnames(DFMonAllMean)
# dim(DFMonAllMean)
AddedDataPred = list()
for(i in 1:nrow(DFMonAllMean)){
  NameCOLL = substr(names(DFMonAllMean[1, -1]), 1, 3)
  year = substr(names(DFMonAllMean[1, -1]), 4, 7)
  month = c()
  for(j in 1:length(NameCOLL)){
    month[j] = which(month.abb %in% NameCOLL[j])
  }
  # AddedDataPred[[i]] = data.frame(year, month, 
  #                                 precipitation = as.numeric(as.matrix(DFMonAllMean[i, -1])))
  AddedDataPred[[i]] = data.frame(year, month,
                                  precipitation = as.numeric(as.matrix(DFMonAllMean[i, 3])))
}


StationName = NamSOFT
Sm = list()
for(i in 1:length(StationName)){
  Sm[[i]] = as.character(as.matrix(strsplit(StationName[i], split = " \\(")[[1]]))
  if(length(Sm) < 3){
    Sm[[i]] = c(Sm[[i]], rep(NA, 3-length(Sm)))
  }else if(length(Sm) > 3){
    Sm[[i]] = Sm[[i]][1:3]
  }
}

Station =as.character( as.matrix(do.call(rbind, Sm)[, 1]))
Station[grep(Station, pattern = "\\/")]
Station1 = gsub(Station, pattern = "\\/", replacement = "_GM_")

Station1 = gsub(Station1, pattern = " ", replacement = "_")
Station1 = gsub(Station1, pattern = "\\-", replacement = "_SR_")
Station1 = gsub(Station1, pattern = "\\.", replacement = "")
Station1 = gsub(Station1, pattern = "\\(", replacement = "_TKO_")
Station1 = tolower(gsub(Station1, pattern = "\\)", replacement = "_TKC_"))

Ada = c()
for(i in 1:length(NAMESPI)){
  Ada[i] = grep(tolower(Station1), pattern = NAMESPI[i])
}

Data = SOFT[, -1]
Sltc = as.data.frame(Data[, Ada])
colnames(Sltc) = Station1[Ada]
BackUpdata =  list()

for(i in 1:ncol(Sltc)){
  BackUpdata[[ i]] = as.numeric(Sltc[, i])
  BackUpdata[[ i]][BackUpdata[[ i]] == 9999] = 0
  BackUpdata[[ i]][BackUpdata[[ i]] == 8888] = NA
}
BackUpdataDF = data.frame(do.call(cbind, BackUpdata))
names(BackUpdataDF) = Station1[Ada]

# Sltc[Sltc == 9999] = 0
# Sltc[Sltc == 8888] = NA
InputSPI2 = InputSPI

YMDLast = as.Date(paste0(paste(tail(InputSPI2[[1]], 1)[1,1:2], collapse = "-"), "-01"))
YMDNew = YMDLast+ 32
JumlahData = c()
Hasil1Bulan = list()
Hasil3Bulan = list()
Hasil1BulanLast = c()
Hasil3BulanLast = c()
Tail3Monthly = list()
BackUpdataDF$stasiun_klimatologi_banten
for(i in 1:ncol(BackUpdataDF)){
  # i = 19
  # InputSPI2[[i]] = InputSPI2[[i]][(!is.na(InputSPI2[[i]]$precipitation)), ]
  if(length(which(is.na(BackUpdataDF[, i])))/length(BackUpdataDF[, i]) > 0.6){
    JumlahData[i] = NA
  }else{
    JumlahData[i] = sum(BackUpdataDF[, i], na.rm = T)
    # DFNew = data.frame(year = as.integer(substr(YMDNew, 1, 4)), 
                       # month = as.integer(substr(YMDNew, 6, 7)), 
                       # precipitation = JumlahData[i]
    DFNew = AddedDataPred[[i]]
    
    if(!any(InputSPI2[[i]]$year == DFNew$year[1] & InputSPI2[[i]]$month == DFNew$month[1])){
      InputSPI2[[i]] = rbind(InputSPI2[[i]], DFNew)
    }
    
    Tail3Monthly[[i]] = tail(InputSPI2[[i]], 3)
    
    
    foo = InputSPI2[[i]]$precipitation
    
    myts <- ts(foo, start=c(range(InputSPI2[[i]]$year)[1], 
                            head(InputSPI2[[i]]$month, 1)), 
               end=c(range(InputSPI2[[i]]$year)[2], 
                     tail(InputSPI2[[i]]$month, 1)), frequency=12) 
    # which(is.na(myts))
    # class(foo) <- c('mts', 'ts')
    
    Hasil1Bulan[[i]] = SPEI::spi(myts, scale =  1, na.rm = T)
    Hasil3Bulan[[i]] = SPEI::spi(myts, scale =  3, na.rm = T)
    Hasil1BulanLast[i] = tail(Hasil1Bulan[[i]]$fitted, 1)
    Hasil3BulanLast[i] = tail(Hasil3Bulan[[i]]$fitted, 1)
    
    png(filename = paste0("../output/PlotPred/Plot_Sta_",NAMESPI[i], ".png"), 
        width = 350, height = 500 , units = "px")
    par(mfrow = c(2, 1))
    plot(Hasil1Bulan[[i]], main = paste0("SPI ", NAMESPI[i], " 1 bulanan, \n", 
                                         month.abb[as.integer(substr(YMDNew, 6,7))]," ", 
                                         substr(YMDNew, 1, 4)))
    plot(Hasil3Bulan[[i]], main = paste0("SPI ", NAMESPI[i], " 3 bulanan, \n", 
                                         month.abb[as.integer(substr(YMDNew, 6,7))]," ", 
                                         substr(YMDNew, 1, 4)))
    dev.off()
  }
}

DFHEAD = read_xlsx("../dependencies/SPIMonthlyDF13_DF.xlsx")
Hasil1BulanLastDF = DFHEAD 
Hasil1BulanLastDF[, 6] =  Hasil1BulanLast
Hasil1BulanLastDF[, 7] =  Hasil3BulanLast
Hasil1BulanLastDF[, 4] = substr(YMDNew, 1, 4)
Hasil1BulanLastDF[, 5] = substr(YMDNew, 6, 7)

ColTailName = paste0(month.abb[Tail3Monthly[[1]][, 2]], "_",Tail3Monthly[[1]][, 1])
Tail3Monthlies = list()

for(i in 1:length(Tail3Monthly)){
  Tail3Monthlies[[i]] = Tail3Monthly[[i]][, 3]
}
Tail3MonthliesDF = data.frame(do.call(rbind, Tail3Monthlies))
colnames(Tail3MonthliesDF) = ColTailName
Hasil1BulanLastDF = data.frame(Hasil1BulanLastDF,Tail3MonthliesDF )

# Hasil1BulanLastDF = data.frame(, SPI =Hasil1BulanLast)
write.csv2(Hasil1BulanLastDF, file = paste0("../output/Hasil1BulanPred_", 
                                           substr(YMDNew, 1, 7), ".csv"), row.names = F)
# write.csv(Hasil1BulanLastDF, file = paste0("../output/Hasil3BulanLast_", 
#                                          substr(YMDNew, 1, 7), ".csv"), row.names = F)



