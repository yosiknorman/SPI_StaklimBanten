library(readxl)

InitTimeX = "2022-12-01" (Parsing Otomatis)
LastTime = gsub(substr(as.Date(InitTimeX) - 2*28, 1, 7), pattern = "\\-", replacement = "_")
InitTime = gsub(substr(as.Date(InitTimeX) - 1*28, 1, 7), pattern = "\\-", replacement = "_")
ANLG = read_excel(paste0("../data/Forecast/Analogi/PrakBul_Analogi_Init",
                         gsub(substr(InitTimeX, 1, 7), pattern   = "\\-", replacement = ""), 
                         ".xlsx"))

REGR = read_excel(paste0("../data/Forecast/SSTForecast/PrakBul_Regresi_Init",
                         gsub(InitTime, pattern   = "_", replacement = ""), 
                         ".xlsx"))

ECMF = read_excel(paste0("../data/Forecast/ECMWFToPoint/PrakBul_ECMWF_Init",
                         gsub(substr(InitTimeX, 1, 7), pattern   = "\\-", replacement = ""), 
                         ".xlsx"))

TargetMonth = substr(InitTimeX, 6, 7)
TargetYear = substr(InitTimeX, 1, 4)

iANLGLast =  grep(names(ANLG), pattern = paste0(month.abb[as.integer(TargetMonth)]))[1]
iREGRLast =  grep(names(REGR), pattern = paste0(month.abb[as.integer(TargetMonth)]))[1]
iECMFLast =  grep(names(ECMF), pattern = paste0(month.abb[as.integer(TargetMonth)]))[1]
# names(ECMF)[iECMFLast]


Tabs = read_excel("../dependencies/TableSPIPRAK.xlsx")
DFIdx = data.frame(PosisiSPI = Tabs$PosisiSPI, 
                   Name = Tabs$Referensi,PosisiPrakBUl = Tabs$No)
BySPI = DFIdx[order(DFIdx$PosisiSPI),]
Coba = data.frame(Bul = BySPI$Name, SPI = Tabs$StaSPI)

DFMonPlus1 = data.frame(ANL = as.numeric(as.matrix(ANLG[BySPI$PosisiPrakBUl, iANLGLast:(iANLGLast+0)])), 
           REG = as.numeric(as.matrix(REGR[BySPI$PosisiPrakBUl, iREGRLast:(iREGRLast+0)])), 
           ECM = as.numeric(as.matrix(ECMF[BySPI$PosisiPrakBUl, iECMFLast:(iECMFLast+0)])) 
           )

DFMonPlus2 = data.frame(ANL = as.numeric(as.matrix(ANLG[BySPI$PosisiPrakBUl, (iANLGLast+1)])), 
           REG = as.numeric(as.matrix(REGR[BySPI$PosisiPrakBUl, (iREGRLast+1)])), 
           ECM = as.numeric(as.matrix(ECMF[BySPI$PosisiPrakBUl, (iECMFLast+1)]))
           )
NaMean = function(x){
  mean(x[!is.na(x)])
}


DFMonAllMean = data.frame(Sta = ECMF$Referensi[BySPI$PosisiPrakBUl], MonPlus0 = apply(DFMonPlus1, 1, FUN = NaMean), 
                          MonPlus1 = apply(DFMonPlus2, 1, FUN = NaMean))
names(DFMonAllMean) = c("Sta", names(ECMF)[iECMFLast:(iECMFLast+1)])

save(DFMonAllMean, file = paste0("../data/Forecast/Ensemble/DFMonAllMean_", InitTime, ".Rda" ))


