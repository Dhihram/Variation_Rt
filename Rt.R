library(tidyverse)
library(dplyr)
library(rio)
library(openxlsx)
library(EpiEstim)
library(ggplot2)
library(incidence)
library(tidyr)
library(readxl)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(MASS)


#data thoriq
data <- read_excel("C:/Users/dhihr/Downloads/230608_harian_prov_data.xlsx")
data2 <- melt(data, id = c("Tanggal", "Provinsi")) 
data2 <- data2 %>% 
  rename("Kasus Harian" = "value")
data <- data2 %>% 
  rename("Provinsi" = "variable")
data$Provinsi <- as.character(data$Provinsi)



#provinsi

data <- read_excel("C:/Users/dhihr/Downloads/230611_harian_prov_data.xlsx")

data_olah_Aceh <- filter(data, Provinsi == "Aceh")
Rt_Aceh <- data.frame(I = data_olah_Aceh$`Kasus Harian`, dates = data_olah_Aceh$Tanggal, Provinsi = data_olah_Aceh$Provinsi)

data_olah_Bali <- filter(data, Provinsi == "Bali")
Rt_Bali <- data.frame(I = data_olah_Bali$`Kasus Harian`, dates = data_olah_Bali$Tanggal, Provinsi = data_olah_Bali$Provinsi)

data_olah_Banten <- filter(data, Provinsi == "Banten")
Rt_Banten <- data.frame(I = data_olah_Banten$`Kasus Harian`, dates = data_olah_Banten$Tanggal, Provinsi = data_olah_Banten$Provinsi)

data_olah_Bengkulu <- filter(data, Provinsi == "Bengkulu")
Rt_Bengkulu <- data.frame(I = data_olah_Bengkulu$`Kasus Harian`, dates = data_olah_Bengkulu$Tanggal, Provinsi = data_olah_Bengkulu$Provinsi)

data_olah_DIY <- filter(data, Provinsi == "DI Yogyakarta")
Rt_DIY <- data.frame(I = data_olah_DIY$`Kasus Harian`, dates = data_olah_DIY$Tanggal,  Provinsi = data_olah_DIY$Provinsi)

data_olah_DKI <- filter(data, Provinsi == "DKI Jakarta")
Rt_DKI <- data.frame(I = data_olah_DKI$`Kasus Harian`, dates = data_olah_DKI$Tanggal,  Provinsi = data_olah_DKI$Provinsi)

data_olah_Gorontalo <- filter(data, Provinsi == "Gorontalo")
Rt_Gorontalo <- data.frame(I = data_olah_Gorontalo$`Kasus Harian`, dates = data_olah_Gorontalo$Tanggal,  Provinsi = data_olah_Gorontalo$Provinsi)

data_olah_Jambi <- filter(data, Provinsi == "Jambi")
Rt_Jambi <- data.frame(I = data_olah_Jambi$`Kasus Harian`, dates = data_olah_Jambi$Tanggal,  Provinsi = data_olah_Jambi$Provinsi)

data_olah_Jabar <- filter(data, Provinsi == "Jawa Barat")
Rt_Jabar <- data.frame(I = data_olah_Jabar$`Kasus Harian`, dates = data_olah_Jabar$Tanggal,  Provinsi = data_olah_Jabar$Provinsi)

data_olah_Jateng<- filter(data, Provinsi == "Jawa Tengah")
Rt_Jateng <- data.frame(I = data_olah_Jateng$`Kasus Harian`, dates = data_olah_Jateng$Tanggal,  Provinsi = data_olah_Jateng$Provinsi)

data_olah_Jatim <- filter(data, Provinsi == "Jawa Timur")
Rt_Jatim <- data.frame(I = data_olah_Jatim$`Kasus Harian`, dates = data_olah_Jatim$Tanggal,  Provinsi = data_olah_Jatim$Provinsi)

data_olah_Kalbar <- filter(data, Provinsi == "Kalimantan Barat")
Rt_Kalbar <- data.frame(I = data_olah_Kalbar$`Kasus Harian`, dates = data_olah_Kalbar$Tanggal,  Provinsi = data_olah_Kalbar$Provinsi)

data_olah_Kalsel <- filter(data, Provinsi == "Kalimantan Selatan")
Rt_Kalsel <- data.frame(I = data_olah_Kalsel$`Kasus Harian`, dates = data_olah_Kalsel$Tanggal,  Provinsi = data_olah_Kalsel$Provinsi)

data_olah_Kalteng <- filter(data, Provinsi == "Kalimantan Tengah")
Rt_Kalteng <- data.frame(I = data_olah_Kalteng$`Kasus Harian`, dates = data_olah_Kalteng$Tanggal,  Provinsi = data_olah_Kalteng$Provinsi)

data_olah_Kaltim <- filter(data, Provinsi == "Kalimantan Timur")
Rt_Kaltim <- data.frame(I = data_olah_Kaltim$`Kasus Harian`, dates = data_olah_Kaltim$Tanggal,  Provinsi = data_olah_Kaltim$Provinsi)

data_olah_Kaltara <- filter(data, Provinsi == "Kalimantan Utara")
Rt_Kaltara <- data.frame(I = data_olah_Kaltara$`Kasus Harian`, dates = data_olah_Kaltara$Tanggal,  Provinsi = data_olah_Kaltara$Provinsi)

data_olah_Babel <- filter(data, Provinsi == "Kepulauan Bangka Belitung")
Rt_Babel <- data.frame(I = data_olah_Babel$`Kasus Harian`, dates = data_olah_Babel$Tanggal,  Provinsi = data_olah_Babel$Provinsi)

data_olah_Kepri <- filter(data, Provinsi == "Kepulauan Riau")
Rt_Kepri <- data.frame(I = data_olah_Kepri$`Kasus Harian`, dates = data_olah_Kepri$Tanggal,  Provinsi = data_olah_Kepri$Provinsi)

data_olah_Lampung <- filter(data, Provinsi == "Lampung")
Rt_Lampung <- data.frame(I = data_olah_Lampung$`Kasus Harian`, dates = data_olah_Lampung$Tanggal,  Provinsi = data_olah_Lampung$Provinsi)

data_olah_Maluku <- filter(data, Provinsi == "Maluku")
Rt_Maluku <- data.frame(I = data_olah_Maluku$`Kasus Harian`, dates = data_olah_Maluku$Tanggal,  Provinsi = data_olah_Maluku$Provinsi)

data_olah_Malut <- filter(data, Provinsi == "Maluku Utara")
Rt_Malut <- data.frame(I = data_olah_Malut$`Kasus Harian`, dates = data_olah_Malut$Tanggal,  Provinsi = data_olah_Malut$Provinsi)

data_olah_NTB <- filter(data, Provinsi == "Nusa Tenggara Barat")
Rt_NTB <- data.frame(I = data_olah_NTB$`Kasus Harian`, dates = data_olah_NTB$Tanggal,  Provinsi = data_olah_NTB$Provinsi)

data_olah_NTT <- filter(data, Provinsi == "Nusa Tenggara Timur")
Rt_NTT <- data.frame(I = data_olah_NTT$`Kasus Harian`, dates = data_olah_NTT$Tanggal,  Provinsi = data_olah_NTT$Provinsi)

data_olah_Papua <- filter(data, Provinsi == "Papua")
Rt_Papua <- data.frame(I = data_olah_Papua$`Kasus Harian`, dates = data_olah_Papua$Tanggal,  Provinsi = data_olah_Papua$Provinsi)

data_olah_Pabar <- filter(data, Provinsi == "Papua Barat")
Rt_Pabar <- data.frame(I = data_olah_Pabar$`Kasus Harian`, dates = data_olah_Pabar$Tanggal,  Provinsi = data_olah_Pabar$Provinsi)

data_olah_Riau <- filter(data, Provinsi == "Riau")
Rt_Riau <- data.frame(I = data_olah_Riau$`Kasus Harian`, dates = data_olah_Riau$Tanggal,  Provinsi = data_olah_Riau$Provinsi)

data_olah_Sulbar <- filter(data, Provinsi == "Sulawesi Barat")
Rt_Sulbar <- data.frame(I = data_olah_Sulbar$`Kasus Harian`, dates = data_olah_Sulbar$Tanggal,  Provinsi = data_olah_Sulbar$Provinsi)

data_olah_Sulsel <- filter(data, Provinsi == "Sulawesi Selatan")
Rt_Sulsel <- data.frame(I = data_olah_Sulsel$`Kasus Harian`, dates = data_olah_Sulsel$Tanggal,  Provinsi = data_olah_Sulsel$Provinsi)

data_olah_Sulteng <- filter(data, Provinsi == "Sulawesi Tengah")
Rt_Sulteng <- data.frame(I = data_olah_Sulteng$`Kasus Harian`, dates = data_olah_Sulteng$Tanggal,  Provinsi = data_olah_Sulteng$Provinsi)

data_olah_Sultra <- filter(data, Provinsi == "Sulawesi Tenggara")
Rt_Sultra <- data.frame(I = data_olah_Sultra$`Kasus Harian`, dates = data_olah_Sultra$Tanggal,  Provinsi = data_olah_Sultra$Provinsi)

data_olah_Sulut <- filter(data, Provinsi == "Sulawesi Utara")
Rt_Sulut <- data.frame(I = data_olah_Sulut$`Kasus Harian`, dates = data_olah_Sulut$Tanggal,  Provinsi = data_olah_Sulut$Provinsi)

data_olah_Sumbar <- filter(data, Provinsi == "Sumatera Barat")
Rt_Sumbar <- data.frame(I = data_olah_Sumbar$`Kasus Harian`, dates = data_olah_Sumbar$Tanggal,  Provinsi = data_olah_Sumbar$Provinsi)

data_olah_Sumsel <- filter(data, Provinsi == "Sumatera Selatan")
Rt_Sumsel <- data.frame(I = data_olah_Sumsel$`Kasus Harian`, dates = data_olah_Sumsel$Tanggal,  Provinsi = data_olah_Sumsel$Provinsi)

data_olah_Sumut <- filter(data, Provinsi == "Sumatera Utara")
Rt_Sumut <- data.frame(I = data_olah_Sumut$`Kasus Harian`, dates = data_olah_Sumut$Tanggal,  Provinsi = data_olah_Sumut$Provinsi)

#nasional
data_nas <- read_excel("C:/Users/dhihr/Downloads/230618_nasional_jb_njb.xlsx")
data_olah_nasional <- filter(data_nas, Provinsi == "Indonesia")
Rt_Indonesia <- data.frame(I = data_olah_nasional$`Kasus Harian`, dates = data_olah_nasional$Tanggal, Provinsi = data_olah_nasional$Provinsi)
Rt_Indonesia <- as.Date(Rt_Indonesia$dates)

data_olah_nasional <- data %>%
  group_by(Tanggal) %>%
  summarise(`Kasus Harian` = sum(`Kasus Harian`))
data_olah_nasional$Provinsi = "Indonesia"

#epiestim

#aceh
Rt_Aceh$dates <- as.Date(Rt_Aceh$dates, format='%Y-%m-%d')
kasus_tanggal_Aceh <- Rt_Aceh[-c(1:7),]
str(Rt_Aceh)
res_parametric_si_Aceh <- estimate_R(Rt_Aceh, 
                                     method="parametric_si",
                                     config = make_config(list(
                                       mean_si = 3, 
                                       std_si = 2.5))
)
R_epiestim_Aceh <- data.frame(tanggal = kasus_tanggal_Aceh$dates, median_r = round(res_parametric_si_Aceh$R$`Median(R)`,2), 
                              Provinsi = kasus_tanggal_Aceh$Provinsi)
#bali
Rt_Bali$dates <- as.Date(Rt_Bali$dates, format='%Y-%m-%d')
kasus_tanggal_Bali <- Rt_Bali[-c(1:7),]
str(Rt_Bali)
res_parametric_si_Bali <- estimate_R(Rt_Bali, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 3, 
                                  std_si = 2.5))
)
R_epiestim_Bali <- data.frame(tanggal = kasus_tanggal_Bali$dates, median_r = round(res_parametric_si_Bali$R$`Median(R)`,2), 
                              Provinsi = kasus_tanggal_Bali$Provinsi)

#banten
Rt_Banten$dates <- as.Date(Rt_Banten$dates, format='%Y-%m-%d')
kasus_tanggal_Banten <- Rt_Banten[-c(1:7),]
str(Rt_Banten)
res_parametric_si_Banten <- estimate_R(Rt_Banten, 
                                     method="parametric_si",
                                     config = make_config(list(
                                       mean_si = 3, 
                                       std_si = 2.5))
)
R_epiestim_Banten <- data.frame(tanggal = kasus_tanggal_Banten$dates, median_r = round(res_parametric_si_Banten$R$`Median(R)`,2), 
                                Provinsi = kasus_tanggal_Banten$Provinsi)
#bengkulu
Rt_Bengkulu$dates <- as.Date(Rt_Bengkulu$dates, format='%Y-%m-%d')
kasus_tanggal_Bengkulu <- Rt_Banten[-c(1:7),]
str(Rt_Bengkulu)
res_parametric_si_Bengkulu <- estimate_R(Rt_Bengkulu, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Bengkulu <- data.frame(tanggal = kasus_tanggal_Bengkulu$dates, median_r = round(res_parametric_si_Bengkulu$R$`Median(R)`,2), 
                                Provinsi = kasus_tanggal_Bengkulu$Provinsi)
#DIY
Rt_DIY$dates <- as.Date(Rt_DIY$dates, format='%Y-%m-%d')
kasus_tanggal_DIY <- Rt_DIY[-c(1:7),]
str(Rt_DIY)
res_parametric_si_DIY <- estimate_R(Rt_DIY, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_DIY <- data.frame(tanggal = kasus_tanggal_DIY$dates, median_r = round(res_parametric_si_DIY$R$`Median(R)`,2), 
                             Provinsi = kasus_tanggal_DIY$Provinsi)

#DKI
Rt_DKI$dates <- as.Date(Rt_DKI$dates, format='%Y-%m-%d')
kasus_tanggal_DKI <- Rt_DKI[-c(1:7),]
str(Rt_DKI)
res_parametric_si_DKI <- estimate_R(Rt_DKI, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3, 
                                      std_si = 2.5))
)
plot(res_parametric_si_DKI, legend = FALSE)
R_epiestim_DKI <- data.frame(tanggal = kasus_tanggal_DKI$dates, median_r = round(res_parametric_si_DKI$R$`Median(R)`,2),
                             Provinsi = kasus_tanggal_DKI$Provinsi)

#Gorontalo
Rt_Gorontalo$dates <- as.Date(Rt_Gorontalo$dates, format='%Y-%m-%d')
kasus_tanggal_Gorontalo <- Rt_Gorontalo[-c(1:7),]
str(Rt_Gorontalo)
res_parametric_si_Gorontalo <- estimate_R(Rt_Gorontalo, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3, 
                                      std_si = 2.5))
)
R_epiestim_Gorontalo <- data.frame(tanggal = kasus_tanggal_Gorontalo$dates, median_r = round(res_parametric_si_Gorontalo$R$`Median(R)`,2),
                             Provinsi = kasus_tanggal_Gorontalo$Provinsi)

#jambi
Rt_Jambi$dates <- as.Date(Rt_Jambi$dates, format='%Y-%m-%d')
kasus_tanggal_Jambi <- Rt_Jambi[-c(1:7),]
str(Rt_Jambi)
res_parametric_si_Jambi <- estimate_R(Rt_Jambi, 
                                          method="parametric_si",
                                          config = make_config(list(
                                            mean_si = 3, 
                                            std_si = 2.5))
)
R_epiestim_Jambi <- data.frame(tanggal = kasus_tanggal_Jambi$dates, median_r = round(res_parametric_si_Jambi$R$`Median(R)`,2),
                                   Provinsi = kasus_tanggal_Jambi$Provinsi)



#jabar
Rt_Jabar$dates <- as.Date(Rt_Jabar$dates, format='%Y-%m-%d')
kasus_tanggal_Jabar <- Rt_Jabar[-c(1:7),]
str(Rt_Jabar)
res_parametric_si_Jabar <- estimate_R(Rt_Jabar, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3, 
                                      std_si = 2.5))
)
R_epiestim_Jabar <- data.frame(tanggal = kasus_tanggal_Jabar$dates, median_r = round(res_parametric_si_Jabar$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Jabar$Provinsi)

#jateng
Rt_Jateng$dates <- as.Date(Rt_Jateng$dates, format='%Y-%m-%d')
kasus_tanggal_Jateng <- Rt_Jateng[-c(1:7),]
str(Rt_Jateng)
res_parametric_si_Jateng <- estimate_R(Rt_Jateng, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Jateng <- data.frame(tanggal = kasus_tanggal_Jateng$dates, median_r = round(res_parametric_si_Jateng$R$`Median(R)`,2), 
                                Provinsi = kasus_tanggal_Jateng$Provinsi)

#jatim
Rt_Jatim$dates <- as.Date(Rt_Jatim$dates, format='%Y-%m-%d')
kasus_tanggal_Jatim <- Rt_Jatim[-c(1:7),]
str(Rt_Jatim)
res_parametric_si_Jatim <- estimate_R(Rt_Jatim, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Jatim <- data.frame(tanggal = kasus_tanggal_Jatim$dates, median_r = round(res_parametric_si_Jatim$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Jatim$Provinsi)

#kalbar
Rt_Kalbar$dates <- as.Date(Rt_Kalbar$dates, format='%Y-%m-%d')
kasus_tanggal_Kalbar <- Rt_Kalbar[-c(1:7),]
str(Rt_Kalbar)
res_parametric_si_Kalbar <- estimate_R(Rt_Kalbar, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Kalbar <- data.frame(tanggal = kasus_tanggal_Kalbar$dates, median_r = round(res_parametric_si_Kalbar$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Kalbar$Provinsi)

#kalsel
Rt_Kalsel$dates <- as.Date(Rt_Kalsel$dates, format='%Y-%m-%d')
kasus_tanggal_Kalsel <- Rt_Kalsel[-c(1:7),]
str(Rt_Kalsel)
res_parametric_si_Kalsel <- estimate_R(Rt_Kalsel, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Kalsel <- data.frame(tanggal = kasus_tanggal_Kalsel$dates, median_r = round(res_parametric_si_Kalsel$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Kalsel$Provinsi)
#kalteng
Rt_Kalteng$dates <- as.Date(Rt_Kalteng$dates, format='%Y-%m-%d')
kasus_tanggal_Kalteng <- Rt_Kalsel[-c(1:7),]
str(Rt_Kalteng)
res_parametric_si_Kalteng <- estimate_R(Rt_Kalteng, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Kalteng <- data.frame(tanggal = kasus_tanggal_Kalteng$dates, median_r = round(res_parametric_si_Kalteng$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Kalteng$Provinsi)

#kaltim
Rt_Kaltim$dates <- as.Date(Rt_Kaltim$dates, format='%Y-%m-%d')
kasus_tanggal_Kaltim <- Rt_Kaltim[-c(1:7),]
str(Rt_Kaltim)
res_parametric_si_Kaltim <- estimate_R(Rt_Kaltim, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Kaltim <- data.frame(tanggal = kasus_tanggal_Kaltim$dates, median_r = round(res_parametric_si_Kaltim$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Kaltim$Provinsi)


#kaltara
Rt_Kaltara$dates <- as.Date(Rt_Kaltara$dates, format='%Y-%m-%d')
kasus_tanggal_Kaltara <- Rt_Kaltara[-c(1:7),]
str(Rt_Kaltara)
res_parametric_si_Kaltara <- estimate_R(Rt_Kaltara, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Kaltara <- data.frame(tanggal = kasus_tanggal_Kaltara$dates, median_r = round(res_parametric_si_Kaltara$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Kaltara$Provinsi)

#babel
Rt_Babel$dates <- as.Date(Rt_Babel$dates, format='%Y-%m-%d')
kasus_tanggal_Babel <- Rt_Babel[-c(1:7),]
str(Rt_Babel)
res_parametric_si_Babel <- estimate_R(Rt_Babel, 
                                        method="parametric_si",
                                        config = make_config(list(
                                          mean_si = 3, 
                                          std_si = 2.5))
)
R_epiestim_Babel <- data.frame(tanggal = kasus_tanggal_Babel$dates, median_r = round(res_parametric_si_Babel$R$`Median(R)`,2),
                                 Provinsi = kasus_tanggal_Babel$Provinsi)

#kepri
Rt_Kepri$dates <- as.Date(Rt_Kepri$dates, format='%Y-%m-%d')
kasus_tanggal_Kepri <- Rt_Kepri[-c(1:7),]
str(Rt_Kepri)
res_parametric_si_Kepri <- estimate_R(Rt_Kepri, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Kepri <- data.frame(tanggal = kasus_tanggal_Kepri$dates, median_r = round(res_parametric_si_Kepri$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Kepri$Provinsi)

#Lampung
Rt_Lampung$dates <- as.Date(Rt_Lampung$dates, format='%Y-%m-%d')
kasus_tanggal_Lampung <- Rt_Lampung[-c(1:7),]
str(Rt_Lampung)
res_parametric_si_Lampung <- estimate_R(Rt_Lampung, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Lampung <- data.frame(tanggal = kasus_tanggal_Lampung$dates, median_r = round(res_parametric_si_Lampung$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Lampung$Provinsi)

#Maluku
Rt_Maluku$dates <- as.Date(Rt_Maluku$dates, format='%Y-%m-%d')
kasus_tanggal_Maluku <- Rt_Maluku[-c(1:7),]
str(Rt_Maluku)
res_parametric_si_Maluku <- estimate_R(Rt_Maluku, 
                                        method="parametric_si",
                                        config = make_config(list(
                                          mean_si = 3, 
                                          std_si = 2.5))
)
R_epiestim_Maluku <- data.frame(tanggal = kasus_tanggal_Maluku$dates, median_r = round(res_parametric_si_Maluku$R$`Median(R)`,2),
                                 Provinsi = kasus_tanggal_Maluku$Provinsi)

#Malut
Rt_Malut$dates <- as.Date(Rt_Malut$dates, format='%Y-%m-%d')
kasus_tanggal_Malut <- Rt_Malut[-c(1:7),]
str(Rt_Malut)
res_parametric_si_Malut <- estimate_R(Rt_Malut, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Malut <- data.frame(tanggal = kasus_tanggal_Malut$dates, median_r = round(res_parametric_si_Malut$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Malut$Provinsi)

#NTB
Rt_NTB$dates <- as.Date(Rt_NTB$dates, format='%Y-%m-%d')
kasus_tanggal_NTB <- Rt_NTB[-c(1:7),]
str(Rt_NTB)
res_parametric_si_NTB <- estimate_R(Rt_NTB, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_NTB <- data.frame(tanggal = kasus_tanggal_NTB$dates, median_r = round(res_parametric_si_NTB$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_NTB$Provinsi)

#NTT
Rt_NTT$dates <- as.Date(Rt_NTT$dates, format='%Y-%m-%d')
kasus_tanggal_NTT <- Rt_NTT[-c(1:7),]
str(Rt_NTT)
res_parametric_si_NTT <- estimate_R(Rt_NTT, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3, 
                                      std_si = 2.5))
)
R_epiestim_NTT <- data.frame(tanggal = kasus_tanggal_NTT$dates, median_r = round(res_parametric_si_NTT$R$`Median(R)`,2),
                             Provinsi = kasus_tanggal_NTT$Provinsi)

#Papua
Rt_Papua$dates <- as.Date(Rt_Papua$dates, format='%Y-%m-%d')
kasus_tanggal_Papua <- Rt_Papua[-c(1:7),]
str(Rt_Papua)
sum(is.na(Rt_Papua))
Rt_Papua <- Rt_Papua$I %>% replace_na(0)
res_parametric_si_Papua <- estimate_R(Rt_Papua, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 3, 
                                      std_si = 2.5))
)
R_epiestim_Papua <- data.frame(tanggal = kasus_tanggal_Papua$dates, median_r = round(res_parametric_si_Papua$R$`Median(R)`,2),
                             Provinsi = kasus_tanggal_Papua$Provinsi)

#Papua Barat
Rt_Pabar$dates <- as.Date(Rt_Pabar$dates, format='%Y-%m-%d')
kasus_tanggal_Pabar <- Rt_Pabar[-c(1:7),]
str(Rt_Pabar)
sum(is.na(Rt_Pabar))
Rt_Pabar <- Rt_Pabar$I %>% replace_na(0)
res_parametric_si_Pabar <- estimate_R(Rt_Pabar, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Pabar <- data.frame(tanggal = kasus_tanggal_Pabar$dates, median_r = round(res_parametric_si_Pabar$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Pabar$Provinsi)

#Riau
Rt_Riau$dates <- as.Date(Rt_Riau$dates, format='%Y-%m-%d')
kasus_tanggal_Riau <- Rt_Riau[-c(1:7),]
str(Rt_Riau)
sum(is.na(Rt_Riau))
Rt_Riau <- Rt_Riau$I %>% replace_na(0)
res_parametric_si_Riau <- estimate_R(Rt_Riau, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Riau <- data.frame(tanggal = kasus_tanggal_Riau$dates, median_r = round(res_parametric_si_Riau$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Riau$Provinsi)

#Sulbar
Rt_Sulbar$dates <- as.Date(Rt_Sulbar$dates, format='%Y-%m-%d')
kasus_tanggal_Sulbar <- Rt_Sulbar[-c(1:7),]
str(Rt_Sulbar)
sum(is.na(Rt_Sulbar))
Rt_Sulbar <- Rt_Sulbar$I %>% replace_na(0)
res_parametric_si_Sulbar <- estimate_R(Rt_Sulbar, 
                                     method="parametric_si",
                                     config = make_config(list(
                                       mean_si = 3, 
                                       std_si = 2.5))
)
R_epiestim_Sulbar <- data.frame(tanggal = kasus_tanggal_Sulbar$dates, median_r = round(res_parametric_si_Sulbar$R$`Median(R)`,2),
                              Provinsi = kasus_tanggal_Sulbar$Provinsi)

#Sulsel
Rt_Sulsel$dates <- as.Date(Rt_Sulsel$dates, format='%Y-%m-%d')
kasus_tanggal_Sulsel <- Rt_Sulsel[-c(1:7),]
str(Rt_Sulsel)
sum(is.na(Rt_Sulsel))
Rt_Sulsel <- Rt_Sulsel$I %>% replace_na(0)
res_parametric_si_Sulsel <- estimate_R(Rt_Sulsel, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Sulsel <- data.frame(tanggal = kasus_tanggal_Sulsel$dates, median_r = round(res_parametric_si_Sulsel$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Sulsel$Provinsi)

#Sulteng
Rt_Sulteng$dates <- as.Date(Rt_Sulteng$dates, format='%Y-%m-%d')
kasus_tanggal_Sulteng <- Rt_Sulteng[-c(1:7),]
str(Rt_Sulteng)
sum(is.na(Rt_Sulteng))
Rt_Sulsel <- Rt_Sulteng$I %>% replace_na(0)
res_parametric_si_Sulteng <- estimate_R(Rt_Sulteng, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Sulteng <- data.frame(tanggal = kasus_tanggal_Sulteng$dates, median_r = round(res_parametric_si_Sulteng$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Sulteng$Provinsi)

#Sultra
Rt_Sultra$dates <- as.Date(Rt_Sultra$dates, format='%Y-%m-%d')
kasus_tanggal_Sultra <- Rt_Sultra[-c(1:7),]
str(Rt_Sultra)
sum(is.na(Rt_Sultra))
Rt_Sultra <- Rt_Sultra$I %>% replace_na(0)
res_parametric_si_Sultra <- estimate_R(Rt_Sultra, 
                                        method="parametric_si",
                                        config = make_config(list(
                                          mean_si = 3, 
                                          std_si = 2.5))
)
R_epiestim_Sultra <- data.frame(tanggal = kasus_tanggal_Sultra$dates, median_r = round(res_parametric_si_Sultra$R$`Median(R)`,2),
                                 Provinsi = kasus_tanggal_Sultra$Provinsi)

#Sulut
Rt_Sulut$dates <- as.Date(Rt_Sulut$dates, format='%Y-%m-%d')
kasus_tanggal_Sulut <- Rt_Sulut[-c(1:7),]
str(Rt_Sulut)
sum(is.na(Rt_Sulut))
Rt_Sulut <- Rt_Sulut$I %>% replace_na(0)
res_parametric_si_Sulut <- estimate_R(Rt_Sulut, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Sulut <- data.frame(tanggal = kasus_tanggal_Sulut$dates, median_r = round(res_parametric_si_Sulut$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Sulut$Provinsi)

#Sumbar
Rt_Sumbar$dates <- as.Date(Rt_Sumbar$dates, format='%Y-%m-%d')
kasus_tanggal_Sumbar <- Rt_Sumbar[-c(1:7),]
str(Rt_Sumbar)
sum(is.na(Rt_Sumbar))
Rt_Sumbar <- Rt_Sumbar$I %>% replace_na(0)
res_parametric_si_Sumbar <- estimate_R(Rt_Sumbar, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3, 
                                        std_si = 2.5))
)
R_epiestim_Sumbar <- data.frame(tanggal = kasus_tanggal_Sumbar$dates, median_r = round(res_parametric_si_Sumbar$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Sumbar$Provinsi)

#Sumsel
Rt_Sumsel$dates <- as.Date(Rt_Sumsel$dates, format='%Y-%m-%d')
kasus_tanggal_Sumsel <- Rt_Sumsel[-c(1:7),]
str(Rt_Sumsel)
sum(is.na(Rt_Sumsel))
Rt_Sumsel <- Rt_Sumsel$I %>% replace_na(0)
res_parametric_si_Sumsel <- estimate_R(Rt_Sumsel, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Sumsel <- data.frame(tanggal = kasus_tanggal_Sumsel$dates, median_r = round(res_parametric_si_Sumsel$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Sumsel$Provinsi)

#sumut
Rt_Sumut$dates <- as.Date(Rt_Sumut$dates, format='%Y-%m-%d')
kasus_tanggal_Sumut <- Rt_Sumut[-c(1:7),]
str(Rt_Sumut)
sum(is.na(Rt_Sumut))
Rt_Sumsel <- Rt_Sumut$I %>% replace_na(0)
res_parametric_si_Sumut <- estimate_R(Rt_Sumut, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 3, 
                                         std_si = 2.5))
)
R_epiestim_Sumut <- data.frame(tanggal = kasus_tanggal_Sumut$dates, median_r = round(res_parametric_si_Sumut$R$`Median(R)`,2),
                                Provinsi = kasus_tanggal_Sumut$Provinsi)
#nasional
Rt_Indonesia$dates <- as.Date(Rt_Indonesia$dates, format='%Y-%m-%d')
kasus_tanggal_Indonesia <- Rt_Indonesia[-c(1:7),]
str(Rt_Indonesia)
Rt_Indonesia <- Rt_Indonesia$I %>% replace_na(0)

res_parametric_si_Indonesia <- estimate_R(Rt_Indonesia, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 3.5, 
                                        std_si = 2.8))
)
R_epiestim_Indonesia <- data.frame(tanggal = kasus_tanggal_Indonesia$dates, median_r = round(res_parametric_si_Indonesia$R$`Median(R)`,2),
                               Provinsi = kasus_tanggal_Indonesia$Provinsi)
#R_epiestim_Indonesia <- data.frame(tanggal = kasus_tanggal_Indonesia$dates, mean_r = res_parametric_si_Indonesia$R$`Mean(R)`,
                                   #Provinsi = kasus_tanggal_Indonesia$Provinsi)
#gabung
R_epiestim_total <- rbind(R_epiestim_Indonesia, R_epiestim_Aceh, R_epiestim_Bali, R_epiestim_Banten, R_epiestim_Bengkulu, R_epiestim_DIY, R_epiestim_DKI, R_epiestim_Gorontalo, R_epiestim_Jambi, R_epiestim_Jabar, R_epiestim_Jateng, R_epiestim_Jatim,
                          R_epiestim_Kalbar, R_epiestim_Kalsel, R_epiestim_Kalteng, R_epiestim_Kaltim, R_epiestim_Kaltara, R_epiestim_Babel, R_epiestim_Kepri, R_epiestim_Lampung, R_epiestim_Maluku, R_epiestim_Malut, R_epiestim_NTB, R_epiestim_NTT,
                          R_epiestim_Papua, R_epiestim_Pabar, R_epiestim_Riau, R_epiestim_Sulbar, R_epiestim_Sulsel, R_epiestim_Sulteng, R_epiestim_Sultra, R_epiestim_Sulut, R_epiestim_Sumbar, R_epiestim_Sumsel, R_epiestim_Sumut)

R_epiestim_akhir <- R_epiestim_total %>% group_by(Provinsi) %>% summarize(Rt_akhir = last(median_r)) %>% as.data.frame()
R_epiestim_akhir$kategori <- ifelse(R_epiestim_akhir$Rt_akhir > 1, "Tidak Terkendali", "Terkendali")
colnames(R_epiestim_total)[2]  <- "Rt"
R_epiestim_dashboard <- left_join(R_epiestim_total, R_epiestim_akhir, by = 'Provinsi')
write.xlsx(R_epiestim_dashboard, "C:/Users/dhihr/Documents/R_total.xlsx")
write.csv(R_epiestim_dashboard, "C:/Users/dhihr/Documents/R_total.csv")

#mingguan nasional
minggu <- R_epiestim_total
minggu <- minggu %>% filter(Provinsi == 'Indonesia')
minggu$awal_minggu <- floor_date(minggu$tanggal, unit = "week", week_start = getOption("lubridate.week.start", 7))
agregat_minggu <- minggu %>% group_by(awal_minggu) %>% summarize(mean = mean(median_r), median = median(median_r)) %>% as.data.frame()
agregat_minggu$akhir_minggu <- ceiling_date(agregat_minggu$awal_minggu, unit = "week", week_start = getOption("lubridate.week.end", 6))
agregat_minggu$mean <- round(agregat_minggu$mean,2)
write.xlsx(agregat_minggu, "C:/Users/dhihr/Documents/agregat_minggu.xlsx")



#coba epinow
#nasional
data_nas <- read_excel("C:/Users/dhihr/Downloads/230618_nasional_jb_njb.xlsx")
data_olah_nasional <- filter(data_nas, Provinsi == "Indonesia")
Rt_Indonesia <- data.frame(I = data_olah_nasional$`Kasus Harian`, dates = data_olah_nasional$Tanggal, Provinsi = data_olah_nasional$Provinsi)

#nasional
Rt_Indonesia$dates <- as.Date(Rt_Indonesia$dates, format='%Y-%m-%d')
kasus_tanggal_Indonesia <- Rt_Indonesia[-c(1:7),]
#str(Rt_Indonesia)
#res_parametric_si_Indonesia <- estimate_R(Rt_Indonesia, 
                                          #method="parametric_si",
                                          #config = make_config(list(
                                            #mean_si = 3, 
                                            #std_si = 2))
#)
#plot(res_parametric_si_Indonesia, legend = FALSE)
#R_epiestim_Indonesia_coba <- data.frame(tanggal = kasus_tanggal_Indonesia$dates, median_r = res_parametric_si_Indonesia$R$`Median(R)`,
                                   #Provinsi = kasus_tanggal_Indonesia$Provinsi)
write.xlsx(reported_cases, "C:/Users/dhihr/Documents/epinow_template.xlsx")



#epinow
#import epinow template
epinow1 <- data.frame(date = data_olah_nasional$Tanggal, confirm = data_olah_nasional$`Kasus Harian`)
epinow_template <- tail(epinow1, n = 100)
tail(epinow_template)

drat:::add("epiforecasts")
install.packages("EpiNow2")
library(EpiNow2)
reported_cases <- epinow_template
reported_cases$date <- as.Date(reported_cases$date, format='%Y-%m-%d')
str(reported_cases)
reporting_delay <- estimate_delay(rlnorm(1000,  log(3), 1),
                                  max_value = 15, bootstraps = 1)
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
estimates <- epinow(reported_cases = reported_cases, 
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4))
knitr::kable(summary(estimates))
head(summary(estimates, type = "parameters", params = "R"))
head(summary(estimates, output = "estimated_reported_cases"))
R_epinow1 <- summary(estimates, type = "parameters", params = "R")
R_epinow2 <- summary(estimates, output = "estimated_reported_cases")
R_epinow3 <- summary(estimates, type = "parameters", params = "infections")
head(R_epinow3)
plot(estimates)
write.xlsx(R_epinow1, "C:/Users/dhihr/Documents/R_epinow_R.xlsx")
write.xlsx(R_epinow2, "C:/Users/dhihr/Documents/R_epinow_report_cases.xlsx")
write.xlsx(R_epinow3, "C:/Users/dhihr/Documents/R_epinow_report_infection.xlsx")


#R0
library(R0)
data_kasus <- Rt_Indonesia$I
names(data_kasus) <- Rt_Indonesia$dates
data_kasus <- tail(data_kasus, 1000)
tail(data_kasus)
mGT <- generation.time("gamma", c(3,2.5))
SB <- est.R0.SB(data_kasus, begin=900, end=1000, mGT)
SB$R
plot(SB)
plotfit(SB)
SB$end
SB$begin
data_bettencourt <- tail(data_kasus,101)
data_bettencourt <- as.data.frame(data_bettencourt)
data_bettencourt$date <- row.names(data_bettencourt)
data_bettencourt <- data_bettencourt %>% filter(row_number() <= n()-1)
data_bettencourt$bettencourt <- SB$R

TD <- est.R0.TD(data_kasus, mGT, begin=900, end=1000, nsim=80)
plot(TD)
J <- data.frame(R0 = TD$R)
J$tanggal <- row.names(J)
tail(J)
J$tanggal <- as.Date(J$tanggal)
write.xlsx(J, "C:/Users/dhihr/Documents/R_teunis.xlsx")

#epilinvert
library(EpiInvert)
Rt_Indonesia$dates <- as.Date(Rt_Indonesia$dates)
EpiInvert <- EpiInvert(Rt_Indonesia$I,
                           "2023-06-25",Rt_Indonesia$dates,
                           select_params(list(mean_si = 3,sd_si=2.5, shift_si=-1)))
EpiInvert_plot(EpiInvert)


#Sebastian Ortiz 
setwd("C:/Users/dhihr/Downloads")
owid_covid_data <- read_csv("owid-covid-data.csv")
owid_covid_data <- owid_covid_data %>% filter(date > '2023-01-01')
indonesia <- filter(owid_covid_data, location == "Indonesia")
Rt_Ortiz <- data.frame(Date = indonesia$date, Ortiz = indonesia$reproduction_rate)



#coba plot
R_epiestim_total$region =ifelse(R_epiestim_total$Provinsi %in% c("Bali", "Banten", "DKI Jakarta","DI Yogyakarta", "Jawa Barat",
                                                       "Jawa Tengah", "Jawa Timur"), yes = "Jawa-Bali", ifelse(R_epiestim_total$Provinsi %in% c("Nusa Tenggara Barat","Nusa Tenggara Timur"), yes = "Nusa Tenggara", 
                                                                                                               ifelse(R_epiestim_total$Provinsi %in% c("Kalimantan Utara","Kalimantan Barat", "Kalimantan Timur", "Kalimantan Selatan", "Kalimantan Tengah"), yes = "Pulau Kalimantan", 
                                                                                                                      ifelse(R_epiestim_total$Provinsi %in% c("Sulawesi Selatan", "Sulawesi Tengah", "Sulawesi Barat", "Sulawesi Utara", "Sulawesi Tenggara", "Gorontalo"), yes = "Pulau Sulawesi",
                                                                                                                             ifelse(R_epiestim_total$Provinsi %in% c("Papua", "Papua Barat", "Maluku", "Maluku Utara"), yes= "Papua-Maluku", 
                                                                                                                                    ifelse(R_epiestim_total$Provinsi %in% c("Indonesia"), yes= "Indonesia",no = "Pulau Sumatera"))))))
R_epiestim_total <- R_epiestim_total %>% filter(region != c('Indonesia'))
#R_epiestim_total <- R_epiestim_total %>% filter(region == 'Indonesia')

R_epiestim_total$treshold <- 1
R_epiestim_total <- R_epiestim_total %>% filter(tanggal > '2023-03-01')
indikator_akhir <- R_epiestim_total %>% group_by(Provinsi) %>% summarise(indikator_akhir = last(Rt))
indikator_akhir$label <- ifelse(indikator_akhir$indikator_akhir > 1, "t", "u")
indikator_akhir$label <- factor(indikator_akhir$label, levels=c('t','u'))
R_epiestim_total <- right_join(R_epiestim_total, indikator_akhir, by = 'Provinsi')
R_epiestim_total <- arrange(R_epiestim_total, label)
R_epiestim_total <- R_epiestim_total %>% unite("BothLabels", label, Provinsi, sep = "_", remove = FALSE)

RmType <- function(string) {
  sub("._", "", string)
}
ggplot() +
  geom_area(data = R_epiestim_total, aes(x = tanggal, y = Rt, fill = label), alpha = 0.12) + 
  facet_wrap(~BothLabels, labeller = labeller(BothLabels = RmType), ncol = 7, scales = "free_y") + 
  geom_line(data = R_epiestim_total, aes(x = tanggal, y = Rt, color = label), size = 1.2) +
  geom_line(data = R_epiestim_total, aes(x = tanggal, y = treshold), size = 0.5, color = "black", linetype = "dashed") + theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="2 week", date_labels="%d-%b", limits = as.Date(c("2023-06-23", "2023-07-23")))

ggplot() + 
  facet_wrap(~BothLabels, labeller = labeller(BothLabels = RmType), ncol = 8, scales = "free_y") + 
  geom_line(data = R_epiestim_total, aes(x = tanggal, y = Rt, color = label), size = 1.2) + scale_color_manual(values=c("firebrick", "turquoise4")) +
  geom_line(data = R_epiestim_total, aes(x = tanggal, y = treshold), size = 0.5, color = "black", linetype = "dashed") + theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + ylab("Estimasi Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 month", date_labels="%d-%b", limits = as.Date(c("2023-05-01", "2023-06-8")))

write.xlsx(R_epiestim_Indonesia, "C:/Users/dhihr/Documents/Indonesia2.xlsx")

#gabung metode
R_epinow <- data.frame(Date = as.Date(R_epinow1$date), Epinow = round(R_epinow1$median,2), treshold = 1)
data_ends <- R_epinow %>% filter(Date == "2023-06-24")
R_teunis <- data.frame(Date = as.Date(J$tanggal), Teunis = round(J$R0,2), treshold = 1)
R_teunis <- R_teunis %>% filter(row_number() <= n()-1)
data_ends2 <- R_teunis %>% filter(Date == "2023-06-24")
R_epiestim <-data.frame(Date = as.Date(R_epiestim_Indonesia$tanggal), Epiestim = round(R_epiestim_Indonesia$median_r,2), treshold = 1)
data_ends3 <- R_epiestim %>% filter(Date == "2023-06-24")
R_bettencourt <- data.frame(Date = as.Date(data_bettencourt$date), bettencourt = data_bettencourt$bettencourt, treshold = 1)
data_ends4 <- R_bettencourt %>% filter(Date == "2023-06-24")
Rt_Alvarez <- data.frame(Date = as.Date(EpiInvert$dates), EpiInvert = round(EpiInvert$Rt,2), treshold = 1)
data_ends5 <- Rt_Alvarez %>% filter(Date == "2023-06-24")



#plot
p <- ggplot() + geom_line(data = R_epinow, aes(x = Date, y = Epinow), size = 1.5, color = "turquoise4") +
  geom_line(data = R_epinow, aes(x = Date, y = treshold), size = 0.5, color = "black", linetype = "dashed") +
  theme(plot.title=element_text(size=16), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + labs(title ="Rt Abbott et al") + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 week", date_labels="%d-%b", limits = as.Date(c("2023-05-20", "2023-06-24")))
p<-p + geom_text_repel(
  aes(Date, Epinow, label = Epinow), data = data_ends,
  fontface ="bold", color = "black", size = 4) + theme_minimal()
p
p2 <- ggplot() + geom_line(data = R_teunis, aes(x = Date, y = Teunis), size = 1.5, color = "turquoise4") +
  geom_line(data = R_teunis, aes(x = Date, y = treshold), size = 0.5, color = "black", linetype = "dashed") +
  theme(plot.title=element_text(size=16), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + labs(title ="Rt Wallinga & Teunis") + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 week", date_labels="%d-%b", limits = as.Date(c("2023-05-20", "2023-06-24")))
p2<- p2 + geom_text_repel(
  aes(Date, Teunis, label = Teunis), data = data_ends2,
  fontface ="bold", color = "black", size = 4) + theme_minimal()
p2
p3 <- ggplot() + geom_line(data = R_epiestim, aes(x = Date, y = Epiestim), size = 1.5, color = "turquoise4") +
  geom_line(data = R_epiestim, aes(x = Date, y = treshold), size = 0.5, color = "black", linetype = "dashed") +
  theme(plot.title=element_text(size=16), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + labs(title ="Rt Cori et al") + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 week", date_labels="%d-%b", limits = as.Date(c("2023-05-20", "2023-06-24")))
p3 <- p3 + geom_text_repel(
  aes(Date, Epiestim, label = Epiestim), data = data_ends3,
  fontface ="bold", color = "black", size = 4) + theme_minimal()
p3
p4 <- ggplot() + geom_line(data = R_bettencourt, aes(x = Date, y = bettencourt), size = 1.5, color = "turquoise4") +
  geom_line(data = R_bettencourt, aes(x = Date, y = treshold), size = 0.5, color = "black", linetype = "dashed") +
  theme(plot.title=element_text(size=16), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + labs(title ="Rt Bettencourt & Ribiero") + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 week", date_labels="%d-%b", limits = as.Date(c("2023-05-20", "2023-06-24")))
p4 <- p4 + geom_text_repel(
  aes(Date, bettencourt, label = bettencourt), data = data_ends4,
  fontface ="bold", color = "black", size = 4) + theme_minimal()
p4
p5 <- ggplot() + geom_line(data = Rt_Alvarez, aes(x = Date, y = EpiInvert), size = 1.5, color = "turquoise4") +
  geom_line(data = Rt_Alvarez, aes(x = Date, y = treshold), size = 0.5, color = "black", linetype = "dashed") +
  theme(plot.title=element_text(size=16), panel.grid.minor = element_blank()) + theme(strip.text = element_text(face="bold", size=9)) + labs(title ="Rt Alvarez") + ylab("Rt") + xlab("Tanggal") +
  scale_x_date(date_breaks="1 week", date_labels="%d-%b", limits = as.Date(c("2023-05-20", "2023-06-24")))
p5 <- p5 + geom_text_repel(
  aes(Date, EpiInvert, label = EpiInvert), data = data_ends5,
  fontface ="bold", color = "black", size = 4) + theme_minimal()
p5
grid.arrange(p, p2, p3, p4, p5, ncol=2)


