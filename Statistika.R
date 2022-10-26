#Mengambil data dari csv
databerat <- read.csv2(file = "d:/data_berat.csv", header = TRUE)
databerat

#Menghitung banyak data
banyak_data <- nrow(databerat)
banyak_data

#Mencari nilai terkecil dan terbesar
nilai_terkecil <- min(databerat$berat)
nilai_terbesar <- max(databerat$berat)
nilai_terkecil
nilai_terbesar

#Mencari range
range <- nilai_terbesar-nilai_terkecil
range

#Mencari banyak kelas
k <- 1+(3.3*log(range, base=10))
k <- round(k)
k

#Mencari interval kelas
interval <- range/k
interval <- interval
interval <- round(interval)
interval

#Membuat tabel#
tabel <- data.frame(kelas1 = c(38,39,40,41,42,43,44,45,46,47),
                    kelas2 = c(48,49,50,51,52,53,54,55,56,57),
                    kelas3 = c(58,59,60,61,62,63,64,65,66,67),
                    kelas4 = c(68,69,70,71,72,73,74,75,76,77),
                    kelas5 = c(78,79,80,81,82,83,84,85,86,87),
                    kelas6 = c(88,89,90,91,92,93,94,95,96,97),
                    kelas7 = c(98,99,100,101,102,103,104,105,106,107))
                    
frekuensi1 <- 17
frekuensi2 <- 71
frekuensi3 <- 79
frekuensi4 <- 27
frekuensi5 <- 5
frekuensi6 <- 0
frekuensi7 <- 1

fixi <- c(median(tabel$kelas1)*frekuensi1,
          median(tabel$kelas2)*frekuensi2,
          median(tabel$kelas3)*frekuensi3,
          median(tabel$kelas4)*frekuensi4,
          median(tabel$kelas5)*frekuensi5,
          median(tabel$kelas6)*frekuensi6,
          median(tabel$kelas7)*frekuensi7)

#Mencari mean
rata2 <- sum(fixi)/banyak_data
rata2

Tb <- 58-0.5
Tb

median <- Tb+10*((0.5*banyak_data)-frekuensi2)/frekuensi3
median

#Mencari modus
b1 <- frekuensi3-frekuensi2
b2 <- frekuensi3-frekuensi4

modus <- Tb+10*(b1/(b1+b2))
modus

#Mencari range
#Cara 1

median_kelas1 <- median(tabel$kelas1)
median_kelas7 <- median(tabel$kelas7)

Range <- median_kelas7 - median_kelas1
Range

#Simpangan rata-rata
xi <- data.frame(xi1 = median(tabel$kelas1),
                 xi2 = median(tabel$kelas2),
                 xi3 = median(tabel$kelas3),
                 xi4 = median(tabel$kelas4),
                 xi5 = median(tabel$kelas5),
                 xi6 = median(tabel$kelas6),
                 xi7 = median(tabel$kelas7))

xi_xbar <- data.frame(a1 = xi$xi1-rata2,
                      a2 = xi$xi2-rata2,
                      a3 = xi$xi3-rata2,
                      a4 = xi$xi4-rata2,
                      a5 = xi$xi5-rata2,
                      a6 = xi$xi6-rata2,
                      a7 = xi$xi7-rata2)

fi_xi_xbar <- data.frame(b1 = frekuensi1*abs(xi_xbar$a1),
                         b2 = frekuensi2*abs(xi_xbar$a2),
                         b3 = frekuensi3*abs(xi_xbar$a3),
                         b4 = frekuensi4*abs(xi_xbar$a4),
                         b5 = frekuensi5*abs(xi_xbar$a5),
                         b6 = frekuensi6*abs(xi_xbar$a6),
                         b7 = frekuensi7*abs(xi_xbar$a7))

SR <- sum(fi_xi_xbar)/banyak_data
SR

#Simpangan baku
xi_xbar2 <- data.frame(a1 = xi_xbar$a1**2,
                       a2 = xi_xbar$a2**2,
                       a3 = xi_xbar$a3**2,
                       a4 = xi_xbar$a4**2,
                       a5 = xi_xbar$a5**2,
                       a6 = xi_xbar$a6**2,
                       a7 = xi_xbar$a7**2)

fi_xi_xbar2 <- data.frame(b1 = frekuensi1*xi_xbar2$a1,
                          b2 = frekuensi2*xi_xbar2$a2,
                          b3 = frekuensi3*xi_xbar2$a3,
                          b4 = frekuensi4*xi_xbar2$a4,
                          b5 = frekuensi5*xi_xbar2$a5,
                          b6 = frekuensi6*xi_xbar2$a6,
                          b7 = frekuensi7*xi_xbar2$a7)

S <- sqrt(sum(fi_xi_xbar2)/banyak_data)
S