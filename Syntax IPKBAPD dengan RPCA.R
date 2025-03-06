# Pengaktifan Package
library(readxl)
library(rrcov)

# Input Data
data <- read_excel(file.choose())
df<-as.data.frame(data)
datas <- df[,-1]
head(datas,3)
rownames(datas) <- df$PUSKESMAS

# Pemisahan Data Pneumonia dan Diare
datap <- datas[,c("ASI","VIT A","DPT","CAMPAK","PCV","PNEUMONIA","AIR","CTPS")]
datad <- datas[,c("ASI","VIT A","ROTAVIRUS","ORALIT","ZINC","SANITASI","AIR","SBS","CTPS")]

set.seed(123)

data.X = data.matrix(datas)
CH = CovMcd(data.X)
RobustDistance = CH$mah ; RobustDistance
cutoff <- qchisq(0.95,df=ncol(datas))
outliers <- RobustDistance > cutoff; outliers
theoutliers <- which(outliers==TRUE); theoutliers

# Pneumonia
data.Xp = data.matrix(datap)
CHp = CovMcd(data.Xp);CHp
summary(CHp)
# Diare
data.Xd = data.matrix(datad)
CHd = CovMcd(data.Xd);CHd
summary(CHd)

# Kovarians robust
COVRp <- CHp$cov
rCOVRp <- round(COVRp,5);rCOVRp
COVRd <- CHd$cov
rCOVRd <- round(COVRd,5);rCOVRd

# Determinan Kovarians Robust
det(CHp$cov)
det(CHd$cov)

# Dekomposisi eigen
CHCOV.e.p = eigen(CHp$cov)
round(CHCOV.e.p$values,5)
round(CHCOV.e.p$vectors,5)
CHCOV.e.d = eigen(CHd$cov)
round(CHCOV.e.d$values,5)
round(CHCOV.e.d$vectors,5)

# Variansi yang dijelaskan oleh setiap komponen
explained_variance_p <- CHCOV.e.p$values / sum(CHCOV.e.p$values)
explained_variance_d <- CHCOV.e.d$values / sum(CHCOV.e.d$values)

# Kumulatif variansi
cumulative_variance_p <- cumsum(explained_variance_p)
cumulative_variance_d <- cumsum(explained_variance_d)

# Menampilkan hasil
data.frame(PC = 1:length(CHCOV.e.p$values), 
           Eigenvalues = CHCOV.e.p$values, 
           Explained_Variance = explained_variance_p, 
           Cumulative_Variance = cumulative_variance_p)
data.frame(PC = 1:length(CHCOV.e.d$values), 
           Eigenvalues = CHCOV.e.d$values, 
           Explained_Variance = explained_variance_d, 
           Cumulative_Variance = cumulative_variance_d)

# Ambil 3 komponen utama
k <- 3
selected_eigen_values_p <- CHCOV.e.p$values[1:k]
selected_eigen_vectors_p <- CHCOV.e.p$vectors[, 1:k]
selected_eigen_values_d <- CHCOV.e.d$values[1:k]
selected_eigen_vectors_d <- CHCOV.e.d$vectors[, 1:k]

# 1. Hitung bobot komponen utama (w_i)
w_p <- selected_eigen_values_p / sum(selected_eigen_values_p); round(w_p,5)
w_d <- selected_eigen_values_d / sum(selected_eigen_values_d); round(w_d,5)

# 2. Hitung bobot variabel untuk setiap komponen utama
bobot_variabel_p <- selected_eigen_vectors_p; bobot_variabel_p
bobot_variabel_d <- selected_eigen_vectors_d; bobot_variabel_d

# 2.1 Penyaringan bobot positif (set negatif ke nol)
bobot_variabel_p_pos <- ifelse(bobot_variabel_p > 0, bobot_variabel_p, 0); round(bobot_variabel_p_pos,5)
bobot_variabel_d_pos <- ifelse(bobot_variabel_d > 0, bobot_variabel_d, 0); round(bobot_variabel_d_pos,5)

# 2.2 Normalisasi ulang bobot (agar total setiap PC tetap 1)
bobot_variabel_p_pos_std <- apply(bobot_variabel_p_pos, 2, function(x) if (sum(x) > 0) x / sum(x) else x); round(bobot_variabel_p_pos_std,5)
bobot_variabel_d_pos_std <- apply(bobot_variabel_d_pos, 2, function(x) if (sum(x) > 0) x / sum(x) else x); round(bobot_variabel_d_pos_std,5)


# 3. Hitung skor komponen utama (t_i)
t_scores_p <- as.matrix(datap) %*% bobot_variabel_p_pos_std
t_scores_d <- as.matrix(datad) %*% bobot_variabel_d_pos_std

# 4. Hitung indeks Pneumonia dan Diare
I_Pneumonia <- rowSums(t_scores_p %*% diag(w_p))
I_Diare <- rowSums(t_scores_d %*% diag(w_d))

# Indeks Penanggulangan Kematian Balita akibat Pneumonia dan Diare
# Menggabungkan kedua dataset secara horizontal
combined_data <- cbind(I_Pneumonia, I_Diare)

# Menghitung rata-rata setiap baris
row_average <- rowMeans(combined_data, na.rm = TRUE)

# Menyimpan hasil dalam data frame baru
average_df <- data.frame(I_Pneumonia,I_Diare, IndeksPuskesmas = row_average);average_df

# Ranking
# Mengurutkan dataframe menurut IndeksPuskesmas secara menurun
average_df <- average_df[order(-average_df$IndeksPuskesmas), ]
# Ranking secara menurun
average_df$RankPuskesmas <- 1:nrow(average_df)
# Menampilkan update dataframe
average_df

# Kategorisasi
q <- quantile(average_df$IndeksPuskesmas, probs = c(0, 0.25, 0.50, 0.75, 1))
quantile(average_df$IndeksPuskesmas, probs = c(0.25, 0.5, 0.75))

Kategorisasi <- cut(
  average_df$IndeksPuskesmas, 
  breaks = q, 
  labels = c("Sangat Rendah", "Rendah", "Tinggi", "Sangat Tinggi"), 
  include.lowest = TRUE
)
    
# Kelompokkan Puskesmas berdasarkan kategori
kategori_puskesmas <- split(row.names(average_df), Kategorisasi)
kategori_puskesmas
