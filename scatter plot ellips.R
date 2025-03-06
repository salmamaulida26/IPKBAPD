# Pengaktifan Package
library(readxl)
library(rrcov)
library(ggplot2)
library(dplyr)
library(sp)

# Input Data
data <- read_excel(file.choose())
df<-as.data.frame(data)
datas <- df[,-1]
head(datas,3)
rownames(datas) <- df$PUSKESMAS

# Identifikasi titik outlier (misalnya, di luar 95% confidence ellipse)
threshold <- 0.95

# Definisikan pasangan variabel dengan string
var_combinations <- list(
  c("ASI","VIT A"), c("ASI","DPT"),c("ASI","CAMPAK"),c("ASI","PCV"),c("ASI","ROTAVIRUS"),
  c("ASI","PNEUMONIA"),c("ASI",'ORALIT'),c("ASI","ZINC"),c("ASI","SANITASI"),
  c("ASI","AIR"),c("ASI","SBS"),c("ASI","CTPS"),
  c("VIT A","DPT"),c("VIT A","CAMPAK"),c("VIT A","PCV"),c("VIT A","ROTAVIRUS"),
  c("VIT A","PNEUMONIA"),c("VIT A",'ORALIT'),c("VIT A","ZINC"),c("VIT A","SANITASI"),
  c("VIT A","AIR"),c("VIT A","SBS"),c("VIT A","CTPS"),
  c("DPT","CAMPAK"),c("DPT","PCV"),c("DPT","ROTAVIRUS"),
  c("DPT","PNEUMONIA"),c("DPT",'ORALIT'),c("DPT","ZINC"),c("DPT","SANITASI"),
  c("DPT","AIR"),c("DPT","SBS"),c("DPT","CTPS"),
  c("CAMPAK","PCV"),c("CAMPAK","ROTAVIRUS"),
  c("CAMPAK","PNEUMONIA"),c("CAMPAK",'ORALIT'),c("CAMPAK","ZINC"),c("CAMPAK","SANITASI"),
  c("CAMPAK","AIR"),c("CAMPAK","SBS"),c("CAMPAK","CTPS"),
  c("PCV","ROTAVIRUS"),c("PCV","PNEUMONIA"),c("PCV",'ORALIT'),c("PCV","ZINC"),c("PCV","SANITASI"),
  c("PCV","AIR"),c("PCV","SBS"),c("PCV","CTPS"),
  c("ROTAVIRUS","PNEUMONIA"),c("ROTAVIRUS",'ORALIT'),c("ROTAVIRUS","ZINC"),c("ROTAVIRUS","SANITASI"),
  c("ROTAVIRUS","AIR"),c("ROTAVIRUS","SBS"),c("ROTAVIRUS","CTPS"),
  c("PNEUMONIA",'ORALIT'),c("PNEUMONIA","ZINC"),c("PNEUMONIA","SANITASI"),
  c("PNEUMONIA","AIR"),c("PNEUMONIA","SBS"),c("PNEUMONIA","CTPS"),
  c("ORALIT","ZINC"),c("ORALIT","SANITASI"),c("ORALIT","AIR"),c("ORALIT","SBS"),c("ORALIT","CTPS"),
  c("ZINC","SANITASI"),c("ZINC","AIR"),c("ZINC","SBS"),c("ZINC","CTPS"),
  c("SANITASI","AIR"),c("SANITASI","SBS"),c("SANITASI","CTPS"),
  c("AIR","SBS"),c("AIR","CTPS"),c("SBS","CTPS")
)

plots <- list()  # List untuk menyimpan semua plot

for (i in seq_along(var_combinations)) {
  x_var <- var_combinations[[i]][1]
  y_var <- var_combinations[[i]][2]
  
  # Pastikan variabel ada dalam datas
  if (!(x_var %in% colnames(datas)) | !(y_var %in% colnames(datas))) {
    warning(paste("Variabel", x_var, "atau", y_var, "tidak ditemukan dalam datas"))
    next  # Lewati iterasi jika variabel tidak ditemukan
  }
  
  # Buat plot sementara untuk mendapatkan koordinat ellips
  tmp_plot <- ggplot(datas, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    stat_ellipse(level = threshold)
  
  # Ambil data koordinat ellips
  ellips_data <- ggplot_build(tmp_plot)$data[[1]]
  
  # Cek apakah ellips_data ada dan memiliki cukup titik
  if (!is.null(ellips_data) && nrow(ellips_data) > 2) {
    # Tambahkan kolom indikator outlier berdasarkan polygon
    datas <- datas %>%
      mutate(outlier = mapply(point.in.polygon, .[[x_var]], .[[y_var]],
                              MoreArgs = list(ellips_data$x, ellips_data$y)) == 0)
  } else {
    datas <- datas %>% mutate(outlier = FALSE)  # Jika tidak ada ellips, semua dianggap bukan outlier
  }
  
  # Buat plot dan simpan ke dalam list
  plots[[i]] <- ggplot(datas, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
    geom_text(aes(label = ifelse(outlier, data$PUSKESMAS, "")), vjust = -1, size = 3) +
    stat_ellipse(level = threshold, color = "red") +
    ggtitle(paste(x_var, "vs", y_var)) +
    theme_minimal()
}

# Menampilkan semua plot
for (p in plots) print(p)
