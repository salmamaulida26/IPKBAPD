# Pengaktifan Package
library(readxl)
library(rrcov)

# Input Data
data <- read_excel(file.choose())
df<-as.data.frame(data)
datas <- df[,-1]
head(datas,3)
rownames(datas) <- df$PUSKESMAS

ASI = datas$ASI
VIT_A = datas$`VIT A`
DPT = datas$DPT
Campak = datas$CAMPAK
PCV = datas$PCV
Rotavirus = datas$ROTAVIRUS
Pneumonia = datas$PNEUMONIA
Oralit = datas$ORALIT
Zinc = datas$ZINC
Sanitasi = datas$SANITASI
Air = datas$AIR
SBS = datas$SBS
CTPS = datas$CTPS

# Identifikasi titik outlier (misalnya, di luar 95% confidence ellipse)
threshold <- 0.95

p1 <- ggplot(datas, aes(x = ASI, y = VIT_A)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p1)

p2 <- ggplot(datas, aes(x = ASI, y = DPT)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p2)

p3 <- ggplot(datas, aes(x = ASI, y = Campak)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p3)

p4 <- ggplot(datas, aes(x = ASI, y = PCV)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p4)

p5 <- ggplot(datas, aes(x = ASI, y = Rotavirus)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p5)

p6 <- ggplot(datas, aes(x = ASI, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p6)

p7 <- ggplot(datas, aes(x = ASI, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p7)

p8 <- ggplot(datas, aes(x = ASI, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p8)

p9 <- ggplot(datas, aes(x = ASI, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p9)

p10 <- ggplot(datas, aes(x = ASI, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p10)

p11 <- ggplot(datas, aes(x = ASI, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p11)

p12 <- ggplot(datas, aes(x = ASI, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p12)

p13 <- ggplot(datas, aes(x = VIT_A, y = DPT)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p13)

p14 <- ggplot(datas, aes(x = VIT_A, y = Campak)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p14)

p15 <- ggplot(datas, aes(x = VIT_A, y = PCV)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p15)

p16 <- ggplot(datas, aes(x = VIT_A, y = Rotavirus)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p16)

p17 <- ggplot(datas, aes(x = VIT_A, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p17)

p18 <- ggplot(datas, aes(x = VIT_A, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p18)

p19 <- ggplot(datas, aes(x = VIT_A, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p19)

p20 <- ggplot(datas, aes(x = VIT_A, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p20)

p21 <- ggplot(datas, aes(x = VIT_A, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p21)

p22 <- ggplot(datas, aes(x = VIT_A, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p22)

p23 <- ggplot(datas, aes(x = VIT_A, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p23)

p24 <- ggplot(datas, aes(x = DPT, y = Campak)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p24)

p25 <- ggplot(datas, aes(x = DPT, y = PCV)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p25)

p26 <- ggplot(datas, aes(x = DPT, y = Rotavirus)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p26)

p27 <- ggplot(datas, aes(x = DPT, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p27)

p28 <- ggplot(datas, aes(x = DPT, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p28)

p29 <- ggplot(datas, aes(x = DPT, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p29)

p30 <- ggplot(datas, aes(x = DPT, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p30)

p31 <- ggplot(datas, aes(x = DPT, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p31)

p32 <- ggplot(datas, aes(x = DPT, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p32)

p33 <- ggplot(datas, aes(x = DPT, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p33)

p34 <- ggplot(datas, aes(x = Campak, y = PCV)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p34)

p35 <- ggplot(datas, aes(x = Campak, y = Rotavirus)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p35)

p36 <- ggplot(datas, aes(x = Campak, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p36)

p37 <- ggplot(datas, aes(x = Campak, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p37)

p38 <- ggplot(datas, aes(x = Campak, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p38)

p39 <- ggplot(datas, aes(x = Campak, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p39)

p40 <- ggplot(datas, aes(x = Campak, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p40)

p41 <- ggplot(datas, aes(x = Campak, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p41)

p42 <- ggplot(datas, aes(x = Campak, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p42)

p43 <- ggplot(datas, aes(x = PCV, y = Rotavirus)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p43)

p44 <- ggplot(datas, aes(x = PCV, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p44)

p45 <- ggplot(datas, aes(x = PCV, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p45)

p46 <- ggplot(datas, aes(x = PCV, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p46)

p47 <- ggplot(datas, aes(x = PCV, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p47)

p48 <- ggplot(datas, aes(x = PCV, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p48)

p49 <- ggplot(datas, aes(x = PCV, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p49)

p50 <- ggplot(datas, aes(x = PCV, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p50)

p51 <- ggplot(datas, aes(x = Rotavirus, y = Pneumonia)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p51)

p52 <- ggplot(datas, aes(x = Rotavirus, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p52)

p53 <- ggplot(datas, aes(x = Rotavirus, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p53)

p54 <- ggplot(datas, aes(x = Rotavirus, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p54)

p55 <- ggplot(datas, aes(x = Rotavirus, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p55)

p56 <- ggplot(datas, aes(x = Rotavirus, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p56)

p57 <- ggplot(datas, aes(x = Rotavirus, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p57)

p58 <- ggplot(datas, aes(x = Pneumonia, y = Oralit)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p58)

p59 <- ggplot(datas, aes(x = Pneumonia, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p59)

p60 <- ggplot(datas, aes(x = Pneumonia, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p60)

p61 <- ggplot(datas, aes(x = Pneumonia, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p61)

p62 <- ggplot(datas, aes(x = Pneumonia, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p62)

p63 <- ggplot(datas, aes(x = Pneumonia, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p63)

p64 <- ggplot(datas, aes(x = Oralit, y = Zinc)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p64)

p65 <- ggplot(datas, aes(x = Oralit, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p65)

p66 <- ggplot(datas, aes(x = Oralit, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p66)

p67 <- ggplot(datas, aes(x = Oralit, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p67)

p68 <- ggplot(datas, aes(x = Oralit, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p68)

p69 <- ggplot(datas, aes(x = Zinc, y = Sanitasi)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p69)

p70 <- ggplot(datas, aes(x = Zinc, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p70)

p71 <- ggplot(datas, aes(x = Zinc, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p71)

p72 <- ggplot(datas, aes(x = Zinc, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p72)

p73 <- ggplot(datas, aes(x = Sanitasi, y = Air)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p73)

p74 <- ggplot(datas, aes(x = Sanitasi, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p74)

p75 <- ggplot(datas, aes(x = Sanitasi, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p75)

p76 <- ggplot(datas, aes(x = Air, y = SBS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p76)

p77 <- ggplot(datas, aes(x = Air, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p77)

p78 <- ggplot(datas, aes(x = SBS, y = CTPS)) +
  geom_point(color = "black", fill = "blue", shape = 21, size = 3) +
  geom_text(aes(label = df$PUSKESMAS), vjust = -1, size = 3) +
  stat_ellipse(level = threshold, color = "red") +
  theme_minimal()
print(p78)
