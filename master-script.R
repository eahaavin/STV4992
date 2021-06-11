
library(QCA)
library(tidyverse)
library(readxl)
library(stargazer)



###########GTD############


globalterrorismdb_0919dist <- read_excel("~/Masteroppgave/Terrorisme/Data/globalterrorismdb_0919dist.xlsx")


#Datasett fra 2012-2018 
gtd_2012 <- globalterrorismdb_0919dist %>%
  select(eventid, iyear, imonth, iday, doubtterr, country, country_txt,
         region, success, nkill, nwound, attacktype1_txt, gname, targtype1_txt) %>%
  filter(iyear <= 2018, iyear >=2012, doubtterr == 0,
         country_txt %in% c("Austria", "Belgium", 
                            "Denmark", 
                            "Finland", "France", "Germany", 
                            "Greece", "Iceland", 
                            "Ireland", "Italy", "Luxembourg", 
                            "Netherlands", "Norway", 
                            "Portugal", "Spain", "Sweden", 
                            "Switzerland", "United Kingdom"))





##Trekker ut tidsperiode fra 1995-2018 til figur 1.1

gtd_plot <- globalterrorismdb_0919dist %>%
  select(eventid, iyear, imonth, iday, doubtterr, country, country_txt,
         region, success, nkill, nwound, attacktype1_txt, gname, targtype1_txt) %>%
  filter(iyear <= 2018, iyear >= 1995, doubtterr == 0,
         country_txt %in% c("Austria", "Belgium", 
                            "Denmark", 
                            "Finland", "France", "Germany", 
                            "Greece", "Iceland", 
                            "Ireland", "Italy", "Luxembourg", 
                            "Netherlands", "Norway", 
                            "Portugal", "Spain", "Sweden", 
                            "Switzerland", "United Kingdom"))



##Figur 1.1

gtd_plot %>%
  group_by(iyear) %>%
  count(country_txt) %>%
  ggplot(aes(x = iyear,
             y = n, fill = iyear)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Antall angrep i Vest-Europa 1995-2018",
       y = "Antall angrep",
       x = "År") +
  scale_x_continuous(breaks = c(1995, 1997, 1999, 2001, 2003, 2005, 2007,
                                2009, 2011, 2013, 2015, 2017)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))


  
#Figur 1.2

gtd_2012 %>%
  group_by(country_txt, iyear) %>%
  count(country_txt) %>%
  ggplot(aes(x = iyear,
         y = country_txt, fill = n)) +
  geom_tile() +
  labs(x = "", y = "", title = "Angrep per land 2012-2018", fill = "Antall") +
  scale_fill_gradient(low = "#FECFCF", high = "#FF3939") +
  theme_classic() + 
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018)) +
  scale_y_discrete(labels = c("Østerrike", "Belgia", "Danmark", "Finland",
                              "Frankrike", "Tyskland", "Hellas", "Island", "Irland",
                              "Italia", "Nederland", "Norge", "Spania", "Sverige",
                              "Sveits", "Storbritannia"))

  
  

###Lager for analysen, teller antall rader. 1 rad = ett angrep
qca_2012 <- gtd_2012 %>%
  data.frame() %>%
  count(country_txt, sort = T)

#Portugal og Luxembourg har ingen registrerte angrep, legger inn manuelt
qca_2012 <- qca_2012 %>%
  add_row(country_txt = "Portugal", n = 0)

qca_2012 <- qca_2012 %>%
  add_row(country_txt = "Luxembourg", n = 0)


##Ender navn
qca_2012 <- qca_2012 %>%
  rename(land = country_txt, antall_angrep = n)



########VDEM data#############

V_Dem_CY_Full_Others_v10 <- read_csv("V-Dem-CY-Full+Others-v10.csv")



##Trekker ut land, periode og polariserings variabelen
vdem <- V_Dem_CY_Full_Others_v10 %>%
  select(country_name, year, v2smpolsoc_osp) %>%
  filter(year <= 2018, year >=2012, 
         country_name %in% c("United Kingdom", "Greece", "France", 
                             "Spain", "Germany", "Ireland", "Italy", 
                             "Sweden", "Netherlands", 
                             "Belgium", "Austria", "Finland", "Switzerland", 
                             "Denmark", "Norway", "Portugal", 
                             "Iceland", "Luxembourg"))




##Snur skalaretningen på sosial polarisering, slik at høy verdi = høy polarisering
vdem$sospol <- vdem$v2smpolsoc_osp*-1+5

##Sjekker at skalaen er riktig gjennomført
vdem %>%
  ggplot(aes(x = sospol, y = v2smpolsoc_osp)) +
  geom_point()

##Trekker ut gjennomsnittsverdier
vdem %>%
  group_by(country_name) %>%
  summarise(mean(sospol)) -> sosPol


#Bytter navn på variabelen
sosPol <- sosPol %>%
  rename(polsos = `mean(sospol)`)


#Legger inn i datasettet
qca_2012 <- qca_2012 %>%
 left_join(sosPol, by = c("land" = "country_name"))


######Eurostat - fattigdom######

sos_risk <- read_csv("estat_sdg_01_10.csv")


#Trekker ut periode, land og variabel
sos_risk <- sos_risk %>%
  select(unit, geo, TIME_PERIOD, OBS_VALUE) %>%
  filter(unit == "PC", TIME_PERIOD <= 2018, TIME_PERIOD >= 2012,
         geo %in% c("UK", "EL", "FR", "DE", "IE", "SE",
                    "IT", "ES", "NL", "FI", "BE", "AT",
                    "DK", "CH", "IS", "NO", "PT", "LU"))


#Regner ut gjennomsnittsverdier
sos_risk <- sos_risk %>%
  group_by(geo) %>%
  summarise(mean(OBS_VALUE))

#Bytter navn
sos_risk <- sos_risk %>%
  rename(sosial_risk = `mean(OBS_VALUE)`)

#Runder av verdiene til en desimal
sos_risk <- sos_risk %>%
  mutate(sosial_risk = round(sosial_risk, digits = 1))


#Oppretter landkoder
country_code <- c("UK", "EL", "FR", "DE", "IE", "SE", 
                  "IT", "ES", "NL", "FI", "BE", "AT",
                  "DK", "CH", "IS", "NO", "PT", "LU")

#Legger inn landkodene i datasettet
qca_2012 <- qca_2012 %>%
  cbind(country_code)

#Legger inn variabelen i datasettet
qca_2012 <- qca_2012 %>%
  left_join(sos_risk, by = c("country_code" = "geo"))



##########Etnisk mangfold Eurostat ###########

migr_pop <- read_csv("migr_pop3ctb_1_Data.csv")

##Antall personer født utenfor EU i perioden 2014-2018

etnMang <- migr_pop %>%
  select(TIME, GEO, C_BIRTH, SEX, Value) %>%
  filter(TIME >= 2014, TIME <= 2018, C_BIRTH == "Non-EU28 countries (2013-2020) nor reporting country",
         SEX == "Total", 
         GEO %in% c("Austria", "Belgium", 
                    "Denmark", 
                    "Finland", "France", "Germany (until 1990 former territory of the FRG)", 
                    "Greece", "Iceland", 
                    "Ireland", "Italy", "Luxembourg", 
                    "Netherlands", "Norway", 
                    "Portugal", "Spain", "Sweden", 
                    "Switzerland", "United Kingdom"))

###Fjerner komma formateringen
etnMang$Value <- as.numeric(gsub(",","\\",etnMang$Value))


##antall som numerisk variabel
etnMang$antall <- as.numeric(etnMang$Value)

#trekker ut gjennomsnittsverdier
etnMang <- etnMang%>%
  group_by(GEO) %>%
  summarise(mean(antall))

#Endrer navn på Tyskland
land2 <- dplyr::recode(etnMang$GEO, "Germany (until 1990 former territory of the FRG)" = "Germany")

#Legger inn riktig land navn
etnMang <- etnMang %>%
  cbind(land2)


#Endrer av på variabelen
etnMang <- etnMang %>%
  rename(etnPax = `mean(antall)`)

#Fjerner landkodene
etnMang <- etnMang %>%
  mutate(GEO = NULL)

#legger inn variabelen i datasettet
qca_2012 <- qca_2012 %>%
  left_join(etnMang, by = c("land" = "land2"))



###Lager en variabel som måler andel innbyggere født utenfor EU

qca_2012 <- qca_2012 %>%
  mutate(etniskMangfold = etnPax/befolkning*100)


###Legger inn tall for antall mottatte asylsøknader
asylEuro <- migr_asyl %>%
  select(TIME, GEO, CITIZEN, ASYL_APP, Value) %>%
 filter(TIME >= 2012, TIME <= 2018, CITIZEN == "Total", ASYL_APP == "Asylum applicant", 
         GEO %in% c("Austria", "Belgium", 
                    "Denmark", 
                    "Finland", "France", "Germany (until 1990 former territory of the FRG)", 
                    "Greece", "Iceland", 
                    "Ireland", "Italy", "Luxembourg", 
                    "Netherlands", "Norway", 
                    "Portugal", "Spain", "Sweden", 
                    "Switzerland", "United Kingdom"))

#Endrer navnet til Tyskland
land <- recode(asylEuro2015$GEO, "Germany (until 1990 former territory of the FRG)" = "Germany")

#Legger inn riktig land navn
asylEuro <- asylEuro %>%
  cbind(land)

#Endrer kommaformateringen
asylEuro$Value <- as.numeric(gsub(",","\\",asylEuro$Value))

#Trekker ut gjennomsnittsverdiene
asylEuro <- asylEuro %>%
  group_by(land) %>%
  summarise(mean(Value))

#Endrer navn
asylEuro  <- asylEuro %>%
  rename(asyl_app_n = `mean(Value)`)

#Legger inn variabelen i datasettet
qca_2012 <- qca_2012 %>%
 left_join(asylEuro, by = c("land" = "land"))


##Mottatte asylsøknader per 100.000 innbyggere
qca_2012 <- qca_2012 %>%
 mutate(asyl_capita = asyl_app_n/befolkning*100000)

  

#########Befolkningstall################
##Befolkningstall fra verdensbanken
API_SP_POP_TOTL_DS2 <- read_excel("~/Masteroppgave/Terrorisme/Data/API_SP.POP.TOTL_DS2.xls")

pop_tot_2012 <- API_SP_POP_TOTL_DS2 %>%
  select(1, 57:63) %>%
  filter(`Country Name` %in% c("United Kingdom", "Greece", "France", 
                               "Spain", "Germany", "Ireland", "Italy", 
                               "Sweden", "Netherlands", 
                               "Belgium", "Austria", "Finland", "Switzerland", 
                               "Denmark", "Norway", "Portugal", 
                               "Iceland", "Luxembourg"))



##Legger til gjennomsnittelig befolkningstall for perioden
pop_tot_2012$snitt.pop = rowMeans(pop_tot_2012[,c(-1)])


#Trekker ut variabelen med gjennomsnittlig befolkningsstørrelse
popSnitt2012 <- pop_tot_2012 %>%
  select(`Country Name`, snitt.pop)
pop_tot_2012

#Legger inn i datasette
qca_2012 <- qca_2012 %>%
  left_join(popSnitt2012, by = c("land" = "Country Name"))


#Endrer navn
qca_2012 <- qca_2012 %>%
  rename(befolkning = snitt.pop)


############List of former Trust and Non-Self governing territories fra 1946######

##Legger inn antall "kolonier" per land som en vektor
koloniN <- c(53, 0, 18, 0, 0, 0, 1, 2, 4, 0, 2, 0, 1, 0, 0, 0, 9, 0)


#Legger vektoren inn i datasettet
qca_2012 <- qca_2012 %>%
  cbind(koloniN)



####################Prosessering i QCA rådata#####################

##Antall angrep per million innbygger 2012-2018
qca_2012 <- qca_2012 %>%
  mutate(angrepPerMill = antall_angrep/befolkning*1000000)




##Fjerner ekstra desimaler i de aktuelle målene

qca_2012 <- qca_2012 %>%
  mutate(angrepPerMill = round(angrepPerMill, digits = 1))

qca_2012 <- qca_2012 %>%
  mutate(etniskMangfold = round(etniskMangfold, digits = 1))

qca_2012 <- qca_2012 %>%
  mutate(asyl_capita = round(asyl_capita, digits = 1))

qca_2012 <- qca_2012 %>%
  mutate(polsos = round(polsos, digits = 1))



######Kalibrering til fuzzy-skårer######


#Kalibrering koloni, nedre = 0, cut-off = 0.99, øvre= 10
qca_2012$KOL <- admisc::recode(qca_2012$koloniN, cuts = "0, 0.99, 10", values = "0, 0.25, 0.75, 1")

#Kalibrering av asylsøknader, nedre = 60, cut-off=200, øvre= 400
qca_2012$ASY <- admisc::recode(qca_2012$asyl_capita, cuts = "60, 200, 400", values = "0, 0.25, 0.75, 1")

#Kalibrering polarisering, nedre= 1, cut-off=2, øvre=3
qca_2012$POL <- admisc::recode(qca_2012$polsos, cuts = "1, 2, 3", values = "0, 0.25, 0.75, 1")

#Kalibrering fattigdom, nedre = 17, cut-off = 24, øvre= 30
qca_2012$SOR <- admisc::recode(qca_2012$sosial_risk, cuts = "17, 24, 30", values = "0, 0.25, 0.75, 1")

#Legger in fuzzyskårer for militær intervensjon
qca_2012$MIL <- c(1, 0, 1, 0.75, 0, 0.25, 0.75, 0.25, 1, 0.25, 
                  1, 0, 1, 0, 0, 0.25, 0.25, 0)

#Legger inn fuzzyskårer for separatisme
qca_2012$SEP <- c(1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#Kalibrerer komponentskåre for utfall, antall angrep. nedre=10, cut-off=50, øvre=250
qca_2012$ANG3 <- admisc::recode(qca_2012$antall_angrep, cuts = "10, 50, 250", values = "0, 0.25, 0.75, 1")


#Kalibrerer komponentskåre for utfall, angrep per milioner innbyggere, nedre= 0.5, cut-off= 1, øvre=10
qca_2012$ANG2 <- admisc::recode(qca_2012$angrepPerMill, cuts = "0.5, 1, 10", values = "0, 0.25, 0.75, 1")


##Slår sammen komponentskårene for utfall og deler på 2
qca_2012 <- qca_2012 %>%
  mutate(ANG6 = (ANG2+ANG3)/2)

#kalibrerer utfall. nedre=0.24. cut-off=0.49, øvre=0.75
qca_2012$ANG <- admisc::recode(qca_2012$ANG6, cuts = "0.24, 0.49, 0.75", values = "0, 0.25, 0.75, 1")


#Kalibrerer entisk mangfold, nedre= 4.5, cut-off=7, øvre= 10
qca_2012$MANG <- admisc::recode(qca_2012$etniskMangfold, cuts = "4.5, 7, 10", values = "0, 0.25, 0.75, 1")

#Bruker OR for å kombinerer settene med asylsøknader og etnisk mangfold
qca_2012$MANG_ASY <- with(qca_2012, fuzzyor(MANG, ASY))










##############Plot over kalibrering##########

qca_2012 %>%
  ggplot(aes(land, antall_angrep)) +
  geom_bar(position = "dodge", stat = "identity", fill = "steelblue") +
  geom_line(aes(y = 10), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 50), group = 1, alpha = 0.5) +
  geom_line(aes(y = 250), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Antall angrep") +
  xlab("Land") +
  ggtitle("Antall angrep 2012-2018") +
  theme_bw() +
  geom_text(aes(label = antall_angrep), position = position_dodge(width=0.9), vjust=-0.25) +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))


qca_2012 %>%
  ggplot(aes(land, angrepPerMill)) +
  geom_bar(position = "dodge", stat = "identity", fill = "steelblue") +
  geom_line(aes(y = 0.5), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 1), group = 1, alpha = 0.5) +
  geom_line(aes(y = 10), group = 10, alpha = 0.5, linetype = "dashed") +
  ylab("Antall angrep per million") +
  xlab("Land") +
  geom_text(aes(label = angrepPerMill), position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("Antall angrep per million innbyggere 2012-2018") +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))


###kombinert skåre

qca_2012 %>%
  ggplot(aes(land, ANG6)) +
  geom_bar(position = "dodge", stat = "identity", fill = "steelblue") +
  geom_line(aes(y = 0.24), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 0.49), group = 1, alpha = 0.5) +
  geom_line(aes(y = 0.75), group = 1, alpha = 0.5, linetype = "dashed") +
  xlab("Land") +
  ylab("Kombinert skåre") +
  ggtitle("Kombinert skåre for utfall") +
  theme_bw() +
  geom_text(aes(label = ANG6), position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))

###Figurer over de ulike målene som er brukt i kalibreringen


#Polarisering
qca_2012 %>%
  ggplot(aes(land, polsos)) +
  geom_bar(position = "dodge", stat = "identity", fill = "mediumpurple") +
  geom_line(aes(y = 2), group = 1, alpha = 0.5) +
  geom_line(aes(y = 1), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 3), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Verdi") +
  xlab("Land") +
  ggtitle("Polarisering") +
  geom_text(aes(label = polsos), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))
  



#fatigdom
qca_2012 %>%
  ggplot(aes(land, sosial_risk)) +
  geom_bar(position = "dodge", stat = "identity", fill = "sienna2") +
  geom_line(aes(y = 24), group = 1, alpha = 0.5) +
  geom_line(aes(y = 17), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 30), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Prosentandel av befolkningen") +
  xlab("Land") +
  ggtitle("Risiko for fattigdom") +
  geom_text(aes(label = sosial_risk), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))



##asylsøknader

qca_2012 %>%
  ggplot(aes(land, asyl_capita)) +
  geom_bar(position = "dodge", stat = "identity", fill = "yellow3") +
  geom_line(aes(y = 50), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 200), group = 1, alpha = 0.5) +
  geom_line(aes(y = 400), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Antall") +
  xlab("Land") +
  ggtitle("Mottatte asylsøknader per innbygger") +
  geom_text(aes(label = asyl_capita), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))

#etnisk mangfold
qca_2012 %>%
  ggplot(aes(land, etniskMangfold)) +
  geom_bar(position = "dodge", stat = "identity", fill = "yellow3") +
  geom_line(aes(y = 4), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 7), group = 1, alpha = 0.5) +
  geom_line(aes(y = 10), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Andel") +
  xlab("Land") +
  ggtitle("Prosentandel av befolkningen født utenfor EU") +
  geom_text(aes(label = etniskMangfold), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH", "UK"))

#sammenslått sett

qca_2012 %>%
  ggplot(aes(land, MANG_ASY)) +
  geom_bar(position = "dodge", stat = "identity", fill = "yellow3") +
  ylab("Sett-medlemskap") +
  xlab("Land") +
  ggtitle("Etnisk mangfold OR innvandring") +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH", "UK"))



##koloni
qca_2012 %>%
  ggplot(aes(land, koloniN)) +
  geom_bar(position = "dodge", stat = "identity", fill = "lightpink1") +
  geom_line(aes(y = 0), group = 1, alpha = 0.5, linetype = "dashed") +
  geom_line(aes(y = 0.99), group = 1, alpha = 0.5) +
  geom_line(aes(y = 10), group = 1, alpha = 0.5, linetype = "dashed") +
  ylab("Antall") +
  xlab("Land") +
  ggtitle("Antall kolonier fra 1946") +
  geom_text(aes(label = koloniN), position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_bw() +
  scale_x_discrete(labels = c("AT", "BE", "DK",
                              "FI", "FR", "DE",
                              "GR","IS", "IE", "IT",
                              "LU", "NL", "NO", "PT",
                              "ES", "SE", "CH",
                              "UK"))





#Trekker ut QCA betingelsene og lagrer dette i en nytt datasett
QcaData <- qca_2012 %>%
  select(ANG, KOL, POL, MIL, MANG_ASY, SEP, SOR)

#Endrer navn
QcaData <- QcaData %>%
  rename(terror = ANG, koloni = KOL, polarisering = POL, 
         intervensjon = MIL, mangfold = MANG_ASY,
         separatisme = SEP, sosialrisk = SOR)


#Legger til landkoder som rad navn
row.names(QcaData) <- c("UK", "GR", "FR", "DE", "IE", "SE",
                        "IT", "ES", "NL", "FI", "BE", "AT",
                        "DK", "CH", "IS", "NO", "PT", "LU")

#Lager matrise over fuzzy skårene
stargazer(QcaData,
          type = "html",
          summary = F,
          align = T,
          digits = 2,
          covariate.labels = c("Land", "Terror", "Koloni",
                               "Polarisering", "Militær", "Mangfold",
                               "Separatisme", "Fattigdom"),
          out = "kal_matrise.html")


#Lager en matrise med deskriptiv statistikk for betingelsene

desk <- qca_2012 %>%
  select(koloniN, polsos, sosial_risk, asyl_capita, etniskMangfold)

stargazer(desk, type = "text",
          title = c("Deskriptiv statistikk"),
          covariate.labels = c("Koloni", "Polarisering", "Fattigdom",
                               "Asylsøknader", "Entisk mangfold"),
          digits = 1,
          out = "deskriptiv.html")



##regner ut avvikene fra cut-off punktene for de "kvantitative" betingelsene
qca_2012 <- qca_2012 %>%
  mutate(avvik_ANG = ANG6 - 0.49) %>%
  mutate(avvik_KOL = koloniN - 0.99) %>%
  mutate(avvik_POL = round(polsos - 2, digits = 1)) %>%
  mutate(avvik_SOR = round(sosial_risk - 24, digits = 1)) %>%
  mutate(avvik_MANG = round(etniskMangfold - 7, digits = 1)) %>%
  mutate(avvik_ASYL = round(asyl_capita - 200, digits = 1))

###Tar ut informasjon til tabeller, som kan brukes som vedlegg i oppgaven

#utfall
kali_ang <- qca_2012 %>%
  select(land, ANG6, avvik_ANG, ANG) %>%
  arrange(desc(ANG6))


stargazer(kali_ang, type = "html",
          summary = F, 
          title = "Kalibrering av utfallssett",
          covariate.labels = c("","Land", "Kombinert skåre", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_angrep.html")

#Koloni
kali_kol <- qca_2012 %>%
  select(land, koloniN, avvik_KOL, KOL) %>%
  arrange(desc(koloniN))


stargazer(kali_kol, type = "html",
          summary = F, 
          title = "Kalibrering av kolonihistorie",
          covariate.labels = c("", "Land", "Antall kolonier", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_koloni.html")

#Polarisering
kali_pol <- qca_2012 %>%
  select(land, polsos, avvik_POL, POL) %>%
  arrange(desc(polsos))

stargazer(kali_pol, type = "html",
          summary = F,
          title = "Kalibrering av polarisering",
          covariate.labels = c("","Land", "Polarisering", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_polarisering.html")

#Fattigdom
kali_SOR <- qca_2012 %>%
  select(land, sosial_risk, avvik_SOR, SOR) %>%
  arrange(desc(sosial_risk))

stargazer(kali_SOR, type = "html",
          summary = F,
          title = "Kalibrering av risiko for fattigdom",
          covariate.labels = c("", "Land", "Prosentandel i risikosonen", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_sosialrisk.html")


#Etnisk mangfold
kali_mang <- qca_2012 %>%
  select(land, etniskMangfold, avvik_MANG, MANG) %>%
  arrange(desc(etniskMangfold))

stargazer(kali_mang, type = "html",
          summary = F, 
          title = "Kalibrering av etnisk mangfold",
          covariate.labels = c("", "Land", "% født utenfor EU", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_mangfold.html")

#Asylsøknader
kali_asy <- qca_2012 %>%
  select(land, asyl_capita, avvik_ASYL, ASY) %>%
  arrange(desc(asyl_capita))

stargazer(kali_asy, type = "html",
          summary = F, 
          title = "Kalibrering av asylsøknader",
          covariate.labels = c("", "Land", "Antall asylsøknader", "Avvik fra cut-off", "Medlemskap"),
          out = "kali_asyl.html")



######Nødvendighets analyse######



###nødvendighet, en og en betingelse
necKOL <- pof("koloni <= terror", data = QcaData, relation = "necessity")
necPOL <- pof("polarisering <= terror", data = QcaData, relation = "necessity")
necMIL <- pof("intervensjon <= terror", data = QcaData, relation = "necessity")
necMANG <- pof("mangfold <= terror", data = QcaData, relation = "necessity")
necSEP <- pof("separatisme <= terror", data = QcaData, relation = "nec")
necSOR <- pof("sosialrisk <= terror", data = QcaData, relation = "nec")



##lagrer relasjonene i ett datasett
necessity <- necKOL$incl.cov

#Legger til resterende betingelser
necessity <- necessity %>%
  add_row(necPOL$incl.cov) %>%
  add_row(necSOR$incl.cov) %>%
  add_row(necMIL$incl.cov) %>%
  add_row(necMANG$incl.cov) %>%
  add_row(necSEP$incl.cov)

#Legger til radnavn
row.names(necessity) <- c("Koloni", "Polarisering", "Fattigdom",
                          "Intervensjon", "Mangfold", "Separatisme")

#Lagrer resultatene i en tabell

stargazer(necessity, type = "html",
          title = "Nødvendighet",
          summary = F,
          covariate.labels = c("Betingelser", "Konsistens", "Relevans", "Dekning"), 
          out = "nødvendighet.html")



###Nødvendighet, ikke-utfall

negasjonNecKol <- pof("koloni", "~terror", data = QcaData, relation = "nec")
negasjonNecPol <- pof("polarisering", "~terror", data = QcaData, relation = "nec")
negasjonNecSOR <- pof("sosialrisk", "~terror", data = QcaData, relation = "nec")
negasjonNecMil <- pof("intervensjon", "~terror", data = QcaData, relation = "nec")
negasjonNecMang <- pof("mangfold", "~terror", data = QcaData, relation = "nec")
negasjonNecSEP <- pof("separatisme", "~terror", data = QcaData, relation = "nec")

###lagrer på samme måte som ovenfor

NecNeg <- negasjonNecKol$incl.cov

NecNeg <- NecNeg %>%
  add_row(negasjonNecPol$incl.cov) %>%
  add_row(negasjonNecSOR$incl.cov) %>%
  add_row(negasjonNecMil$incl.cov) %>%
  add_row(negasjonNecMang$incl.cov) %>%
  add_row(negasjonNecSEP$incl.cov)

row.names(NecNeg) <- c("Koloni", "Polarisering", "Fattigdom",
                          "Militær", "Mangfold", "Separatisme")

#oppretter tabell

stargazer(NecNeg, type = "html",
          title = "Nødvendighet, ~Terror",
          summary = F,
          covariate.labels = c("Betingelser", "Konsistens", "Relevans", "Dekning"), 
          out = "nødvendighet_negasjon.html")



#########Tilsrekkelighet######


##Ser på tilstrekkelighet for hver enkelt betingelse

tilKol <- pof("koloni => terror", data = QcaData, relation = "suf")
tilPol <- pof("polarisering => terror", data = QcaData, relation = "suf")
tilSOR <- pof("sosialrisk => terror", data = QcaData, relation = "suf")
tilMil <- pof("intervensjon => terror", data = QcaData, relation = "suf")
tilMang <- pof("mangfold => terror", data = QcaData, relation = "suf")
tilSEP <- pof("separatisme => terror", data = QcaData, relation = "suf")

#Lagrer dette i ett datasett
tilstrekkelig <- tilKol$incl.cov

#legger til resterende i datasettet
tilstrekkelig <- tilstrekkelig %>%
  add_row(tilPol$incl.cov) %>%
  add_row(tilSOR$incl.cov) %>%
  add_row(tilMil$incl.cov) %>%
  add_row(tilMang$incl.cov) %>%
  add_row(tilSEP$incl.cov)

#Fjerner covU siden denne er tom uansett
tilstrekkelig <- tilstrekkelig %>%
  mutate(covU = NULL)

#Legger til radnavn
row.names(tilstrekkelig) <- c("Koloni", "Polarisering", 
                              "Fattigdom", "Intervensjon", "Mangfold", "Separatisme")


#Lagrer dette i en tabell
stargazer(tilstrekkelig, type = "html",
          title = "Tilstrekkelighet",
          summary = F,
          covariate.labels = c("Betingelser", "Konsistens", "PRI", "Dekning"),
          out = "tilstrekkelig.html")



###Tilstrekkelighet for ikke-utfall

sufNegasjonKOL <- pof("koloni, => ~terror", data = QcaData, relation = "suf")
sufNegasjonPOL <- pof("polarisering => ~terror", data = QcaData, relation = "suf")
sufNegasjonSOR <- pof("sosialrisk => ~terror", data = QcaData, relation = "suf")
sufNegasjonMIL <- pof("intervensjon => ~terror", data = QcaData, relation = "suf")
sufNegasjonMANG <- pof("mangfold => ~terror", data = QcaData, relation = "suf")
sufNegasjonSEP <- pof("separatisme => ~terror", data = QcaData, relation = "suf")

#Samme fremgangsmåte som ovenfor
negasjonSuf <- sufNegasjonKOL$incl.cov

negasjonSuf <- negasjonSuf %>%
  add_row(sufNegasjonPOL$incl.cov) %>%
  add_row(sufNegasjonSOR$incl.cov) %>%
  add_row(sufNegasjonMIL$incl.cov) %>%
  add_row(sufNegasjonMANG$incl.cov) %>%
  add_row(sufNegasjonSEP$incl.cov)

negasjonSuf <- negasjonSuf %>%
  mutate(covU = NULL)


row.names(negasjonSuf) <- c("Koloni", "Polarisering", 
                              "Fattigdom", "Militær", "Mangfold", "Separatisme")

#Lagrer i en tabell
stargazer(negasjonSuf, type = "html",
          title = "Tilstrekkelighet, ~Terror",
          summary = F,
          covariate.labels = c("Betingelser", "Konsistens", "PRI", "Dekning"),
          out = "tilstrekkelig_negasjon.html")




########sannhetstabell###############


sannTabell <- truthTable(QcaData, outcome = "terror", conditions = "sosialrisk,
                         intervensjon, mangfold, polarisering, separatisme, koloni",
                         complete = T, show.cases = T, incl.cut = 0.75,
                         sort.by = "incl, n+")


#Lager xyplot for å undersøke tyskland nærmere 
XYplot("intervensjon*mangfold~koloni", terror, data = QcaData, relation = "suf",
       clabels = rownames(QcaData), xlab = "militær*mangfold~koloni", ylab = "Terror")

XYplot("intervensjon*mangfold~koloni", ~terror, data = QcaData, relation = "suf",
       clabels = rownames(QcaData), xlab = "militær*mangfold~koloni", ylab = "~Terror")


#Trekker ut informasjonen fra tabellen
tabell <- sannTabell$tt

#Gjør tomme celler til NA og fjerner n kolonnen
tabell <- tabell %>%
  mutate(incl = ifelse(incl == "-", NA, incl)) %>%
  mutate(PRI = ifelse(PRI == "-", NA, PRI)) %>%
  mutate(n = NULL)

#Fjerner NA cellene for penere presentasjon
tabell <- tabell %>%
  na.omit()

#Gjør incl og PRI til numerisk
tabell$incl <- as.numeric(tabell$incl)
tabell$PRI <- as.numeric(tabell$PRI)

#Runder av verdiene
tabell <- tabell %>%
  mutate(incl = round(incl, digits = 3)) %>%
  mutate(PRI = round(PRI, digits = 3))

#Organiserer i synkende rekkefølge
tabell <- tabell %>%
  arrange(desc(incl))

#Lagrer dette i en tabell
stargazer(tabell, type = "text", 
          summary = F,
          title = "Sannhetstabell",
          covariate.labels = c("", "Fattigdom", "Militær", "Mangfold",
                               "Polarisering", "Separatisme", "Koloni", "OUT", "Incl", "PRI",
                               "Land"),
          digits = 2, 
          out = "san_tab.html")


#Lager en fullstendig sannhetstabell til appendiks 

tabell_1 <- sannTabell$tt

#Setter tomme celler til 0 isteden for NA
tabell_1 <- tabell_1 %>%
  mutate(incl = ifelse(incl == "-", 0, incl)) %>%
  mutate(PRI = ifelse(PRI == "-", 0, PRI)) %>%
  mutate(n = NULL)

#Numeriske verdier
tabell_1$incl <- as.numeric(tabell_1$incl)
tabell_1$PRI <- as.numeric(tabell_1$PRI)

#Runder av veridene
tabell_1 <- tabell_1 %>%
  mutate(incl = round(incl, digits = 3)) %>%
  mutate(PRI = round(PRI, digits = 3))

#Synkende rekkefølge
tabell_1 <- tabell_1 %>%
  arrange(desc(incl))

#Lagrer dette i en tabell
stargazer(tabell_1, type = "text", 
          summary = F,
          title = "Sannhetstabell",
          covariate.labels = c("", "Fattigdom", "Militær", "Mangfold",
                               "Polarisering", "Separatisme", "Koloni", "OUT", "Incl", "PRI",
                               "Land"),
          digits = 2, 
          out = "san_tab_full.html")



###Sannhetstabell negasjon

sannhet_negasjon <- truthTable(QcaData, outcome = "~terror", conditions = "sosialrisk,
                         intervensjon, mangfold, polarisering, separatisme, koloni",
           complete = T, show.cases = T, incl.cut = 0.75, sort.by = "incl, n+")

#Tar ut informasjonen fra tabellen

tabell_neagsjon <- sannhet_negasjon$tt

tabell_neagsjon


#Erstatter - med 0, og fjerner overflødig kolonne
tabell_neagsjon <- tabell_neagsjon %>%
  mutate(incl = ifelse(incl == "-", 0, incl)) %>%
  mutate(PRI = ifelse(PRI == "-", 0, PRI)) %>%
  mutate(n = NULL)


#Gjør verdiene til numeric
tabell_neagsjon$incl <- as.numeric(tabell_neagsjon$incl)
tabell_neagsjon$PRI <- as.numeric(tabell_neagsjon$PRI)

#Runder verdiene
tabell_neagsjon <- tabell_neagsjon %>%
  mutate(incl = round(incl, digits = 3)) %>%
  mutate(PRI = round(PRI, digits = 3))

#organiserer med incl i synkende rekkefølge
tabell_neagsjon <- tabell_neagsjon %>%
  arrange(desc(incl))


#Lagrer tabellen i stargazer
stargazer(tabell_neagsjon, type = "text", 
          summary = F,
          title = "Sannhetstabell, ~Terror",
          covariate.labels = c("", "Fattigdom", "Militær", "Mangfold",
                               "Polarisering", "Separatisme", "Koloni", "OUT", "Incl", "PRI",
                               "Land"),
          digits = 2, 
          out = "san_tab_negert.html")




#####logisk minimering##########

##Logisk minimering - konservativ løsningsformel
minimize(sannTabell, details = TRUE, outcome = "terror")



##Parsimouis løsning
minimize(sannTabell, include = "?", details = T)

##mellomliggende løsning versjonen som brukes i oppgaven

Intermediate <- minimize(sannTabell, include = "?", dir.exp = "1, 1, 1, 1, 1, 1", details = T)




##############Robusthetstester##############


###Fjerner først Luxembourg og Island

RobustQca <- QcaData[-c(15, 18),]

robust_tabell <- truthTable(RobustQca, outcome = "terror", conditions = "sosialrisk,
                         intervensjon, mangfold, polarisering, separatisme, koloni",
                         complete = T, show.cases = T, incl.cut = 0.75,
                         sort.by = "incl, n+")


robust_tabell

###Både sannhetstabell og løsningsformel er identisk
minimize(robust_tabell, include = "?", dir.exp = "1, 1, 1, 1, 1, 1", details = T)



##Kalibrer betingelsene med ankerpunkt på gjennomsnitt

##Oppretter ett robust datasett med alle casene
RobustQca <- QcaData

mean(qca_2012$koloniN)
RobustQca$koloni <- admisc::recode(qca_2012$koloniN, cuts = "0, 0.9, 10", values = "0, 0.25, 0.75, 1")


mean(qca_2012$asyl_capita)
RobustQca$ASY <- admisc::recode(qca_2012$asyl_capita, cuts = "60, 205, 400", values = "0, 0.25, 0.75, 1")

mean(qca_2012$polsos)
RobustQca$polarisering <- admisc::recode(qca_2012$polsos, cuts = "1.1, 2.1, 3.1", values = "0, 0.25, 0.75, 1")


mean(qca_2012$sosial_risk)
RobustQca$sosialrisk <- admisc::recode(qca_2012$sosial_risk, cuts = "15, 20.8, 30", values = "0, 0.25, 0.75, 1")


mean(qca_2012$etniskMangfold)
RobustQca$MANG <- admisc::recode(qca_2012$etniskMangfold, cuts = "4, 8, 11", values = "0, 0.25, 0.75, 1")



##Lager mangfoldsbetingelsen, på samme måte som tidligere
RobustQca$mangfold <- with(RobustQca, fuzzyor(MANG, ASY))


#Lager sannhetstabell med re-kalibrerte betingelser
rb2 <- truthTable(RobustQca, outcome = "terror", conditions = "sosialrisk,
                         intervensjon, mangfold, polarisering, separatisme, koloni",
           complete = T, show.cases = T, incl.cut = 0.75,
           sort.by = "incl, n+")

#Minimerer tabellen
minimize(rb2, include = "?", dir.exp = "1, 1, 1, 1, 1, 1", details = T)


##Kjører med ny inklusjonsterskel 
rb3 <- truthTable(QcaData, outcome = "terror", conditions = "sosialrisk,
                         intervensjon, mangfold, polarisering, separatisme, koloni",
                  complete = T, show.cases = T, incl.cut = 0.8,
                  sort.by = "incl, n+")

#Minimerer tabellen med ny terskel
minimize(rb3, include = "?", dir.exp = "1, 1, 1, 1, 1, 1", details = T)


