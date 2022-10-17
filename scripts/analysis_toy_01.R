#SENIORS-I Apr 2022 Bericht

#Prep
.packages = c("lubridate", "tidyverse","lessR","epiR","waffle",
              "janitor","cowplot","circlize","chorddiag",
              "patchwork","viridis", "here","rio")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)
master <- rio::import(here("data","master.rds"))
#Neuer master ist beschränkt auf nur die im Jahr 2021

#Standard für Grafiken
theme_clean1 <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
          plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
          panel.background = element_rect(fill = NA, colour = "white"), 
          panel.grid.major = element_blank(),
          axis.ticks = element_blank())
}

#confine dataset to 1 year --> new master file is just 2021


#1. Datenstruktur----
##Tab 1. Verteilung Daten über Messzeitpunkte----
tab1a <- subset(master, subset = ereignis ==2)
tab1b <- tab1a %>% group_by(fall_id) %>% summarise(count = n())
tab1c <- subset(tab1b, subset = count >= 2)
tab1d <- subset(tab1b, subset = count >=3)
tab1e <- subset(tab1a, subset = phasenwechsel == 0)
tab1f <- tab1e %>% group_by(fall_id) %>% summarise(count = n())
tab1g <- subset(tab1a, subset = phasenwechsel == 1)
tab1h <- tab1g %>% group_by(fall_id) %>% summarise(count = n())
tab1i <- subset(tab1h, subset = count >= 2)
tab1j <- subset(tab1h, subset = count >= 3)
numbers <- c(250,66,8,272,94,8,45)
n <- c(405,405,405,405,405,405,405)
tab1k <- data.frame(numbers, n)
tab1k$percent <- round((tab1k$numbers/tab1k$n)*100, 1)

##Abb 1. Anzahl Episoden pro Patient Donut----
abb1 <- subset(master, subset = ereignis == 1)
table(abb1$episode_nr)
episode1 <- rep(1, 372)
episode2 <- rep(2, 29)
episode3 <- rep(3, 4)
abb1 <- c(episode1, episode2, episode3)
abb1 <- data.frame(abb1)
names(abb1)[1] <- "Episoden"
abb1$Episoden <- factor(abb1$Episoden, levels = c(1:3),
                           labels = c("1 Episode","2 Episoden",
                                      "3 Episoden"))
png("abb1.png", 
    width = 6, height = 4, units = 'in', res = 300)
lessR::PieChart(Episoden, data = abb1,
                fill = c("#9ECAE1","#4292C6","#084594"),
                values = "input",
                main = "Anzahl der Episoden pro Patient",
                color = "white",
                lwd = 1.5,
                values_size = 0.85,
                hole = 0.5,
                hole_fill = "#DEEBF7")
dev.off()

##Abb 2. Heatmap n Patienten über Monate----
#Eintritt, Zwischen-IPOS, Austritt auf x-Achse?  
abb2 <- master %>% group_by(month, ereignis) %>%
  summarise(no_pats = length(unique(fall_id)))
abb2 <- abb2[c(1:36),]
abb2$ereignis <- factor(abb2$ereignis, levels=c(1:3),
                        labels=c("Eintritt","Zwischen-Assessment",
                                 "Austritt"))
png("abb2.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = abb2,aes(x = ereignis, y = forcats::fct_rev(month),
                              fill = no_pats))+
  geom_tile()+
  scale_fill_gradient(name = "Anzahl Patienten", low = "#FFFFFF",high = "#084594")+
  labs(title = "Verteilung der Patienten über das Jahr 2021",
       subtitle = "n = 405 Fälle", x="") +
  theme(strip.placement = "outside",
        plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off() 

##Tab 2. Deskriptivstatistik der Liegedauer----
tab2 <- subset(master, subset = ereignis == 1)
psych::describe(tab2$liegedauer)
summary(tab2$liegedauer)

##Abb 3. Verteilung der Verweildauer Boxplot----
abb3 <- subset(master, subset = ereignis == 1)
png("abb3.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb3, aes(x="",y=liegedauer, fill="")) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_manual(values=c("#2171B5")) +
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  geom_jitter(color="lightblue", size=0.4, alpha=0.8) +
  labs(title = "Verteilung der Verweildauer",
       subtitle = "Verweildauer in Tagen",
       y = "Liegedauer in Tagen", x="")+
  theme_clean1()
dev.off()

##Abb 4. Verteilung der Verweildauer aufgeteilt nach Episoden----
abb4 <- subset(master, subset = ereignis == 1)
abb4a <- subset(abb4, subset = episode_nr == 1)
abb4a <- abb4a[,c(3,22)]
abb4b <- subset(abb4, subset = episode_nr == 2)
abb4b <- abb4b[,c(3,22)]
abb4c <- subset(abb4, subset = episode_nr == 3)
abb4c <- abb4c[,c(3,22)]
abb4d <- abb4[,c(3,22)]
abb4a$episode <- 1
abb4b$episode <- 2
abb4c$episode <- 3
abb4d$episode <- 4
abb4 <- rbind(abb4a,abb4b,abb4c,abb4d)
abb4$episode <- factor(abb4$episode, levels = c(4, 1, 2, 3),
                       labels = c("Gesamt (n = 405)",
                                  "1 Episode (n = 372)",
                                  "2 Episoden (n = 29)",
                                  "3 Episoden (n = 4)"))
png("abb4.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb4, aes(x=forcats::fct_rev(episode), y=liegedauer, 
                 fill=episode)) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_manual(values=c("#084594","#BDD7E7","#6BAED6","#2171B5")) +
  geom_jitter(color="lightblue", size=0.4, alpha=0.8) +
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(title = "Verteilung der Verweildauer",
       subtitle = "Verweildauer in Tagen pro Anzahl Episoden",
       y = "Liegedauer in Tagen",x="")+
  theme_clean1()
dev.off()

##Abb 5. Verweildauer Histogramm----
png("abb5.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(subset(abb4, subset = episode != "Gesamt (n = 405)"), 
       aes(x = liegedauer, fill = episode)) + 
  geom_histogram(bins = 50, alpha = 0.75, position = "identity",
                 colour = "white",
                 lwd = 0.2,
                 linetype = 1)+
  scale_fill_manual(values = c("#6BAED6","#2171B5","#084594"))+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  labs(title = "Histogramm der Verweildauer",
       subtitle = "Verweildauer in Tagen pro Anzahl Episoden",
       x = "Liegedauer in Tagen")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
    plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA, colour = "white"),
    axis.title.y = element_blank()
  )
dev.off()

##Abb 6. Verweildauer Balkendiagramm klassiert----
abb6 <- subset(master, subset = ereignis == 1)
abb6 <- abb6[,c(3,5,23)]
abb6a <- abb6 %>%
  group_by(episode_nr, ld_cat) %>%
  summarise(anzahl = n())
abb6b <- abb6 %>%
  group_by(ld_cat) %>%
  summarise(anzahl = n())
abb6b$episode_nr <- 4
abb6 <- rbind(abb6a, abb6b)
abb6$episode_nr <- factor(abb6$episode_nr, levels = c(4,1,2,3),
                          labels = c("Gesamt (n = 405)",
                                     "1 Episode (n = 372)",
                                     "2 Episoden (n = 29)",
                                     "3 Episoden (n = 4)"))
png("abb6.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb6, aes(x = anzahl,y = ld_cat, 
                fill = episode_nr))+
  geom_bar(stat='identity')+
  facet_grid(~episode_nr)+
  scale_fill_manual(values=c("#084594","#BDD7E7", "#6BAED6","#2171B5"))+
  labs(title = "Balkendiagramm der Verweildauer",
       subtitle = "Klassierte Daten pro Anzahl Episoden",
       y = "Liegedauer (klassiert)",x="")+
  theme_clean1()
dev.off()

##Abb 7. Liegedauer von Patienten Heatmap klassiert----
abb7 <- master[,c(3,14,19,23)]
abb7$month <- as.numeric(abb7$month)
abb7$month <- factor(abb7$month)
abb7 <- abb7 %>%
  group_by(ereignis, month, ld_cat) %>%
  summarise(anzahl = n())
abb7$ereignis <- factor(abb7$ereignis, levels = c(1:3),
                        labels = c("Eintritt","Zwischen-IPOS","Austritt"))
abb7 <- na.omit(abb7)
png("abb7.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = abb7, aes(x = month, y = ld_cat,
                        fill = anzahl))+
  geom_tile()+
  facet_grid(~ ereignis, switch = "x")+
  scale_fill_gradient(name = "Anzahl", low = "#c7d9f0",high = "#084594")+
  #scale_y_discrete(limits = rev(levels(as.factor(abb7$ld_cat))))+
  labs(title = "Liegedauer von Patienten",
       subtitle = "Monate und Ereignisse (n = 405 Patienten)") +
  theme_minimal()+
  theme(strip.placement = "outside",
        plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  )
dev.off()

##Abb 8. Anteil gestorbener Patienten parliament----
abb8 <- subset(master, subset = ereignis == 3)
table(abb8$tod)
#286 lebend, 119 gestorben
abb8 <- data.frame("Tod" = c("lebend","verstorben"),
                         "Anteil" = c(286,119))
abb8$Tod <- factor(abb8$Tod)
abb8$Share <- abb8$Anteil / sum(abb8$Anteil)
abb8$ymax <- cumsum(abb8$Share)
abb8$ymin <- c(0, head(abb8$ymax, n= -1))
abb8$labelPosition <- (abb8$ymax + abb8$ymin) / 2
abb8$label <- paste0(abb8$Anteil,"\n",abb8$Tod)
png("abb8.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb8, aes(fill = Tod, ymax = ymax, ymin = ymin, 
                       xmax = 2, xmin = 1)) + 
  geom_rect() + 
  geom_label( x=1.5, aes(y=labelPosition, label=label), 
              color="white",size=3) +
  coord_polar(theta = "y",start=-pi/2) + 
  scale_fill_manual(values = c("#9ecae1","#3182bd"))+
  xlim(c(0,2)) + 
  ylim(c(0,2)) +
  labs(title = "Anzahl gestorbener Patienten",
       subtitle = "n = 405 Patienten") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(c(1,0,0,0), unit = "pt"),
        plot.title = element_text(size = 18, margin = margin(4, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(4, 0, 10, 0), color = "gray"))
dev.off()

#2. Demographie----
##Tab 3. Demografische Tabelle Alter, Geschlecht----
psych::describe(tab2$age)
tabyl(tab2$age_cat)
tabyl(tab2$sex)
##Abb 9. Geschlechterverteilung als Pictogramm----
#eher nicht
##Abb 10. Verteilung Krebs - Nicht-Krebs parliament----
abb10 <- subset(master, subset = ereignis == 1)
table(abb10$hdiagc)
#154 Krebs, 74 Nichtkrebs
abb10a <- tabyl(abb10$hdiagc)
abb10a$new_percent <- round((abb10a$n / 405)*100, 1)
abb10a$`abb10$hdiagc` <- fct_explicit_na(abb10a$`abb10$hdiagc`, na_level = "Fehlend")
abb10a$Share <- abb10a$n / sum(abb10a$n)
abb10a$ymax <- cumsum(abb10a$Share)
abb10a$ymin <- c(0, head(abb10a$ymax, n= -1))
abb10a$labelPosition <- (abb10a$ymax + abb10a$ymin) / 2
abb10a$label <- paste0(abb10a$`abb10$hdiagc`,"\n",abb10a$new_percent,"%")
names(abb10a)[1] <- "diagnose"
png("abb10.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb10a, aes(fill = diagnose, ymax = ymax, ymin = ymin, 
                       xmax = 2, xmin = 1)) + 
  geom_rect() + 
  geom_label( x=1.5, aes(y=labelPosition, label=label), 
              color="white",size=3) +
  coord_polar(theta = "y",start=-pi/2) + 
  scale_fill_manual(values = c("#3182bd","#9ecae1","#939699"))+
  xlim(c(0,2)) + 
  ylim(c(0,2)) +
  labs(title = "Verteilung Krankheitsart",
       subtitle = "Krebs vs. Nicht-Krebserkrankungen (n = 405)") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, margin = margin(4, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(4, 0, 10, 0), color = "gray"))
dev.off()

##Abb 11. Balkendiagramm Krebs----
abb11 <- subset(master, subset = ereignis == 1)
abb11 <- tabyl(abb11$diag_cancer)
names(abb11)[1] <- "diagnose"
abb11$diagnose <- fct_explicit_na(abb11$diagnose, na_level = "Fehlend")
png("abb11.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb11, aes(x = reorder(diagnose, n), y = n)) + 
  geom_col(fill = '#3182bd', width = 0.8) +
  geom_text(aes(label = n), vjust = 0.5, hjust = 1.2, 
            color = "white", size = 4) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "ICD-10 Diagnosen: Krebserkrankungen",
       subtitle = "Bei Einweisung (n = 405)") +
  theme(
    plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0))
  )
dev.off()

##Abb 12. Balkendiagramm Nicht-Krebs----
abb12 <- subset(master, subset = ereignis == 1)
abb12 <- tabyl(abb12$diag_noncancer)
names(abb12)[1] <- "diagnose"
abb12$diagnose <- fct_explicit_na(abb12$diagnose, na_level = "Fehlend")
png("abb12.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb12, aes(x = reorder(diagnose, n), y = n)) + 
  geom_col(fill = '#54278F', width = 0.8) +
  geom_text(aes(label = n), vjust = 0.5, hjust = 1.2, 
            color = "white", size = 4) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "ICD-10 Diagnosen: Nicht-Krebserkrankungen",
       subtitle = "Bei Einweisung (n = 405)") +
  theme(
    plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0))
  )
dev.off()

##Abb 13. Donut Einweisungsgründe----
abb13 <- subset(master, subset = ereignis == 1)
abb13 <- tabyl(abb13$egrundkat)
names(abb13)[1] <- "egrund"
abb13$egrund <- fct_explicit_na(abb13$egrund, na_level = "Fehlend")
abb13 <- abb13[,c(1,2)]
abb13 <- abb13 %>%
  uncount(n)
levels(abb13$egrund)
abb13col <- c("#08519C","#6BAED6","#9ECAE1","#3182BD","#a0bfde","#C6DBEF","#939699")
png("abb13.png", 
    width = 6, height = 4, units = 'in', res = 300)
PieChart(egrund, data = abb13,
         fill = abb13col,
         values = "input",
         main = "Einweisungsgrund (n = 405)",
         color = "white",
         lwd = 1.5,
         values_size = 0.85,
         labels_cex = 0.6,
         hole = 0.5)
dev.off()

#3. Heatmaps----
##Tab 4. Heatmap fehlende Werte per Messzeitpunkt ohne 5----
tab4 <- master[,c(3,24,26,44:60)]
tab4 <- tab4 %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab4 <- tab4 %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 1191 Episoden
tab4$percent <- round(tab4$missings / tab4$n_rows * 100, 1)

#Eintritt
tab4a <- master[,c(3,14,24,26,44:60)]
tab4a <- subset(tab4a, subset = ereignis == 1)
tab4a <- tab4a[,c(1,3:21)]
tab4a <- tab4a %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab4a <- tab4a %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab4a$percent <- round(tab4a$missings / tab4a$n_rows * 100, 1)

#Austritt
tab4b <- master[,c(3,14,24,26,44:60)]
tab4b <- subset(tab4b, subset = ereignis == 3)
tab4b <- tab4b[,c(1,3:21)]
tab4b <- tab4b %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab4b <- tab4b %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab4b$percent <- round(tab4b$missings / tab4b$n_rows * 100, 1)

#Zwischen-IPOS
tab4c <- master[,c(3,14,24,26,44:60)]
tab4c <- subset(tab4c, subset = ereignis == 2)
tab4c <- tab4c[,c(1,3:21)]
tab4c <- tab4c %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab4c <- tab4c %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab4c$percent <- round(tab4c$missings / tab4c$n_rows * 100, 1)

#Bei Phasenwechsel
tab4d <- master[,c(3,14,16,24,26,44:60)]
tab4d <- subset(tab4d, subset = ereignis == 2)
tab4d <- subset(tab4d, subset = phasenwechsel == 1)
tab4d <- tab4d[,c(1,4:22)]
tab4d <- tab4d %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab4d <- tab4d %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 328 Episoden
tab4d$percent <- round(tab4d$missings / tab4d$n_rows * 100, 1)
tab4 <- tab4[,c(1,4)]
tab4a <- tab4a[,c(1,4)]
tab4b <- tab4b[,c(1,4)]
tab4c <- tab4c[,c(1,4)]
tab4d <- tab4d[,c(1,4)]
tab4$ereignis <- 1
tab4a$ereignis <- 2
tab4b$ereignis <- 3
tab4c$ereignis <- 4
tab4d$ereignis <- 5
tab4 <- rbind(tab4, tab4a, tab4b, tab4c, tab4d)
tab4$name <- factor(tab4$name, levels = c("poi","akps","i1s5","i2s5",
                                          "i3s5","i4s5","i5s5","i6s5",
                                          "i7s5","i8s5","i9s5","i10s5",
                                          "i11s5","i12s5","i13s5",
                                          "i14s5","i15s5","i16s5","i17s5"),
                    labels = c("Krankheitsphase","AKPS Funktionsstatus",
                               "IPOS Schmerzen", "IPOS Atemnot",
                               "IPOS Fatigue", "IPOS Übelkeit",
                               "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                               "IPOS Verstopfung", "IPOS Mundtrockenheit",
                               "IPOS Schläfrigkeit", "IPOS Mobilität",
                               "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                               "IPOS Depression", "IPOS Im Frieden",
                               "IPOS Gefühle teilen", "IPOS Information",
                               "IPOS Probleme"))
tab4$ereignis <- factor(tab4$ereignis, levels = c(1:5),
                        labels = c("Alle \n (n = 842)", 
                                   "Eintritt \n (n = 405)", 
                                   "Austritt \n (n = 405)",
                                   "Zwischen-\nIPOS \n (n = 360)",
                                   "Phasen-\nwechsel \n (n = 328)"))
png("tab4.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = tab4,aes(x = ereignis, y = forcats::fct_rev(name)))+
  geom_tile(aes(fill=tab4$percent),colour = "grey60", size=0.6)+
  geom_text(aes(label = percent), size = 3) +
  scale_x_discrete(expand = c(0,0), position = "top")+
  coord_equal(ratio=0.25)+
  scale_fill_gradientn(colours=c("#548235","#A9D08E","#E2EFDA",
                                 "#FCE4D6","#F8CBAD","#F4B084",
                                 "#ED7D31","#C65911"),
                       values=c(0,0.125,0.25,0.375,0.5,0.625,0.75,1),
                       guide="colourbar",
                       name="% Fehlend",limits=c(0,50),
                       breaks=c(0,5,10,15,20,25,30,40), 
                       labels=c(0,5,10,15,20,25,30,40))+
  labs(title = "Heatmap fehlende Werte über Messzeitpunkte",
       subtitle = "5 als fehlend kodiert", x="") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off() 

##Tab 5. Heatmap fehlende Werte per Messzeitpunkt mit 5----
tab5 <- master[,c(3,24,26,27:43)]
tab5 <- tab5 %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab5 <- tab5 %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 1191 Episoden
tab5$percent <- round(tab5$missings / tab5$n_rows * 100, 1)

#Eintritt
tab5a <- master[,c(3,14,24,26,27:43)]
tab5a <- subset(tab5a, subset = ereignis == 1)
tab5a <- tab5a[,c(1,3:21)]
tab5a <- tab5a %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab5a <- tab5a %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab5a$percent <- round(tab5a$missings / tab5a$n_rows * 100, 1)

#Austritt
tab5b <- master[,c(3,14,24,26,27:43)]
tab5b <- subset(tab5b, subset = ereignis == 3)
tab5b <- tab5b[,c(1,3:21)]
tab5b <- tab5b %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab5b <- tab5b %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab5b$percent <- round(tab5b$missings / tab5b$n_rows * 100, 1)

#Zwischen-IPOS
tab5c <- master[,c(3,14,24,26,27:43)]
tab5c <- subset(tab5c, subset = ereignis == 2)
tab5c <- tab5c[,c(1,3:21)]
tab5c <- tab5c %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab5c <- tab5c %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 405 Episoden
tab5c$percent <- round(tab5c$missings / tab5c$n_rows * 100, 1)

#Bei Phasenwechsel
tab5d <- master[,c(3,14,16,24,26,27:43)]
tab5d <- subset(tab5d, subset = ereignis == 2)
tab5d <- subset(tab5d, subset = phasenwechsel == 1)
tab5d <- tab5d[,c(1,4:22)]
tab5d <- tab5d %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab5d <- tab5d %>% group_by(name) %>%
  summarise(missings = sum(is.na(value)),
            n_rows = n())
#master n = 328 Episoden
tab5d$percent <- round(tab5d$missings / tab5d$n_rows * 100, 1)
tab5 <- tab5[,c(1,4)]
tab5a <- tab5a[,c(1,4)]
tab5b <- tab5b[,c(1,4)]
tab5c <- tab5c[,c(1,4)]
tab5d <- tab5d[,c(1,4)]
tab5$ereignis <- 1
tab5a$ereignis <- 2
tab5b$ereignis <- 3
tab5c$ereignis <- 4
tab5d$ereignis <- 5
tab5 <- rbind(tab5, tab5a, tab5b, tab5c, tab5d)
tab5$name <- factor(tab5$name, levels = c("poi","akps","i1","i2",
                                          "i3","i4","i5","i6",
                                          "i7","i8","i9","i10",
                                          "i11","i12","i13",
                                          "i14","i15","i16","i17"),
                    labels = c("Krankheitsphase","AKPS Funktionsstatus",
                               "IPOS Schmerzen", "IPOS Atemnot",
                               "IPOS Fatigue", "IPOS Übelkeit",
                               "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                               "IPOS Verstopfung", "IPOS Mundtrockenheit",
                               "IPOS Schläfrigkeit", "IPOS Mobilität",
                               "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                               "IPOS Depression", "IPOS Im Frieden",
                               "IPOS Gefühle teilen", "IPOS Information",
                               "IPOS Probleme"))
tab5$ereignis <- factor(tab5$ereignis, levels = c(1:5),
                        labels = c("Alle \n (n = 842)", 
                                   "Eintritt \n (n = 405)", 
                                   "Austritt \n (n = 405)",
                                   "Zwischen-\nIPOS \n (n = 360)",
                                   "Phasen-\nwechsel \n (n = 328)"))
png("tab5.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = tab5,aes(x = ereignis, y = forcats::fct_rev(name)))+
  geom_tile(aes(fill=tab5$percent),colour = "grey60", size=0.6)+
  geom_text(aes(label = percent), size = 3) +
  scale_x_discrete(expand = c(0,0), position = "top")+
  coord_equal(ratio=0.25)+
  scale_fill_gradientn(colours=c("#548235","#A9D08E","#E2EFDA",
                                 "#FCE4D6","#F8CBAD","#F4B084",
                                 "#ED7D31","#C65911"),
                       values=c(0,0.125,0.25,0.375,0.5,0.625,0.75,1),
                       guide="colourbar",
                       name="% Fehlend",limits=c(0,50),
                       breaks=c(0,5,10,15,20,25,30,40), 
                       labels=c(0,5,10,15,20,25,30,40))+
  labs(title = "Heatmap fehlende Werte über Messzeitpunkte",
       subtitle = "Nur fehlende Werte als fehlend kodiert", x="") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off()

##Tab 6. Heatmap fehlende Werte per Palliativphase ohne 5----
tab6 <- master[,c(24,26,44:60)]
tab6v <- tab6 %>%
  gather("items", "value", -1)
tab6v$items <- factor(tab6v$items, levels = c("akps","i1s5","i2s5",
                                          "i3s5","i4s5","i5s5","i6s5",
                                          "i7s5","i8s5","i9s5","i10s5",
                                          "i11s5","i12s5","i13s5",
                                          "i14s5","i15s5","i16s5","i17s5"),
                    labels = c("AKPS Funktionsstatus",
                               "IPOS Schmerzen", "IPOS Atemnot",
                               "IPOS Fatigue", "IPOS Übelkeit",
                               "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                               "IPOS Verstopfung", "IPOS Mundtrockenheit",
                               "IPOS Schläfrigkeit", "IPOS Mobilität",
                               "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                               "IPOS Depression", "IPOS Im Frieden",
                               "IPOS Gefühle teilen", "IPOS Information",
                               "IPOS Probleme"))
tab6v <- tab6v %>% group_by(poi, items) %>%
  count(value, .drop = FALSE)
tab6v <- tab6v %>% group_by(poi, items) %>%
  filter(is.na(value))
tab6v <- tab6v[(1:88),]
tab6a <- subset(tab6v, subset = poi == 1) #472
tab6b <- subset(tab6v, subset = poi == 2) #351
tab6c <- subset(tab6v, subset = poi == 3) #117
tab6d <- subset(tab6v, subset = poi == 4) #112
tab6e <- subset(tab6v, subset = poi == 5) #117
tab6a$gesamtn <- 472
tab6a$percent <- round(tab6a$n / tab6a$gesamtn * 100, 1)
tab6b$gesamtn <- 351
tab6b$percent <- round(tab6b$n / tab6b$gesamtn * 100, 1)
tab6c$gesamtn <- 117
tab6c$percent <- round(tab6c$n / tab6c$gesamtn * 100, 1)
tab6d$gesamtn <- 112
tab6d$percent <- round(tab6d$n / tab6d$gesamtn * 100, 1)
tab6e$gesamtn <- 117
tab6e$percent <- round(tab6e$n / tab6e$gesamtn * 100, 1)
tab6d[nrow(tab6d) + 1, ] <- list(4,"AKPS Funktionsstatus",NA,0,112,0.0)
tab6e[nrow(tab6e) + 1, ] <- list(5,"AKPS Funktionsstatus",NA,0,117,0.0)
tab6 <- rbind(tab6a, tab6b, tab6c, tab6d, tab6e)
tab6$poi <- factor(tab6$poi, levels = c(1:5),
                        labels = c("Stabil \n (n = 472)", 
                                   "Instabil \n (n = 351)", 
                                   "Verschlechternd \n (n = 117)",
                                   "Sterbend \n (n = 112)",
                                   "Gestorben \n (n = 117)"))

png("tab6.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = tab6,aes(x = poi, y = forcats::fct_rev(items)))+
  geom_tile(aes(fill=tab6$percent),colour = "grey60", size=0.6)+
  geom_text(aes(label = percent), size = 3) +
  scale_x_discrete(expand = c(0,0), position = "top")+
  coord_equal(ratio=0.25)+
  scale_fill_gradientn(colours=c("#548235","#A9D08E","#E2EFDA",
                                 "#FCE4D6","#F8CBAD","#F4B084",
                                 "#ED7D31","#C65911"),
                       values=c(0,0.125,0.25,0.375,0.5,0.625,0.75,1),
                       guide="colourbar",
                       name="% Fehlend",limits=c(0,50),
                       breaks=c(0,5,10,15,20,25,30,40), 
                       labels=c(0,5,10,15,20,25,30,40))+
  labs(title = "Heatmap fehlende Werte über Phasen",
       subtitle = "5 als fehlend kodiert", x="") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off() 

##Tab 7. Heatmap fehlende Werte per Palliativphase mit 5----
tab7 <- master[,c(24,26,27:43)]
tab7a <- subset(tab7, subset = poi == 1) #472
tab7b <- subset(tab7, subset = poi == 2) #351
tab7c <- subset(tab7, subset = poi == 3) #117
tab7d <- subset(tab7, subset = poi == 4) #112
tab7e <- subset(tab7, subset = poi == 5) #117
tab7a <- tab7a %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab7a <- tab7a %>% group_by(name) %>%
  filter(is.na(value))
tab7a$gesamtn <- 472
tab7a$percent <- round(tab7a$n / tab7a$gesamtn * 100, 1)
tab7b <- tab7b %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab7b <- tab7b %>% group_by(name) %>%
  filter(is.na(value))
tab7b$gesamtn <- 351
tab7b$percent <- round(tab7b$n / tab7b$gesamtn * 100, 1)
#c
tab7c <- tab7c %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab7c <- tab7c %>% group_by(name) %>%
  filter(is.na(value))
tab7c$gesamtn <- 117
tab7c$percent <- round(tab7c$n / tab7c$gesamtn * 100, 1)
#d
tab7d <- tab7d %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab7d <- tab7d %>% group_by(name) %>%
  filter(is.na(value))
tab7d$gesamtn <- 112
tab7d$percent <- round(tab7d$n / tab7d$gesamtn * 100, 1)
#e
tab7e <- tab7e %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
tab7e <- tab7e %>% group_by(name) %>%
  filter(is.na(value))
tab7e$gesamtn <- 117
tab7e$percent <- round(tab7e$n / tab7e$gesamtn * 100, 1)
tab7a$phase <- 1
tab7b$phase <- 2
tab7c$phase <- 3
tab7d$phase <- 4
tab7e$phase <- 5
tab7d[nrow(tab7d) + 1, ] <- list("akps",NA,0,112,0.0,4)
tab7e[nrow(tab7e) + 1, ] <- list("akps",NA,0,117,0.0,5)
tab7e[nrow(tab7e) + 1, ] <- list("i4",NA,0,117,0.0,5)
tab7e[nrow(tab7e) + 1, ] <- list("i5",NA,0,117,0.0,5)
tab7e[nrow(tab7e) + 1, ] <- list("i8",NA,0,117,0.0,5)
tab7e[nrow(tab7e) + 1, ] <- list("i9",NA,0,117,0.0,5)
tab7e[nrow(tab7e) + 1, ] <- list("i10",NA,0,117,0.0,5)
tab7 <- rbind(tab7a, tab7b, tab7c, tab7d, tab7e)
tab7$name <- factor(tab7$name, levels = c("akps","i1","i2",
                                          "i3","i4","i5","i6",
                                          "i7","i8","i9","i10",
                                          "i11","i12","i13",
                                          "i14","i15","i16","i17"),
                    labels = c("AKPS Funktionsstatus",
                               "IPOS Schmerzen", "IPOS Atemnot",
                               "IPOS Fatigue", "IPOS Übelkeit",
                               "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                               "IPOS Verstopfung", "IPOS Mundtrockenheit",
                               "IPOS Schläfrigkeit", "IPOS Mobilität",
                               "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                               "IPOS Depression", "IPOS Im Frieden",
                               "IPOS Gefühle teilen", "IPOS Information",
                               "IPOS Probleme"))
tab7$phase <- factor(tab7$phase, levels = c(1:5),
                     labels = c("Stabil \n (n = 472)", 
                                "Instabil \n (n = 351)", 
                                "Verschlechternd \n (n = 117)",
                                "Sterbend \n (n = 112)",
                                "Gestorben \n (n = 117)"))

png("tab7.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(data = tab7,aes(x = phase, y = forcats::fct_rev(name)))+
  geom_tile(aes(fill=tab7$percent),colour = "grey60", size=0.6)+
  geom_text(aes(label = percent), size = 3) +
  scale_x_discrete(expand = c(0,0), position = "top")+
  coord_equal(ratio=0.25)+
  scale_fill_gradientn(colours=c("#548235","#A9D08E","#E2EFDA",
                                 "#FCE4D6","#F8CBAD","#F4B084",
                                 "#ED7D31","#C65911"),
                       values=c(0,0.125,0.25,0.375,0.5,0.625,0.75,1),
                       guide="colourbar",
                       name="% Fehlend",limits=c(0,50),
                       breaks=c(0,5,10,15,20,25,30,40), 
                       labels=c(0,5,10,15,20,25,30,40))+
  labs(title = "Heatmap fehlende Werte über Phasen",
       subtitle = "Nur fehlende Werte als fehlend kodiert", x="") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off() 

##Tab 8. Heatmap fehlende Werte pro Monat ohne 5----
tab8 <- master[,c(19,24,26,44:60)]
tab8$month<-as.numeric(tab8$month)
tab8 <- tab8 %>%
  gather("items", "value", -1)
tab8$items <- factor(tab8$items, levels = c("poi","akps","i1s5","i2s5",
                                              "i3s5","i4s5","i5s5","i6s5",
                                              "i7s5","i8s5","i9s5","i10s5",
                                              "i11s5","i12s5","i13s5",
                                              "i14s5","i15s5","i16s5","i17s5"),
                      labels = c("Krankheitsphase","AKPS Funktionsstatus",
                                 "IPOS Schmerzen", "IPOS Atemnot",
                                 "IPOS Fatigue", "IPOS Übelkeit",
                                 "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                                 "IPOS Verstopfung", "IPOS Mundtrockenheit",
                                 "IPOS Schläfrigkeit", "IPOS Mobilität",
                                 "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                                 "IPOS Depression", "IPOS Im Frieden",
                                 "IPOS Gefühle teilen", "IPOS Information",
                                 "IPOS Probleme"))
tab8 <- tab8 %>% group_by(month, items) %>%
  count(value, .drop = FALSE)
tab8 <- tab8 %>% group_by(month, items) %>%
  filter(is.na(value))
tab8 <- tab8[(1:210),]
master %>% tabyl(month)
tab8e <- table(tab8$items, tab8$month, useNA = "always")
write.csv2(tab8e, "tab8e.csv")
#1 = 1, 3 = 6, 4 = 7, 5 = 2, 7 = 2
month <- c(1,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,7,7)
items <- c(1,3,5,6,7,10,12,1,3,4,5,7,13,14,1,2,1,2)
n <- rep(0, times=18)
tab8a <- tab8[,c(1,2,4)]
tab8b <- data.frame(month, items, n)
tab8b$items <- factor(tab8b$items, levels = c(1:19),
                      labels = c("Krankheitsphase","AKPS Funktionsstatus",
                                 "IPOS Schmerzen", "IPOS Atemnot",
                                 "IPOS Fatigue", "IPOS Übelkeit",
                                 "IPOS Erbrechen", "IPOS Appetitlosigkeit",
                                 "IPOS Verstopfung", "IPOS Mundtrockenheit",
                                 "IPOS Schläfrigkeit", "IPOS Mobilität",
                                 "IPOS Sorgen/Angst", "IPOS Familie besorgt",
                                 "IPOS Depression", "IPOS Im Frieden",
                                 "IPOS Gefühle teilen", "IPOS Information",
                                 "IPOS Probleme"))
tab8c <- rbind(tab8a, tab8b)
tab8c <- tab8c %>% arrange(month, items)
tab8c$total <- rep(c(137,90,93,105,86,111,70,82,100,105,94,115), each = 19)
tab8c$percent <- round((tab8c$n/tab8c$total)*100, 1)
tab8c$month <- factor(tab8c$month, levels = c(1:12),
                   labels = c("Jan \n (137)", 
                              "Feb \n (90)", 
                              "Mär \n (93)",
                              "Apr \n (105)",
                              "Mai \n (86)",
                              "Jun \n (111)",
                              "Jul \n (70)",
                              "Aug \n (82)",
                              "Sep \n (100)",
                              "Okt \n (105)",
                              "Nov \n (94)",
                              "Dez \n (115)"))
par(mar=c(1,1,1,1))
png("tab8.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(data = tab8c,aes(x = month, y = forcats::fct_rev(items)))+
  geom_tile(aes(fill=tab8c$percent),colour = "grey60", size=0.6)+
  geom_text(aes(label = percent), size = 3) +
  scale_x_discrete(expand = c(0,0), position = "top")+
  scale_fill_gradientn(colours=c("#548235","#A9D08E","#E2EFDA",
                                 "#FCE4D6","#F8CBAD","#F4B084",
                                 "#ED7D31","#C65911"),
                       values=c(0,0.125,0.25,0.375,0.5,0.625,0.75,1),
                       guide="colourbar",
                       name="% Fehlend",limits=c(0,50),
                       breaks=c(0,5,10,15,20,25,30,40), 
                       labels=c(0,5,10,15,20,25,30,40))+
  labs(title = "Heatmap fehlende Werte über Monate",
       subtitle = "5 als fehlend kodiert", x="") +
  theme_minimal()+
  theme(plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )
dev.off() 

##Tab 9. Heatmap fehlende Werte pro Monat mit 5----
#lohnt sich nicht --> weggelassen

#4. Krankheitsphase----
##Tab 10. Verteilung Phasen über Messzeitpunkte
##Abb 14. Stacked bar chart Krankheitsphase pro Messzeitpunkt----
abb14 <- master[,c(3,14,16,25)]
abb14a <- subset(abb14, subset = ereignis == 2)
abb14b <- subset(abb14, subset = ereignis != 2)
abb14a <- subset(abb14, subset = phasenwechsel == 1)
abb14 <- rbind(abb14a, abb14b)
abb14 <- arrange(abb14, fall_id, ereignis)
abb14 <- abb14 %>%
  group_by(ereignis, poif) %>%
  summarise(anzahl = n())
abb14 <- abb14 %>% group_by(ereignis) %>% 
  mutate(freq = anzahl / sum(anzahl))
abb14$poif <- fct_explicit_na(abb14$poif, na_level = "Fehlend")
abb14$ereignis <- factor(abb14$ereignis, levels = c(1:3),
                        labels = c("Eintritt","Phasenwechsel","Austritt"))
png("abb14.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb14, aes(fill=forcats::fct_rev(poif),y=freq, 
                  x=forcats::fct_rev(ereignis))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=c("lightgrey","grey40", "red4","darkorange2",
                             "goldenrod2","darkgreen")) +
  labs(title = "Verteilung der Krankheitsphasen",
       subtitle = "Verteilung über Messzeitpunkte in % (n = 405)",
       y = "", x = "", fill = "Palliativphase")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

##Abb 15. Sankey Veränderung Krankheitsphase pro Patient über MZP

#5. AKPS Funktionsstatus----
##Abb 16. AKPS pro Messzeitpunkt----
abb16 <- master[,c(3,14,16,26)]
abb16a <- subset(abb16, subset = ereignis == 2)
abb16b <- subset(abb16, subset = ereignis != 2)
abb16a <- subset(abb16, subset = phasenwechsel == 1)
abb16 <- rbind(abb16a, abb16b)
abb16 <- arrange(abb16, fall_id, ereignis)
abb16 <- abb16 %>%
  group_by(ereignis, akps) %>%
  summarise(anzahl = n())
abb16 <- na.omit(abb16)
#for counts
abb16c <- master %>% group_by(ereignis, akps) %>%
  mutate(sum = sum(anzahl))
#391,327,405
abb16$ereignis <- factor(abb16$ereignis, levels = c(1:3),
                          labels = c("Eintritt (n = 391)",
                                     "Phasenwechsel (n = 327)",
                                     "Austritt (n = 405)"))
png("abb16.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb16, aes(x = anzahl,y = as.factor(akps), 
                 fill = ereignis))+
  geom_bar(stat = "identity")+
  facet_grid(~ereignis)+
  scale_fill_manual(values = c("darkgreen","darkgreen","darkgreen"))+
  labs(title = "Balkendiagramm AKPS Funktionsstatus",
       subtitle = "Verteilung über die Messzeitpunkte",
       y = "AKPS",x="")+
  theme_clean1()
dev.off()

##Abb 17. AKPS pro Krankheitsphase----
#Für alle Messzeitpunkte - gemittelt
abb17 <- master[,c(3,25,26)]
abb17 <- abb17 %>%
  group_by(poif, akps) %>%
  summarise(anzahl = n())
abb17 <- na.omit(abb17)
#for counts
abb17a <- abb17 %>% group_by(poif) %>%
  mutate(sum = sum(anzahl))
#469,345,116,112,117
abb17$akps <- factor(abb17$akps)
abb17$poif <- as.numeric(abb17$poif)
abb17$poif <- factor(abb17$poif, levels = c(1:5),
                     labels = c("Stabil \n (n = 469)",
                                "Instabil \n (n = 345)",
                                "Verschlechternd \n (n = 116)",
                                "Sterbend \n (n = 112)",
                                "Verstorben \n (n = 117)"))
png("abb17.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb17, aes(x = anzahl,y = akps, 
                  fill = poif))+
  geom_bar(stat = "identity")+
  facet_grid(~poif)+
  scale_fill_manual(values = c("darkgreen","darkgreen","darkgreen",
                               "darkgreen","darkgreen"))+
  labs(title = "Balkendiagramm AKPS Funktionsstatus",
       subtitle = "Verteilung über Krankheitsphase (alle Daten)",
       y = "AKPS",x="")+
  theme_clean1()
dev.off()

#6. IPOS auf Itemebene----
##Abb 18. IPOS stacked bar bei Eintritt----
#5 als Faktorlevel enthalten
#Datensatz formen
abb18 <- subset(master, subset = ereignis == 1)
abb18 <- abb18[,c(27:43)]
abb18 <- abb18 %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb18$name <- factor(abb18$name, levels = c("i1","i2","i3","i4","i5",
                                            "i6","i7","i8","i9","i10",
                                            "i11","i12","i13","i14",
                                            "i15","i16","i17"),
                     labels = c("Schmerzen","Atemnot","Fatigue",
                                "Übelkeit","Erbrechen","Appetitlosigkeit",
                                "Verstopfung","Mundtrockenheit","Schläfrigkeit",
                                "Mobilität","Angst","Familienangst","Depression",
                                "Frieden","Gefühle","Information","Probleme"))
abb18 <- abb18 %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb18$value[is.na(abb18$value)] <- 6
abb18$value <- factor(abb18$value, levels=c(0:6),
                     labels = c("Gar nicht", 
                                "Ein wenig",
                                "Mässig",
                                "Stark",
                                "Extrem stark",
                                "Nicht beurteilbar",
                                "Fehlend"))
levels(abb18$name)
abb18col <- c("#949494","#D3D3D3","#084594", "#2171B5", "#4292CB", 
              "#9ECAE1","#EFF3FF")
png("abb18.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb18, aes(fill=forcats::fct_rev(value), 
                 y=freq, 
                 x=forcats::fct_rev(name))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=abb18col) +
  labs(title = "IPOS Items bei Eintritt",
       subtitle = "(n = 405)", 
       y = "Prozentsatz", x = "IPOS Items", fill = "Antwort")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

##Abb 19. IPOS stacked bar bei Phasenwechsel----
abb19 <- subset(master, subset = ereignis == 2)
abb19 <- subset(abb19, subset = phasenwechsel == 1)
#n = 328
abb19 <- abb19[,c(27:43)]
abb19 <- abb19 %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb19$name <- factor(abb19$name, levels = c("i1","i2","i3","i4","i5",
                                            "i6","i7","i8","i9","i10",
                                            "i11","i12","i13","i14",
                                            "i15","i16","i17"),
                     labels = c("Schmerzen","Atemnot","Fatigue",
                                "Übelkeit","Erbrechen","Appetitlosigkeit",
                                "Verstopfung","Mundtrockenheit","Schläfrigkeit",
                                "Mobilität","Angst","Familienangst","Depression",
                                "Frieden","Gefühle","Information","Probleme"))
abb19 <- abb19 %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb19$value[is.na(abb19$value)] <- 6
abb19$value <- factor(abb19$value, levels=c(0:6),
                      labels = c("Gar nicht", 
                                 "Ein wenig",
                                 "Mässig",
                                 "Stark",
                                 "Extrem stark",
                                 "Nicht beurteilbar",
                                 "Fehlend"))
levels(abb19$name)
levels(abb19$value)
abb19col <- c("#949494","#D3D3D3","#005A32", "#41AB5D", "#74C476", 
              "#A1D99B", "#EDF8E9")
png("abb19.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb19, aes(fill=forcats::fct_rev(value), 
                  y=freq, 
                  x=forcats::fct_rev(name))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=abb19col) +
  labs(title = "IPOS Items bei Phasenwechsel",
       subtitle = "(n = 328)", 
       y = "Prozentsatz", x = "IPOS Items", fill = "Antwort")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

##Abb 20. IPOS stacked bar bei Austritt----
abb20 <- subset(master, subset = ereignis == 3)
#n = 405
abb20 <- abb20[,c(27:43)]
abb20 <- abb20 %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb20$name <- factor(abb20$name, levels = c("i1","i2","i3","i4","i5",
                                            "i6","i7","i8","i9","i10",
                                            "i11","i12","i13","i14",
                                            "i15","i16","i17"),
                     labels = c("Schmerzen","Atemnot","Fatigue",
                                "Übelkeit","Erbrechen","Appetitlosigkeit",
                                "Verstopfung","Mundtrockenheit","Schläfrigkeit",
                                "Mobilität","Angst","Familienangst","Depression",
                                "Frieden","Gefühle","Information","Probleme"))
abb20 <- abb20 %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb20$value[is.na(abb20$value)] <- 6
abb20$value <- factor(abb20$value, levels=c(0:6),
                      labels = c("Gar nicht", 
                                 "Ein wenig",
                                 "Mässig",
                                 "Stark",
                                 "Extrem stark",
                                 "Nicht beurteilbar",
                                 "Fehlend"))
levels(abb20$name)
levels(abb20$value)
abb20col <- c("#949494","#D3D3D3","#54278F", "#756BB1", "#9E9AC8", 
              "#CBC9E2", "#F2F0F7")
png("abb20.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb20, aes(fill=forcats::fct_rev(value), 
                  y=freq, 
                  x=forcats::fct_rev(name))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=abb20col) +
  labs(title = "IPOS Items bei Austritt",
       subtitle = "(n = 405)", 
       y = "Prozentsatz", x = "IPOS Items", fill = "Antwort")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

##Abb 21. IPOS stacked bar bei Tod----
abb21 <- subset(master, subset = ereignis == 3)
abb21 <- subset(abb21, subset = tod == "gestorben")
#n = 119
abb21 <- abb21[,c(27:43)]
abb21 <- abb21 %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb21$name <- factor(abb21$name, levels = c("i1","i2","i3","i4","i5",
                                            "i6","i7","i8","i9","i10",
                                            "i11","i12","i13","i14",
                                            "i15","i16","i17"),
                     labels = c("Schmerzen","Atemnot","Fatigue",
                                "Übelkeit","Erbrechen","Appetitlosigkeit",
                                "Verstopfung","Mundtrockenheit","Schläfrigkeit",
                                "Mobilität","Angst","Familienangst","Depression",
                                "Frieden","Gefühle","Information","Probleme"))
abb21 <- abb21 %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb21$value[is.na(abb21$value)] <- 6
abb21$value <- factor(abb21$value, levels=c(0:6),
                      labels = c("Gar nicht", 
                                 "Ein wenig",
                                 "Mässig",
                                 "Stark",
                                 "Extrem stark",
                                 "Nicht beurteilbar",
                                 "Fehlend"))
levels(abb21$name)
levels(abb21$value)
abb21col <- c("#949494","#D3D3D3","#A63603", "#E6550D", "#FD8D3C", 
              "#FDBE85", "#FEEDDE")
png("abb21.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb21, aes(fill=forcats::fct_rev(value), 
                  y=freq, 
                  x=forcats::fct_rev(name))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=abb21col) +
  labs(title = "IPOS Items bei Tod",
       subtitle = "(n = 119)", 
       y = "Prozentsatz", x = "IPOS Items", fill = "Antwort")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

##Abb 22. IPOS Prävalenz bei Ein- und Austritt----
abb22a <- subset(master, subset = ereignis == 1)
abb22a <- abb22a[,c(27:43)]
abb22a <- abb22a %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb22a <- abb22a %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb22a <- subset(abb22a, subset = value >= 2 & value <= 4)
abb22a <- abb22a %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb22a$ereignis <- 1
abb22b <- subset(master, subset = ereignis == 3)
abb22b <- abb22b[,c(27:43)]
abb22b <- abb22b %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb22b <- abb22b %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb22b <- subset(abb22b, subset = value >= 2 & value <= 4)
abb22b <- abb22b %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb22b$ereignis <- 2
abb22a <- abb22a %>%
  mutate(colbar = ifelse(freq == max(freq), "#63aacf", "0"))
abb22b <- abb22b %>%
  mutate(colbar = ifelse(freq == max(freq), "#8983c9", "#54278F"))
abb22 <- rbind(abb22a,abb22b)
abb22$name <- factor(abb22$name, levels = c("i17","i16","i15","i14","i13",
                                            "i12","i11","i10","i9","i8",
                                            "i7","i6","i5","i4",
                                            "i3","i2","i1"),
                     labels = c("Probleme","Information","Gefühle",
                                "Frieden","Depression", "Familienangst",
                                "Angst","Mobilität","Schläfrigkeit",
                                "Mundtrockenheit","Verstopfung",
                                "Appetitlosigkeit","Erbrechen","Übelkeit",
                                "Fatigue","Atemnot","Schmerzen"))
abb22$ereignis <- factor(abb22$ereignis, levels=c(1,2),
                        labels=c("Eintritt","Austritt"))
abb22$percent <- round((abb22$freq * 100), 0)
View(abb22)
png("abb22.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb22, aes(x = name,y = freq,
                  fill = colbar))+
  geom_bar(stat='identity')+
  geom_text(aes(label = percent), vjust = 0.2, hjust = 1.2, 
            color = "white", size = 3) +
  coord_flip()+
  ylim(0,1)+
  scale_fill_identity()+
  facet_grid(~ereignis)+
  labs(title = "IPOS Prävalenz",
       subtitle = "Mässig bis extrem starke Probleme in % (n = 405)",
       y = "", x="")+
  theme_clean1()+
  theme(axis.text.x = element_blank())
dev.off()

##Abb 23. IPOS Prävalenz pro Krankheitsphase bei Eintritt----
abb23 <- subset(master, subset = ereignis == 1)
abb23a <- subset(abb23, subset = poi == 1) #64
abb23b <- subset(abb23, subset = poi == 2) #266
abb23c <- subset(abb23, subset = poi == 3) #22
abb23d <- subset(abb23, subset = poi == 4) #45
abb23a <- abb23a[,c(44:60)]
abb23b <- abb23b[,c(44:60)]
abb23c <- abb23c[,c(44:60)]
abb23d <- abb23d[,c(44:60)]
abb23a <- abb23a %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb23a <- abb23a %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb23a <- subset(abb23a, subset = value >= 2)
abb23a <- abb23a %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb23a$phase <- 1
abb23a <- abb23a %>%
  mutate(colbar = ifelse(freq == max(freq), "#63aacf", "#084594"))

abb23b <- abb23b %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb23b <- abb23b %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb23b <- subset(abb23b, subset = value >= 2)
abb23b <- abb23b %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb23b$phase <- 2
abb23b <- abb23b %>%
  mutate(colbar = ifelse(freq == max(freq), "#63aacf", "#084594"))

abb23c <- abb23c %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb23c <- abb23c %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb23c <- subset(abb23c, subset = value >= 2)
abb23c <- abb23c %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb23c$phase <- 3
abb23c <- abb23c %>%
  mutate(colbar = ifelse(freq == max(freq), "#63aacf", "#084594"))
abb23c[11,5] <- "#63aacf"

abb23d <- abb23d %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb23d <- abb23d %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb23d <- subset(abb23d, subset = value >= 2)
abb23d <- abb23d %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb23d$phase <- 4
abb23d <- abb23d %>%
  mutate(colbar = ifelse(freq == max(freq), "#63aacf", "#084594"))

abb23 <- rbind(abb23a,abb23b,abb23c,abb23d)
abb23$name <- factor(abb23$name, levels = c("i17s5","i16s5","i15s5","i14s5","i13s5",
                                            "i12s5","i11s5","i10s5","i9s5","i8s5",
                                            "i7s5","i6s5","i5s5","i4s5",
                                            "i3s5","i2s5","i1s5"),
                     labels = c("Probleme","Information","Gefühle",
                                "Frieden","Depression", "Familienangst",
                                "Angst","Mobilität","Schläfrigkeit",
                                "Mundtrockenheit","Verstopfung",
                                "Appetitlosigkeit","Erbrechen","Übelkeit",
                                "Fatigue","Atemnot","Schmerzen"))
abb23$phase <- factor(abb23$phase, levels=c(1:4),
                         labels=c("Stabil (n = 64)",
                                  "Instabil (n = 266)",
                                  "Verschlechternd \n (n = 22)",
                                  "Sterbend (n = 45)"))
abb23$percent <- round((abb23$freq * 100), 0)
png("abb23.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb23, aes(x = name,y = freq,
                  fill = colbar))+
  geom_bar(stat='identity')+
  geom_text(aes(label = percent), vjust = 0.2, hjust = 1.2, 
            color = "white", size = 3) +
  coord_flip()+
  ylim(0,1)+
  scale_fill_identity()+
  facet_grid(~phase)+
  labs(title = "IPOS Prävalenz bei Eintritt über Krankheitsphasen",
       subtitle = "Mässig bis extrem starke Probleme in % (n = 405)",
       y = "", x="")+
  theme_clean1()+
  theme(axis.text.x = element_blank())
dev.off()

##Abb 24. IPoS Prävalenz pro Krankheitsphase bei Austritt----
abb24 <- subset(master, subset = ereignis == 3)
abb24a <- subset(abb24, subset = poi == 1) #214
abb24b <- subset(abb24, subset = poi == 2) #23
abb24c <- subset(abb24, subset = poi == 3) #32
abb24d <- subset(abb24, subset = poi == 4) #9
abb24a <- abb24a[,c(44:60)]
abb24b <- abb24b[,c(44:60)]
abb24c <- abb24c[,c(44:60)]
abb24d <- abb24d[,c(44:60)]
abb24a <- abb24a %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb24a <- abb24a %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb24a <- subset(abb24a, subset = value >= 2)
abb24a <- abb24a %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb24a$phase <- 1
abb24a <- abb24a %>%
  mutate(colbar = ifelse(freq == max(freq), "#8983c9", "#54278F"))

abb24b <- abb24b %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb24b <- abb24b %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb24b <- subset(abb24b, subset = value >= 2)
abb24b <- abb24b %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb24b$phase <- 2
abb24b <- abb24b %>%
  mutate(colbar = ifelse(freq == max(freq), "#8983c9", "#54278F"))

abb24c <- abb24c %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb24c <- abb24c %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb24c <- subset(abb24c, subset = value >= 2)
abb24c <- abb24c %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb24c$phase <- 3
abb24c <- abb24c %>%
  mutate(colbar = ifelse(freq == max(freq), "#8983c9", "#54278F"))

abb24d <- abb24d %>%
  tidyr::pivot_longer(cols = contains("i")) %>%
  count(name, value, .drop = FALSE)
abb24d <- abb24d %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb24d <- subset(abb24d, subset = value >= 2)
abb24d <- abb24d %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb24d$phase <- 4
abb24d <- abb24d %>%
  mutate(colbar = ifelse(freq == max(freq), "#8983c9", "#54278F"))

abb24 <- rbind(abb24a,abb24b,abb24c,abb24d)
abb24$name <- factor(abb24$name, levels = c("i17s5","i16s5","i15s5","i14s5","i13s5",
                                            "i12s5","i11s5","i10s5","i9s5","i8s5",
                                            "i7s5","i6s5","i5s5","i4s5",
                                            "i3s5","i2s5","i1s5"),
                     labels = c("Probleme","Information","Gefühle",
                                "Frieden","Depression", "Familienangst",
                                "Angst","Mobilität","Schläfrigkeit",
                                "Mundtrockenheit","Verstopfung",
                                "Appetitlosigkeit","Erbrechen","Übelkeit",
                                "Fatigue","Atemnot","Schmerzen"))
abb24$phase <- factor(abb24$phase, levels=c(1:4),
                      labels=c("Stabil (n = 214)",
                               "Instabil (n = 23)",
                               "Verschlechternd \n (n = 32)",
                               "Sterbend (n = 9)"))
abb24$percent <- round((abb24$freq * 100), 0)
png("abb24.png", 
    width = 8, height = 6, units = 'in', res = 300)
ggplot(abb24, aes(x = name,y = freq,
                  fill = colbar))+
  geom_bar(stat='identity')+
  geom_text(aes(label = percent), vjust = 0.2, hjust = 1.2, 
            color = "white", size = 3) +
  coord_flip()+
  ylim(0,1)+
  scale_fill_identity()+
  facet_grid(~phase)+
  labs(title = "IPOS Prävalenz bei Austritt über Krankheitsphasen",
       subtitle = "Mässig bis extrem starke Probleme in % (n = 405)",
       y = "", x="")+
  theme_clean1()+
  theme(axis.text.x = element_blank())
dev.off()

##Abb 25. Heatmap Prävalenz pro Monat (x-Achse) für Items----
#Für alle Messzeitpunkte, nur halt über Monate
#Grossen Datensatz nehmen, aber auf die Items beschränken
abb25 <- master[,c(19,44:60)]
abb25 <- abb25 %>%
  gather("items", "value", -1)
abb25 <- subset(abb25, subset = value >= 2)
abb25$items <- factor(abb25$items, levels = c("i17s5","i16s5","i15s5","i14s5","i13s5",
                                                "i12s5","i11s5","i10s5","i9s5","i8s5",
                                                "i7s5","i6s5","i5s5","i4s5",
                                                "i3s5","i2s5","i1s5"),
                       labels = c("Probleme","Information","Gefühle",
                                  "Frieden","Depression", "Familienangst",
                                  "Angst","Mobilität","Schläfrigkeit",
                                  "Mundtrockenheit","Verstopfung",
                                  "Appetitlosigkeit","Erbrechen","Übelkeit",
                                  "Fatigue","Atemnot","Schmerzen"))
abb25 <- abb25 %>% 
  group_by(month, items) %>%
  summarise(n_rows = n())
abb25 <- na.omit(abb25)

#eventuell doch Raten berechnen?
#Prävalenz durch die Anzahl der Patienten pro Monat
abb25a <- master %>% group_by(month) %>%
  summarise(n_count = n())
abb25a <- na.omit(abb25a)
abb25 <- abb25 %>% 
  left_join(abb25a, by = c("month" = "month"))
abb25$prev <- round(abb25$n_rows / abb25$n_count * 100, 1)

abb25$month <- as.numeric(abb25$month)
abb25$month <- factor(abb25$month)
png("abb25.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb25, aes(x = month, y = items,
                        fill = prev))+
  geom_tile()+
  scale_fill_gradient(name = "%", low = "#c7d9f0",high = "#084594")+
  labs(title = "IPOS Prävalenz über Monate",
       subtitle = "Alle Messzeitpunkte für alle mit mind. mässigen Problemen") +
  theme_minimal()+
  theme(strip.placement = "outside",
        plot.title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  )
dev.off()

#7. Verteilung der Schmerzen im Verlauf----
##Abb 26. stacked bar Schmerzen über Messzeitpunkte----
abb26 <- master[,c(3,14,15,16,44)]
abb26a <- subset(abb26, subset = ereignis == 1)
abb26b <- subset(abb26, subset = ereignis == 2)
abb26b <- subset(abb26b, subset = phasenwechsel == 1)
#n = 328
abb26c <- subset(abb26, subset = ereignis == 3)
abb26d <- subset(abb26c, subset = tod == "lebend")
#n =  286 lebend
abb26e <- subset(abb26c, subset = tod == "gestorben")
#n = 119
abb26a$mzp <- 1
abb26b$mzp <- 2
abb26d$mzp <- 3
abb26e$mzp <- 4
abb26 <- rbind(abb26a,abb26b,abb26d,abb26e)
abb26 <- abb26 %>%
  group_by(mzp, i1s5) %>%
  summarise(anzahl = n())
abb26 <- abb26 %>% group_by(mzp) %>% 
  mutate(freq = anzahl / sum(anzahl))
abb26$mzp <- factor(abb26$mzp, levels=c(1:4),
                    labels = c("Eintritt \n  (n = 405)",
                               "Phasenwechsel \n (n = 328)",
                               "Austritt \n (n = 286)",
                               "Tod \n (n = 119)"))
abb26$i1s5 <- factor(abb26$i1s5, levels = c(0:4),
                     labels = c("Gar nicht", "Ein wenig",
                                "Mässig", "Stark","Extrem stark"))
abb26$i1s5 <- fct_explicit_na(abb26$i1s5, na_level = "Fehlend")
abb26col <- c("#D3D3D3","#084594", "#2171B5", "#4292CB", 
              "#9ECAE1","#EFF3FF")
png("abb26.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb26, aes(fill=forcats::fct_rev(i1s5),y=freq, 
                  x=forcats::fct_rev(mzp))) + 
  geom_bar(position="fill", stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=abb26col) +
  labs(title = "Verteilung von IPOS Schmerzen",
       subtitle = "Verteilung über Messzeitpunkte in %",
       y = "", x = "", fill = "Antwort")+
  theme_clean1()+
  theme(legend.position = "right")
dev.off()

#8. Längsschnittanalyse IPOS----
##Abb 27. Über MZP lebend vs verstorben Total IPOS----
abb27 <- master[,c(3,14,15,65:68)]
abb27$ereignis <- factor(abb27$ereignis, levels = c(1:3),
                         labels = c("Eintritt","Zwischen-IPOS",
                                    "Austritt"))
abb27$tod <- as.numeric(abb27$tod)
abb27$tod <- factor(abb27$tod, levels=c(1,2),
                    labels = c("Lebend (n=286)","Verstorben (n=119)"))
png("abb27.png", 
    width = 6, height = 5, units = 'in', res = 300)
ggplot(abb27, aes(x=ereignis, y=total, group=fall_id, 
                   color = tod)) +
  geom_line(alpha = 0.5)+
  geom_point()+
  scale_colour_manual(values = c("#756bb1","#e6550d"))+
  ylim(0,70)+
  facet_grid(~tod)+
  labs(title = "Veränderungen des IPOS Gesamtwerts",
       subtitle = "Über Messzeitpunkte getrennt nach Status",
       y = "Gesamt IPOS", x = "")+
  theme_clean1()
dev.off()

##Abb 28. Über MZP lebend vs verstorben Symptomskala----
png("abb28.png", 
    width = 6, height = 5, units = 'in', res = 300)
ggplot(abb27, aes(x=ereignis, y=symptom, group=fall_id, 
                  color = tod)) +
  geom_line(alpha = 0.5)+
  geom_point()+
  scale_colour_manual(values = c("#756bb1","#e6550d"))+
  ylim(0,40)+
  facet_grid(~tod)+
  labs(title = "Veränderungen der IPOS Symptomskala",
       subtitle = "Über Messzeitpunkte getrennt nach Status",
       y = "Symptomskala", x = "")+
  theme_clean1()
dev.off()

##Abb 29. Über MZP lebend vs verstorben Emotion----
png("abb29.png", 
    width = 6, height = 5, units = 'in', res = 300)
ggplot(abb27, aes(x=ereignis, y=emotion, group=fall_id, 
                  color = tod)) +
  geom_line(alpha = 0.5)+
  geom_point()+
  scale_colour_manual(values = c("#756bb1","#e6550d"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+
  facet_grid(~tod)+
  labs(title = "Veränderungen der IPOS Emotionsskala",
       subtitle = "Über Messzeitpunkte getrennt nach Status",
       y = "Emotionsskala", x = "")+
  theme_clean1()
dev.off()

##Abb 30. Über MZP lebend vs verstorben Praktisch----
png("abb30.png", 
    width = 6, height = 5, units = 'in', res = 300)
ggplot(abb27, aes(x=ereignis, y=praktisch, group=fall_id, 
                  color = tod)) +
  geom_line(alpha = 0.5)+
  geom_point()+
  scale_colour_manual(values = c("#756bb1","#e6550d"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12))+
  facet_grid(~tod)+
  labs(title = "Veränderungen der IPOS Qualitätsskala",
       subtitle = "Über Messzeitpunkte getrennt nach Status",
       y = "Kommunikations- und Qualitätsskala", x = "")+
  theme_clean1()
dev.off()

##Abb 31. Veränderungen individueller Werte Total, nach poi-----
abb31 <- master[,c(3,14,25,65:68)]
#Für jede Person neue Variable schaffen: letzte Krankheitsphase!
abb31a <- subset(abb31, subset = ereignis == 3)
abb31a$last_phase <- abb31a$poif
abb31a <- abb31a[,c(1,8)]
abb31 <- abb31 %>% 
  left_join(abb31a, by = c("fall_id" = "fall_id"))
abb31$ereignis <- factor(abb31$ereignis, levels = c(1:3),
                         labels = c("Eintritt","Zwischen-IPOS",
                                    "Austritt"))
abb31b <- abb31a %>% tabyl(last_phase)
abb31$last_phase <- as.numeric(abb31$last_phase)
abb31$last_phase <- factor(abb31$last_phase, levels=c(1:5),
                           labels = c("Stabil (n = 214)",
                                      "Instabil (n = 23)",
                                      "Verschlechternd (n = 32)",
                                      "Sterbend (n = 9)",
                                      "Verstorben (n = 116)"))
abb31$last_phase <- fct_explicit_na(abb31$last_phase, 
                                    na_level = "Fehlend (n = 11)")
png("abb31.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb31, aes(ereignis,total, color=last_phase))+
  geom_boxplot() +
  geom_point(alpha = 0.2)+ 
  geom_line(aes(group = fall_id),alpha=0.2) +
  facet_wrap(~last_phase, ncol = 3)+
  scale_color_manual(values=c("darkgreen","goldenrod2","darkorange2",
                              "red4","grey40","grey20"))+
  labs(title = "Individueller Verlauf IPOS Gesamtskala",
       subtitle = "Aufgeschlüsselt nach letzter Krankheitsphase",
       y="Gesamt-IPOS",x="") +
  theme_clean1()
dev.off()

##Abb 32. Veränderungen individueller Werte Symptom, nach poi----
png("abb32.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb31, aes(ereignis,symptom, color=last_phase))+
  geom_boxplot() +
  geom_point(alpha = 0.2)+ 
  geom_line(aes(group = fall_id),alpha=0.2) +
  facet_wrap(~last_phase, ncol = 3)+
  scale_color_manual(values=c("darkgreen","goldenrod2","darkorange2",
                              "red4","grey40","grey20"))+
  labs(title = "Individueller Verlauf IPOS Symptomskala",
       subtitle = "Aufgeschlüsselt nach letzter Krankheitsphase",
       y="Symptomskala",x="") +
  theme_clean1()
dev.off()

##Abb 33. Veränderungen individueller Werte Emotion, nach poi----
png("abb33.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb31, aes(ereignis,emotion, color=last_phase))+
  geom_boxplot() +
  geom_point(alpha = 0.2)+ 
  geom_line(aes(group = fall_id),alpha=0.2) +
  facet_wrap(~last_phase, ncol = 3)+
  scale_color_manual(values=c("darkgreen","goldenrod2","darkorange2",
                              "red4","grey40","grey20"))+
  labs(title = "Individueller Verlauf IPOS Emotionsskala",
       subtitle = "Aufgeschlüsselt nach letzter Krankheitsphase",
       y="Emotionsskala",x="") +
  theme_clean1()
dev.off()

##Abb 34. Veränderungen individueller Werte Praktisch, nach poi----
png("abb34.png", 
    width = 8, height = 5, units = 'in', res = 300)
ggplot(abb31, aes(ereignis,praktisch, color=last_phase))+
  geom_boxplot() +
  geom_point(alpha = 0.2)+ 
  geom_line(aes(group = fall_id),alpha=0.2) +
  facet_wrap(~last_phase, ncol = 3)+
  scale_color_manual(values=c("darkgreen","goldenrod2","darkorange2",
                              "red4","grey40","grey20"))+
  labs(title = "Individueller Verlauf IPOS Qualitätsskala",
       subtitle = "Aufgeschlüsselt nach letzter Krankheitsphase",
       y="Kommunikations- und Qualitätsskala",x="") +
  theme_clean1()
dev.off()

##Abb 35. Längsschnitt pro Item und facet Veränderung Verb-keineV-Verschl Schmerz----
#Gruppierungsvariable schaffen im Datensatz pro Item
abb35 <- master %>% 
  group_by(fall_id) %>% 
  arrange(fall_id, date)
abb35 <- abb35[,c(3,14,16,44:60,65:68)]
abb35 <- abb35 %>% group_by(fall_id) %>%
  mutate(
    diff_i1 = last(i1s5) - first(i1s5),
    diff_i2 = last(i2s5) - first(i2s5),
    diff_i3 = last(i3s5) - first(i3s5),
    diff_i4 = last(i4s5) - first(i4s5),
    diff_i5 = last(i5s5) - first(i5s5),
    diff_i6 = last(i6s5) - first(i6s5),
    diff_i7 = last(i7s5) - first(i7s5),
    diff_i8 = last(i8s5) - first(i8s5),
    diff_i9 = last(i9s5) - first(i9s5),
    diff_i10 = last(i10s5) - first(i10s5),
    diff_i11 = last(i11s5) - first(i11s5),
    diff_i12 = last(i12s5) - first(i12s5),
    diff_i13 = last(i13s5) - first(i13s5),
    diff_i14 = last(i14s5) - first(i14s5),
    diff_i15 = last(i15s5) - first(i15s5),
    diff_i16 = last(i16s5) - first(i16s5),
    diff_i17 = last(i17s5) - first(i17s5)
  )
abb35$statusi1 <- ifelse(abb35$diff_i1 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi2 <- ifelse(abb35$diff_i2 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi3 <- ifelse(abb35$diff_i3 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi4 <- ifelse(abb35$diff_i4 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi5 <- ifelse(abb35$diff_i5 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi6 <- ifelse(abb35$diff_i6 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi7 <- ifelse(abb35$diff_i7 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi8 <- ifelse(abb35$diff_i8 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi9 <- ifelse(abb35$diff_i9 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi10 <- ifelse(abb35$diff_i10 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi11 <- ifelse(abb35$diff_i11 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi12 <- ifelse(abb35$diff_i12 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi13 <- ifelse(abb35$diff_i13 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi14 <- ifelse(abb35$diff_i14 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi15 <- ifelse(abb35$diff_i15 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi16 <- ifelse(abb35$diff_i16 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35$statusi17 <- ifelse(abb35$diff_i17 >=1, 1,
                         ifelse(abb35$diff_i1 <0, 3, 2))
abb35 <- abb35 %>%
  filter(ereignis == 1 | ereignis == 3 | (ereignis == 2 & phasenwechsel == 1))

#Schmerzen Item i1
abb35i1 <- abb35[complete.cases(abb35[, c(4,42)]), ]
abb35i1 %>% filter (ereignis == 1) %>% tabyl(statusi1)
#1=55, 2 = 176, 3 = 165
abb35i1$statusi1 <- factor(abb35i1$statusi1, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 55)",
                                      "Keine \n Veränderung \n (n = 176)",
                                      "Verbesserung \n (n = 165)"))
abb35i1$ereignis <- factor(abb35i1$ereignis, levels=c(1:3),
                             labels = c("Eintritt","Phasenwechsel",
                                        "Austritt"))
png("abb35.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i1, aes(ereignis,i1s5, color=statusi1, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi1), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Schmerzen",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Schmerzen")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 36. Längsschnitt i2----
#Atemnot Item i2
abb35i2 <- abb35[complete.cases(abb35[, c(5,43)]), ]
abb35i2 %>% filter (ereignis == 1) %>% tabyl(statusi2)
#1=48, 2 = 198, 3 = 140
abb35i2$statusi2 <- factor(abb35i2$statusi2, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 48)",
                                      "Keine \n Veränderung \n (n = 198)",
                                      "Verbesserung \n (n = 140)"))
abb35i2$ereignis <- factor(abb35i2$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb36.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i2, aes(ereignis,i2s5, color=statusi2, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi2), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Atemnot",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Atemnot")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 37. Längsschnitt i3----
#Fatigue Item i3
abb35i3 <- abb35[complete.cases(abb35[, c(6,44)]), ]
abb35i3 %>% filter (ereignis == 1) %>% tabyl(statusi3)
#1=64, 2 = 180, 3 = 134
abb35i3$statusi3 <- factor(abb35i3$statusi3, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 64)",
                                      "Keine \n Veränderung \n (n = 180)",
                                      "Verbesserung \n (n = 134)"))
abb35i3$ereignis <- factor(abb35i3$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb37.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i3, aes(ereignis,i3s5, color=statusi3, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi3), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Fatigue",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Fatigue")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 38. Längsschnitt i4----
#Übelkeit Item i4
abb35i4 <- abb35[complete.cases(abb35[, c(7,45)]), ]
abb35i4 %>% filter (ereignis == 1) %>% tabyl(statusi4)
#1=31, 2 = 198, 3 = 153
abb35i4$statusi4 <- factor(abb35i4$statusi4, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 31)",
                                      "Keine \n Veränderung \n (n = 198)",
                                      "Verbesserung \n (n = 153)"))
abb35i4$ereignis <- factor(abb35i4$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb38.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i4, aes(ereignis,i4s5, color=statusi4, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi4), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Übelkeit",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Übelkeit")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 39. Längsschnitt i5----
#Erbrechen Item i5
abb35i5 <- abb35[complete.cases(abb35[, c(8,46)]), ]
abb35i5 %>% filter (ereignis == 1) %>% tabyl(statusi5)
#1=19, 2 = 216, 3 = 155
abb35i5$statusi5 <- factor(abb35i5$statusi5, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 19)",
                                      "Keine \n Veränderung \n (n = 216)",
                                      "Verbesserung \n (n = 155)"))
abb35i5$ereignis <- factor(abb35i5$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb39.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i5, aes(ereignis,i5s5, color=statusi5, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi5), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Erbrechen",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Erbrechen")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 40. Längsschnitt i6----
#Appetitlosigkeit Item i6
abb35i6 <- abb35[complete.cases(abb35[, c(9,47)]), ]
abb35i6 %>% filter (ereignis == 1) %>% tabyl(statusi6)
#1=44, 2 = 182, 3 = 144
abb35i6$statusi6 <- factor(abb35i6$statusi6, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 44)",
                                      "Keine \n Veränderung \n (n = 182)",
                                      "Verbesserung \n (n = 144)"))
abb35i6$ereignis <- factor(abb35i6$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb40.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i6, aes(ereignis,i6s5, color=statusi6, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi6), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Appetitlosigkeit",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Appetitlosigkeit")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 41. Längsschnitt i7----
#Verstopfung Item i7
abb35i7 <- abb35[complete.cases(abb35[, c(10,48)]), ]
abb35i7 %>% filter (ereignis == 1) %>% tabyl(statusi7)
#1=58, 2 = 174, 3 = 131
abb35i7$statusi7 <- factor(abb35i7$statusi7, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 58)",
                                      "Keine \n Veränderung \n (n = 174)",
                                      "Verbesserung \n (n = 131)"))
abb35i7$ereignis <- factor(abb35i7$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb41.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i7, aes(ereignis,i7s5, color=statusi7, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi7), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Verstopfung",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Verstopfung")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 42. Längsschnitt i8----
#Mundtrockenheit Item i8
abb35i8 <- abb35[complete.cases(abb35[, c(11,49)]), ]
abb35i8 %>% filter (ereignis == 1) %>% tabyl(statusi8)
#1=65, 2 = 180, 3 = 137
abb35i8$statusi8 <- factor(abb35i8$statusi8, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 65)",
                                      "Keine \n Veränderung \n (n = 180)",
                                      "Verbesserung \n (n = 137)"))
abb35i8$ereignis <- factor(abb35i8$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb42.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i8, aes(ereignis,i8s5, color=statusi8, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi8), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Mundtrockenheit",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Mundtrockenheit")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 43. Längsschnitt i9----
#Schläfrigkeit Item i9
abb35i9 <- abb35[complete.cases(abb35[, c(12,50)]), ]
abb35i9 %>% filter (ereignis == 1) %>% tabyl(statusi9)
#1=66, 2 = 183, 3 = 129
abb35i9$statusi9 <- factor(abb35i9$statusi9, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 66)",
                                      "Keine \n Veränderung \n (n = 183)",
                                      "Verbesserung \n (n = 129)"))
abb35i9$ereignis <- factor(abb35i9$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb43.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i9, aes(ereignis,i9s5, color=statusi9, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi9), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Schläfrigkeit",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Schläfrigkeit")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 44. Längsschnitt i10----
#Eingeschränkte Mobilität Item i10
abb35i10 <- abb35[complete.cases(abb35[, c(13,51)]), ]
abb35i10 %>% filter (ereignis == 1) %>% tabyl(statusi10)
#1=60, 2 = 181, 3 = 139
abb35i10$statusi10 <- factor(abb35i10$statusi10, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 60)",
                                      "Keine \n Veränderung \n (n = 181)",
                                      "Verbesserung \n (n = 139)"))
abb35i10$ereignis <- factor(abb35i10$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb44.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i10, aes(ereignis,i10s5, color=statusi10, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi10), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Eingeschränkte Mobilität",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Mobilität")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 45. Längsschnitt i11----
#Patientenangst Item i11
abb35i11 <- abb35[complete.cases(abb35[, c(14,52)]), ]
abb35i11 %>% filter (ereignis == 1) %>% tabyl(statusi11)
#1=60, 2 = 153, 3 = 128
abb35i11$statusi11 <- factor(abb35i11$statusi11, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 60)",
                                      "Keine \n Veränderung \n (n = 153)",
                                      "Verbesserung \n (n = 128)"))
abb35i11$ereignis <- factor(abb35i11$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb45.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i11, aes(ereignis,i11s5, color=statusi11, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi11), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Patientenangst",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Angst")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 46. Längsschnitt i12----
#Familienangst Item i12
abb35i12 <- abb35[complete.cases(abb35[, c(15,53)]), ]
abb35i12 %>% filter (ereignis == 1) %>% tabyl(statusi12)
#1=55, 2 = 166, 3 = 119
abb35i12$statusi12 <- factor(abb35i12$statusi12, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 55)",
                                      "Keine \n Veränderung \n (n = 166)",
                                      "Verbesserung \n (n = 119)"))
abb35i12$ereignis <- factor(abb35i12$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb46.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i12, aes(ereignis,i12s5, color=statusi12, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi12), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Familienangst",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Familienangst")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 47. Längsschnitt i13----
#Depression selbst Item i13
abb35i13 <- abb35[complete.cases(abb35[, c(16,54)]), ]
abb35i13 %>% filter (ereignis == 1) %>% tabyl(statusi13)
#1=70, 2 = 150, 3 = 125
abb35i13$statusi13 <- factor(abb35i13$statusi13, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 70)",
                                      "Keine \n Veränderung \n (n = 150)",
                                      "Verbesserung \n (n = 125)"))
abb35i13$ereignis <- factor(abb35i13$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb47.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i13, aes(ereignis,i13s5, color=statusi13, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi13), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Depression",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Depression")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 48. Längsschnitt i14----
#Nicht im Frieden mit sich selbst Item i14
abb35i14 <- abb35[complete.cases(abb35[, c(17,55)]), ]
abb35i14 %>% filter (ereignis == 1) %>% tabyl(statusi14)
#1=48, 2 = 160, 3 = 115
abb35i14$statusi14 <- factor(abb35i14$statusi14, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 48)",
                                      "Keine \n Veränderung \n (n = 160)",
                                      "Verbesserung \n (n = 115)"))
abb35i14$ereignis <- factor(abb35i14$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb48.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i14, aes(ereignis,i14s5, color=statusi14, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi14), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Frieden",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Nicht im Frieden mit sich selbst")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 49. Längsschnitt i15----
#Gefühle teilen Item i15
abb35i15 <- abb35[complete.cases(abb35[, c(18,56)]), ]
abb35i15 %>% filter (ereignis == 1) %>% tabyl(statusi15)
#1=77, 2 = 135, 3 = 98
abb35i15$statusi15 <- factor(abb35i15$statusi15, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 77)",
                                      "Keine \n Veränderung \n (n = 135)",
                                      "Verbesserung \n (n = 98)"))
abb35i15$ereignis <- factor(abb35i15$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb49.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i15, aes(ereignis,i15s5, color=statusi15, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi15), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Gefühle teilen",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Gefühle teilen")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 50. Längsschnitt i16----
#Information Item i16
abb35i16 <- abb35[complete.cases(abb35[, c(19,57)]), ]
abb35i16 %>% filter (ereignis == 1) %>% tabyl(statusi16)
#1=39, 2 = 158, 3 = 127
abb35i16$statusi16 <- factor(abb35i16$statusi16, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 39)",
                                      "Keine \n Veränderung \n (n = 158)",
                                      "Verbesserung \n (n = 127)"))
abb35i16$ereignis <- factor(abb35i16$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb50.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i16, aes(ereignis,i16s5, color=statusi16, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi16), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Information",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Information")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

##Abb 51. Längsschnitt i17----
#Probleme Item i17
abb35i17 <- abb35[complete.cases(abb35[, c(20,58)]), ]
abb35i17 %>% filter (ereignis == 1) %>% tabyl(statusi17)
#1=43, 2 = 158, 3 = 103
abb35i17$statusi17 <- factor(abb35i17$statusi17, levels=c(1:3),
                           labels = c("Verschlechterung \n (n = 43)",
                                      "Keine \n Veränderung \n (n = 158)",
                                      "Verbesserung \n (n = 103)"))
abb35i17$ereignis <- factor(abb35i17$ereignis, levels=c(1:3),
                           labels = c("Eintritt","Phasenwechsel",
                                      "Austritt"))
png("abb51.png", 
    width = 6, height = 4, units = 'in', res = 300)
ggplot(abb35i17, aes(ereignis,i17s5, color=statusi17, group = fall_id))+
  geom_line(alpha=0.2) +
  geom_point(alpha = 0.2)+
  facet_wrap(~forcats::fct_rev(statusi17), ncol = 1, strip.position = "right")+
  scale_colour_manual(values = c("red4","grey40","darkgreen"))+
  labs(title = "Individueller Verlauf IPOS Probleme",
       subtitle = "Aufgeteilt nach Art der Veränderung",
       x = "", y = "Probleme")+
  theme_clean1()+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

#9. Spiderplot IPOS----
##Abb 52. Spiderplot Prävalenz alle Items Ein- und Austritt----
abb52 <- abb22
abb52$percent <- round(abb52$freq*100,2)
abb52 <- abb52[,c(1,4,6)]
abb52$name <- as.numeric(abb52$name)
abb52$ereignis <- as.numeric(abb52$ereignis)
abb52a <- pivot_wider(abb52, names_from = name, 
                      values_from = percent, names_prefix = "i")
names(abb52a)
ereignis <- c(3,4)
i17 <- c(100,0)
i8 <- c(100,0)
i7 <- c(100,0)
i6 <- c(100,0)
i5 <- c(100,0)
i4 <- c(100,0)
i3 <- c(100,0)
i2 <- c(100,0)
i1 <- c(100,0)
i16 <- c(100,0)
i15 <- c(100,0)
i14 <- c(100,0)
i13 <- c(100,0)
i12 <- c(100,0)
i11 <- c(100,0)
i10 <- c(100,0)
i9 <- c(100,0)
abb52b <- data.frame(ereignis,i17,i8,i7,i6,i5,i4,i3,i2,i1,i16,i15,i14,i13,i12,i11,i10,i9)
abb52 <- rbind(abb52b, abb52a)
abb52 <- abb52 %>% 
  select(ereignis, i17, i16, i15, i14, i13, i12, i11, i10,
         i9,i8,i7,i6,i5,i4,i3,i2,i1)
abb52$ereignis <- factor(abb52$ereignis, levels = c(3,4,1,2),
                         labels = c("1","2","Eintritt","Austritt"))
names(abb52)[2] <- "Probleme"
names(abb52)[3] <- "Info"
names(abb52)[4] <- "Gefühle"
names(abb52)[5] <- "Frieden"
names(abb52)[6] <- "Depression"
names(abb52)[7] <- "Fam.angst"
names(abb52)[8] <- "Angst"
names(abb52)[9] <- "Mobilität"
names(abb52)[10] <- "Schläfrigk."
names(abb52)[11] <- "Mundtrocken."
names(abb52)[12] <- "Verstopfung"
names(abb52)[13] <- "Appetitlosigkeit"
names(abb52)[14] <- "Erbrechen"
names(abb52)[15] <- "Übelkeit"
names(abb52)[16] <- "Fatigue"
names(abb52)[17] <- "Atemnot"
names(abb52)[18] <- "Schmerzen"
abb52 <- abb52 %>% 
  remove_rownames %>% 
  column_to_rownames(var="ereignis")

coul <- c("#084594","#54278f")
colors_border <- coul
colors_in <- alpha(coul,0.3)
library(fmsb)
par(mar=c(1,1,2,1))

radarchart(abb52,
           axistype = 1, 
           pcol=colors_border,pfcol=colors_in, plwd=2, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey60", 
           caxislabels=seq(0,100,25), cglwd=1.0, calcex = 0.7,
           #custom labels
           vlcex=0.6,
           title = "Veränderungen der Prävalenz zwischen Ein- und Austritt")
legend(x=1.2, y=1.2, legend = rownames(abb52[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

##Abb 53. Spiderplot Prävalenz alle Items Wechsel poi instabil-stabil----
abb53 <- subset(master, subset = ereignis != 2)
abb53 <- abb53[,c(3,14,24,44:60)]
abb53 <- abb53 %>% 
  pivot_wider(names_from = ereignis, values_from = poi:i17s5)
abb53 <- subset(abb53, subset = (poi_1 == 2 & poi_3 == 1))
#n = 144
abb53 <- abb53[,c(4:37)]
abb53 <- abb53 %>%
  pivot_longer(cols = starts_with('i'), names_to = c(".value","time"), 
               names_pattern = ".(\\w+)_(\\d+)")
abb53a <- subset(abb53, subset = time == "1")
abb53a <- abb53a %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
abb53a <- abb53a %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb53a <- subset(abb53a, subset = value >= 2 & value <= 4)
abb53a <- abb53a %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb53a$ereignis <- 1
abb53a$percent <- round(abb53a$freq * 100, 2)
abb53a <- abb53a[,c(1,4,5)]
abb53b <- subset(abb53, subset = time == "3")
abb53b <- abb53b %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
abb53b <- abb53b %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb53b <- subset(abb53b, subset = value >= 2 & value <= 4)
abb53b <- abb53b %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb53b$ereignis <- 2
abb53b$percent <- round(abb53b$freq * 100, 2)
abb53b <- abb53b[,c(1,4,5)]
abb53 <- rbind(abb53a,abb53b)
abb53$name <- factor(abb53$name, levels = c("17s5","16s5","15s5","14s5","13s5",
                                            "12s5","11s5","10s5","9s5","8s5",
                                            "7s5","6s5","5s5","4s5",
                                            "3s5","2s5","1s5"),
                     labels = c("Probleme","Information","Gefühle",
                                "Frieden","Depression", "Familienangst",
                                "Angst","Mobilität","Schläfrigkeit",
                                "Mundtrockenheit","Verstopfung",
                                "Appetitlosigkeit","Erbrechen","Übelkeit",
                                "Fatigue","Atemnot","Schmerzen"))
abb53$name <- as.numeric(abb53$name)
abb53 <- pivot_wider(abb53, names_from = name, 
                      values_from = percent, names_prefix = "i")
abb53 <- abb53 %>% 
  select(ereignis, i17, i16, i15, i14, i13, i12, i11, i10,
         i9,i8,i7,i6,i5,i4,i3,i2,i1)
ereignis <- c(3,4)
i17 <- c(100,0)
i8 <- c(100,0)
i7 <- c(100,0)
i6 <- c(100,0)
i5 <- c(100,0)
i4 <- c(100,0)
i3 <- c(100,0)
i2 <- c(100,0)
i1 <- c(100,0)
i16 <- c(100,0)
i15 <- c(100,0)
i14 <- c(100,0)
i13 <- c(100,0)
i12 <- c(100,0)
i11 <- c(100,0)
i10 <- c(100,0)
i9 <- c(100,0)
abb53c <- data.frame(ereignis,i17,i16,i15,i14,i13,i12,i11,i10,i9,i8,i7,i6,i5,i4,i3,i2,i1)
abb53 <- rbind(abb53c, abb53)
abb53$ereignis <- factor(abb53$ereignis, levels = c(3,4,1,2),
                         labels = c("1","2","Eintritt","Austritt"))
names(abb53)[2] <- "Probleme"
names(abb53)[3] <- "Info"
names(abb53)[4] <- "Gefühle"
names(abb53)[5] <- "Frieden"
names(abb53)[6] <- "Depression"
names(abb53)[7] <- "Fam.angst"
names(abb53)[8] <- "Angst"
names(abb53)[9] <- "Mobilität"
names(abb53)[10] <- "Schläfrigk."
names(abb53)[11] <- "Mundtrocken."
names(abb53)[12] <- "Verstopfung"
names(abb53)[13] <- "Appetitlosigkeit"
names(abb53)[14] <- "Erbrechen"
names(abb53)[15] <- "Übelkeit"
names(abb53)[16] <- "Fatigue"
names(abb53)[17] <- "Atemnot"
names(abb53)[18] <- "Schmerzen"
abb53 <- abb53 %>% 
  remove_rownames %>% 
  column_to_rownames(var="ereignis")
library(gplots)
col2hex("goldenrod2") ##eeb422
col2hex("darkgreen")  ##006400

coul <- c("#eeb422","#006400")
colors_border <- coul
colors_in <- alpha(coul,0.3)
library(fmsb)
par(mar=c(1,1,2,1))

radarchart(abb53,
           axistype = 1, 
           pcol=colors_border,pfcol=colors_in, plwd=2, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey60", 
           caxislabels=seq(0,100,25), cglwd=1.0, calcex = 0.7,
           #custom labels
           vlcex=0.6,
           title = "Veränderungen der Prävalenz instabil zu stabil (n=144)")
legend(x=1.2, y=1.2, legend = rownames(abb53[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

##Abb 54. Spiderplot Prävalenz alle Items Wechsel poi verschl-stabil----
abb54 <- subset(master, subset = ereignis != 2)
abb54 <- abb54[,c(3,14,24,44:60)]
abb54 <- abb54 %>% 
  pivot_wider(names_from = ereignis, values_from = poi:i17s5)
abb54 <- subset(abb54, subset = (poi_1 == 3 & poi_3 == 1))
#n = 9
abb54 <- abb54[,c(4:37)]
abb54 <- abb54 %>%
  pivot_longer(cols = starts_with('i'), names_to = c(".value","time"), 
               names_pattern = ".(\\w+)_(\\d+)")
abb54a <- subset(abb54, subset = time == "1")
abb54a <- abb54a %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
abb54a <- abb54a %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb54a <- subset(abb54a, subset = value >= 2 & value <= 4)
abb54a <- abb54a %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb54a$ereignis <- 1
abb54a$percent <- round(abb54a$freq * 100, 2)
abb54a <- abb54a[,c(1,4,5)]
abb54b <- subset(abb54, subset = time == "3")
abb54b <- abb54b %>%
  tidyr::pivot_longer(cols = -1) %>%
  count(name, value, .drop = FALSE)
abb54b <- abb54b %>% group_by(name) %>% 
  mutate(freq = n / sum(n))
abb54b <- subset(abb54b, subset = value >= 2 & value <= 4)
abb54b <- abb54b %>%
  group_by(name) %>%
  summarise(
    n = sum(n),
    freq = sum(freq)
  )
abb54b$ereignis <- 2
abb54b$percent <- round(abb54b$freq * 100, 2)
abb54b <- abb54b[,c(1,4,5)]
abb54 <- rbind(abb54a,abb54b)
abb54$name <- factor(abb54$name, levels = c("17s5","16s5","15s5","14s5","13s5",
                                            "12s5","11s5","10s5","9s5","8s5",
                                            "7s5","6s5","5s5","4s5",
                                            "3s5","2s5","1s5"),
                     labels = c("Probleme","Information","Gefühle",
                                "Frieden","Depression", "Familienangst",
                                "Angst","Mobilität","Schläfrigkeit",
                                "Mundtrockenheit","Verstopfung",
                                "Appetitlosigkeit","Erbrechen","Übelkeit",
                                "Fatigue","Atemnot","Schmerzen"))
abb54$name <- as.numeric(abb54$name)
abb54 <- pivot_wider(abb54, names_from = name, 
                     values_from = percent, names_prefix = "i")
abb54 <- abb54 %>% 
  select(ereignis, i17, i16, i15, i14, i13, i12, i11, i10,
         i9,i8,i7,i6,i5,i4,i3,i2,i1)
ereignis <- c(3,4)
i17 <- c(100,0)
i8 <- c(100,0)
i7 <- c(100,0)
i6 <- c(100,0)
i5 <- c(100,0)
i4 <- c(100,0)
i3 <- c(100,0)
i2 <- c(100,0)
i1 <- c(100,0)
i16 <- c(100,0)
i15 <- c(100,0)
i14 <- c(100,0)
i13 <- c(100,0)
i12 <- c(100,0)
i11 <- c(100,0)
i10 <- c(100,0)
i9 <- c(100,0)
abb54c <- data.frame(ereignis,i17,i16,i15,i14,i13,i12,i11,i10,i9,i8,i7,i6,i5,i4,i3,i2,i1)
abb54 <- rbind(abb54c, abb54)
abb54$ereignis <- factor(abb54$ereignis, levels = c(3,4,1,2),
                         labels = c("1","2","Eintritt","Austritt"))
names(abb54)[2] <- "Probleme"
names(abb54)[3] <- "Info"
names(abb54)[4] <- "Gefühle"
names(abb54)[5] <- "Frieden"
names(abb54)[6] <- "Depression"
names(abb54)[7] <- "Fam.angst"
names(abb54)[8] <- "Angst"
names(abb54)[9] <- "Mobilität"
names(abb54)[10] <- "Schläfrigk."
names(abb54)[11] <- "Mundtrocken."
names(abb54)[12] <- "Verstopfung"
names(abb54)[13] <- "Appetitlosigkeit"
names(abb54)[14] <- "Erbrechen"
names(abb54)[15] <- "Übelkeit"
names(abb54)[16] <- "Fatigue"
names(abb54)[17] <- "Atemnot"
names(abb54)[18] <- "Schmerzen"
abb54 <- abb54 %>% 
  remove_rownames %>% 
  column_to_rownames(var="ereignis")
library(gplots)
col2hex("darkorange2") ##ee7600
col2hex("darkgreen")  ##006400

coul <- c("#ee7600","#006400")
colors_border <- coul
colors_in <- alpha(coul,0.3)
library(fmsb)
par(mar=c(1,1,2,1))

radarchart(abb54,
           axistype = 1, 
           pcol=colors_border,pfcol=colors_in, plwd=2, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey60", 
           caxislabels=seq(0,100,25), cglwd=1.0, calcex = 0.7,
           #custom labels
           vlcex=0.6,
           title = "Veränderungen der Prävalenz verschlechternd zu stabil (n = 9)")
legend(x=1.2, y=1.2, legend = rownames(abb53[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
