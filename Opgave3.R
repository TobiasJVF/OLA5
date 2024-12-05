# opg.1 – Illustration af spørgsmålet

library(data.table)
library(ggplot2)
library(ordinal)
library(tidyverse)

# normalt read.csv virker ikke godt med store csv-file, derfor anvend her package data.table.
# funktion "fread" er samme som read.csv men hurtigere, alle control i read.csv som nrows, colclasses, sep blive håndtere automatisk
# encoding "Latin-1" kan håndtere dansk bogstaver æ, ø, å

df <- fread("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv", encoding = "Latin-1")

# for lav en grafisk illustration af fordelingen af svarene på spørgsmålet skal vi have en frekvense table
# der findes både dårlige og dårlig under kolonen
df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` <- gsub("Dårlige", "Dårlig", df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

svar_fordeling <- table(df[,1])
svar_df <- data.frame(Svar = names(svar_fordeling), Antal = as.numeric(svar_fordeling))

ggplot(svar_df, aes(x = Svar, y = Antal)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Flest folk synes der er Gode mulighed for at låne penge til sin virksomhed",
       x = "Svarmulighed",
       y = "Antal svar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 10))

# for uddybe opg.1 mht. Baums artikler
# vi vil gerne se hvad har folk svaret ift. størrelse af virksomhed og det gør vi ved deler virksomhedens størrelse med antal ansatte og svarmuligheder med nettotal
df1.1 <- df %>%
  mutate(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget gode" ~ 1,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode" ~ 0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ 0,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlig" ~ -0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget dårlige" ~ -1
  )) %>%
  mutate(`Antal ansatte Cvr-nr.` = case_when(
    `Antal ansatte Cvr-nr.` <= 20 ~ "1-20 ansatte",
    `Antal ansatte Cvr-nr.` <= 50 ~ "21-50 ansatte",
    `Antal ansatte Cvr-nr.` <= 100 ~ "51-100 ansatte",
    `Antal ansatte Cvr-nr.` <= 400 ~ "100-400 ansatte",
    `Antal ansatte Cvr-nr.` > 400 ~ "Over 400 ansatte"
  ))

df_nettotal <- df1.1 %>%
  group_by(`Antal ansatte Cvr-nr.`) %>%
  summarise(nettotal = mean(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`, na.rm = TRUE))

# omdanne til facotr med levels for specifik rækkefølge
df_nettotal$`Antal ansatte Cvr-nr.` <- factor(df_nettotal$`Antal ansatte Cvr-nr.`, 
                                              levels = c("1-20 ansatte", 
                                                         "21-50 ansatte", 
                                                         "51-100 ansatte", 
                                                         "100-400 ansatte",
                                                         "Over 400 ansatte"))
# lave søjle diagram
ggplot(df_nettotal, aes(x = `Antal ansatte Cvr-nr.`, y = nettotal)) +
  geom_bar(stat = "identity", aes(fill = `Antal ansatte Cvr-nr.`)) +
  geom_text(aes(label = round(nettotal, 2)), vjust = -0.5) +  
  labs(title = "De største virksomheder synes bedst om finansieringsklimaet",
       x = "Antal ansatte",
       y = "Nettotal",
       fill = "Antal ansatte") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size = 12))

# Nu vil vi gerne se med økonomisk perpektiv siden antallet af medarbejdere er ikke alene afgørende lånemulighederne
# Vi ved virksomhed med højt indtjening på aktiver er nemmere til at låne penge. Og jo robuste virksomheder er jo bedre adgang til lån
# Derfor til det næste graf kigger vi på afkastningsgrad og soliditetsgrad
# Vi skal først have gennemsnit af afkastningsgraden og soliditetsgraden fordi kolonnerne er blevet delt fra 2016-2021

df_virksomhed_gns <- df %>%
  rowwise() %>%
  mutate(
    # Beregn gennemsnit soliditet per virksomhed
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    # Beregn gennemsnit afkast per virksomhed
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE)
  ) %>%
  ungroup()

# Nu deler vi den ift. svar muligheder

df_svar_gns <- df_virksomhed_gns %>%
  mutate(
    svar_kategori = case_when(
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Meget dårlige", "Dårlige") ~ "Meget dårlige/Dårlige",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ "Neutrale",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode"
    )
  ) %>%
  group_by(svar_kategori) %>%
  summarise(
    gns_soliditet = mean(gns_soliditet, na.rm = TRUE),
    gns_afkast = mean(gns_afkast, na.rm = TRUE)
  )

df_svar_gns <- df_svar_gns[-4,]

# Nu laver vi graf
df_long <- df_svar_gns %>%
  pivot_longer(
    cols = c(gns_soliditet, gns_afkast),
    names_to = "type",
    values_to = "værdi"
  )

df_long$svar_kategorii = factor(df_long$svar_kategori, 
                       levels = c("Meget dårlige/Dårlige", 
                                  "Neutrale", 
                                  "Gode/Meget gode"))


ggplot(df_long, aes(x = type, y = værdi, fill = svar_kategori)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(title = "Virksomheder med et højere soliditetsgrad synes\nbedre om finansieringsklimaet",
       y = "Pct.",
       x = "",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 5))

# opg.2 – Cumulative Link Models

df_CLM <- df %>%
  mutate(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget gode" ~ 5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode" ~ 4,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ 3,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlig" ~ 2,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget dårlige" ~ 1
  )) %>%
  filter(!is.na(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)) %>%
  rowwise() %>%
  mutate(
    # Beregn gennemsnit soliditet per virksomhed
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    # Beregn gennemsnit afkast per virksomhed
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE),
    # Beregn gennemsnit balance per virksomhed
    gns_balance = mean(c(`Balance 2021 (1.000 kr)`,
                        `Balance 2020 (1.000 kr)`,
                        `Balance 2019 (1.000 kr)`,
                        `Balance 2018 (1.000 kr)`,
                        `Balance 2017 (1.000 kr)`,
                        `Balance 2016 (1.000 kr)`),
                      na.rm = TRUE)
  ) %>%
  ungroup


df_CLM$lånmuligheder <- factor(df_CLM$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
                               levels = c("1", "2", "3", "4", "5"),
                               labels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode"),
                               ordered = TRUE)

df_CLM_clean <- df_CLM_clean %>%
  mutate(across(c(gns_soliditet, gns_afkast, gns_balance), 
                ~scale(.) %>% as.vector))

model_clm <- clm(lånmuligheder~ gns_soliditet + 
                  gns_afkast + 
                  gns_balance, 
                data = df_CLM_clean, link = "logit")


summary(model_clm)

# opg.3

# Først laver vi grunddata
df_3 <- df %>%
  rowwise() %>%
  mutate(
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE),
    # Del balance med 1000 for at få samme skala
    gns_balance = mean(c(`Balance 2021 (1.000 kr)`,
                         `Balance 2020 (1.000 kr)`,
                         `Balance 2019 (1.000 kr)`,
                         `Balance 2018 (1.000 kr)`,
                         `Balance 2017 (1.000 kr)`,
                         `Balance 2016 (1.000 kr)`),
                       na.rm = TRUE)/1000
  ) %>%
  ungroup()

# Gruppering og kategorisering
df_3g <- df_3 %>%
  mutate(
    svar_kategori = case_when(
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Meget dårlige", "Dårlige") ~ "Meget dårlige/Dårlige",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ "Neutrale",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode"
    )
  ) %>%
  filter(!is.na(svar_kategori)) %>%  # Fjern NA værdier
  group_by(svar_kategori) %>%
  summarise(
    gns_soliditet = mean(gns_soliditet, na.rm = TRUE),
    gns_afkast = mean(gns_afkast, na.rm = TRUE),
    gns_balance = mean(gns_balance, na.rm = TRUE)
  )

# Omstrukturering til long format
df_3long <- df_3g %>%
  pivot_longer(
    cols = c(gns_soliditet, gns_afkast, gns_balance),
    names_to = "type",
    values_to = "værdi"
  )

# Sæt korrekt rækkefølge på faktor levels
df_3long$svar_kategori <- factor(df_3long$svar_kategori,
                                 levels = c("Meget dårlige/Dårlige",
                                            "Neutrale",
                                            "Gode/Meget gode"))

# Plot
ggplot(df_3long, aes(x = type, y = værdi, fill = svar_kategori)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(title = "Virksomheder med et højere soliditetsgrad synes\nbedre om finansieringsklimaet",
       y = "Pct./ Balance i 100.000kr",
       x = "",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 190),
                     breaks = seq(0, 190, by = 20))












# Tilføj etableringsdato som X variabel i modellen
library(lubridate)

df_CLM <- df_CLM %>%
  mutate(
    alder = year(Sys.Date()) - year(dmy(Etableringsdato))
  )

df_CLM_clean_alder <- df_CLM %>%
  mutate(across(c(gns_soliditet, gns_afkast, gns_balance, alder), 
                ~scale(.) %>% as.vector))

alder_clm <- clm(lånmuligheder ~ gns_soliditet + 
                   gns_afkast + 
                   gns_balance + 
                   alder, 
                 data = df_CLM_clean_alder, 
                 link = "logit")
summary(alder_clm)


