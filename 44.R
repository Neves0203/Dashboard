library(tidyverse)

#Padronização da ESTAT.
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

estat_theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


data <- athlete_events %>%
  select(!Games) %>%
  filter(Year %in% c(2000,2004,2008,2012,2016))  %>%
  filter(Season == "Summer") %>%
  filter(!is.na(Medal))

medalhistas <- data %>%
  select(Name, Medal)
medalhas <- as.data.frame(table(medalhistas))

medalhistas$Medal <- "Medalha"
medalhas_geral <- as.data.frame(table(medalhistas))  
medalhas_geral %>%
  arrange(desc(Freq))

barras <- data %>%
  select(Name,Medal) %>%
  filter(Name == "Michael Fred Phelps, II" | 
           Name == "Natalie Anne Coughlin (-Hall)" | 
           Name == "Ryan Steven Lochte")
barras2 <- medalhas %>%
  filter(Name == "Michael Fred Phelps, II" | 
           Name == "Natalie Anne Coughlin (-Hall)" | 
           Name == "Ryan Steven Lochte")

barras2$Medal <- factor(barras2$Medal, levels = c("Bronze","Silver","Gold"))

barras3 <- barras2 %>%
  group_by(Name) %>%
  summarise(round(Freq/sum(Freq),4))

colnames(barras3) <- c("Name","porcentagem")

#barras2 <- left_join(barras2,barras3)

ggplot(barras2, aes(Name, Freq))+
  geom_col(aes( fill = Medal), position = position_dodge2(preserve = "single", padding = 0))+
  labs(x = "Atleta", y = "Frequência") +
  estat_theme()+
  scale_fill_manual(values = c("#cd7f32","#c0c0c0","#f6d700"))
#geom_text(
# position = position_dodge(width = .9),
# vjust = -0.5, hjust = 0.5,
# size = 3
# ) +

medalhistas_mulheres <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Sex == "F") %>%
  distinct(Event, Medal, Year, .keep_all = TRUE) %>%
  select(Medal,Team) 



medalhistas_mulheres$Medal <- "Medalhas"
medalhistas_mulheres <- as.data.frame(table(medalhistas_mulheres))

medalhistas_mulheres <- medalhistas_mulheres %>%
  arrange(desc(Freq)) %>%
  head(5)

library("ggimage")
medalhistas_mulheres$porcentagem = paste(medalhistas_mulheres$Freq, " (",round(medalhistas_mulheres$Freq/sum(medalhistas_mulheres$Freq),4)*100,"%",")",sep = "")

ggplot(medalhistas_mulheres, aes(fct_reorder(Team, Freq), Freq, label = porcentagem, fill = "#A11D21"))+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_image(aes(image = c("C:/Users/Faculdade/Downloads/eua.jpeg",
                           "C:/Users/Faculdade/Downloads/Russia.jpeg",
                           "C:/Users/Faculdade/Downloads/China.jpg",
                           "C:/Users/Faculdade/Downloads/Austria.jpeg",
                           "C:/Users/Faculdade/Downloads/Alemanha.jpg"
                           )), size = 0.1, by = "width")+
  geom_text(
    position = position_dodge(width = .9),
    vjust = 3, hjust = 0.5,
    size = 3, color = "white"
  ) +
  guides(fill = "none") +
  labs(x = "País", y = "Frequência") +
  estat_theme()

Alemanha1 <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Team == "Germany" & Sex == "F" & Year == 2000) %>%
  distinct(Event, .keep_all = TRUE)

Alemanha2 <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Team == "Germany" & Sex == "F" & Year == 2004) %>%
  distinct(Event, .keep_all = TRUE)

Alemanha3 <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Team == "Germany" & Sex == "F" & Year == 2008) %>%
  distinct(Event, .keep_all = TRUE)

Alemanha4 <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Team == "Germany" & Sex == "F" & Year == 2012) %>%
  distinct(Event, .keep_all = TRUE)

Alemanha5 <- data %>%
  select(Medal, Sex, Team, Event, Year) %>%
  filter(Team == "Germany" & Sex == "F" & Year == 2016) %>%
  distinct(Event, .keep_all = TRUE)

peso_altura <- data %>%
  select(Name, Weight, Height, Sport) %>%
  drop_na(Weight) %>%
  drop_na(Height) %>%
  distinct(Name, .keep_all = TRUE)

peso_altura$Height <- peso_altura$Height/100
peso_altura$IMC = round(peso_altura$Weight/(peso_altura$Height^2),2)

ggplot(peso_altura, aes(Weight, Height)) +
  geom_point()

peso_altura <- peso_altura %>%
  mutate(categoria = case_when(
    IMC < 18.5 ~ "Magreza",
    IMC >= 18.5 & IMC < 24.9999 ~ "Normal",
    IMC >= 25 & IMC < 29.9999 ~ "Sobrepeso",
    IMC >= 30 & IMC < 34.9999 ~ "Obesidade Grau I",
    IMC >= 35 & IMC < 39.9999 ~ "Obesidade Grau II",
    IMC >= 40 ~ "Obesidade Grau III"
  ))

ggplot(peso_altura, aes(Weight, Height, color = categoria)) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.1) +
  labs(
    x = "Peso",
    y = "Altura"
  ) +
  estat_theme()

medalhas_eua <- data %>%
  select(Team, Medal,Year) %>%
  filter(Team == c("United States")) %>%
  select(!Team)
medalhas_eua$Medal <- "Medalhas"

medalhas_eua <- as.data.frame(table(medalhas_eua))


ggplot(medalhas_eua, aes(Year, Freq, group = 1, color = "#A11D21")) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Medalhas") +
  guides(color = "none")+
  estat_theme()

medalhas_italy <- data %>%
  select(Team, Medal,Year) %>%
  filter(Team == c("Italy")) %>%
  select(!Team)
medalhas_italy$Medal <- "Medalhas"

medalhas_italy <- as.data.frame(table(medalhas_italy))

ggplot(medalhas_italy, aes(Year, Freq, group = 1, color = "#A11D21")) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Medalhas") +
  guides(color = "none")+
  estat_theme()

medalhas_france <- data %>%
  select(Team, Medal,Year) %>%
  filter(Team == c("France")) %>%
  select(!Team)
medalhas_france$Medal <- "Medalhas"

medalhas_france <- as.data.frame(table(medalhas_france))

ggplot(medalhas_france, aes(Year, Freq, group = 1, color = "#A11D21")) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Medalhas") +
  guides(color = "none")+
  estat_theme()

medalhas_Brazil <- data %>%
  select(Team, Medal,Year) %>%
  filter(Team == c("Brazil")) %>%
  select(!Team)
medalhas_Brazil$Medal <- "Medalhas"

medalhas_Brazil <- as.data.frame(table(medalhas_Brazil))

ggplot(medalhas_Brazil, aes(Year, Freq, group = 1, color = "#A11D21")) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Medalhas") +
  guides(color = "none")+
  estat_theme()

ggplot(peso_altura, aes(IMC,Sport, group = Sport))+
  geom_boxplot()

peso_altura2 <- peso_altura %>%
  filter(Sport == c("Judo","Rhythmic Gymnastics","Athletics","Football","Badminton"))

ggplot(peso_altura2, aes(fct_reorder(Sport, IMC),IMC, group = Sport))+
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Esporte", y = "IMC") +
  estat_theme()
