if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae)

#Para Fazer leitura de arquivos csv com ; como delimitador
dados <- read_delim("https://raw.githubusercontent.com/andersonhsporto/Dados/main/Dados%20Cerveja/Teste-1.csv", delim = ";")

dados

ggplot(dados, aes(Tratamento, Nota)) +
  geom_boxplot(fill = "grey60", alpha = 0.7) +
  theme_bw(16)

leveneTest(Nota ~ factor(Tratamento), data=dados)

anova <-  aov(Nota ~ Tratamento, data=dados)
shapiro.test(resid(anova))

summary(anova)

tukey <- HSD.test(anova, "Tratamento")

tukey

tukey$groups %>% 
  rownames_to_column(var = "trt") %>% 
  mutate(trt = reorder(trt, -Nota, mean)) %>% 
  ggplot(aes(trt, Nota)) +
    geom_col(alpha = 0.8, color = "black") +
    geom_text(aes(label = groups), vjust = 1.8, size = 9, color = "white") +
    labs(x = "Tratamento", y = "Médias") +
    theme_bw(16)


sessioninfo::session_info(c("readr", "dplyr", "tibble", "ggplot2", "car", "agricolae"))
