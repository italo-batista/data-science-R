library(readr)
library(tidyverse)
library(tidytext)
library(tm) # para stopwords

hh_poetry = read_delim("poesia_hilda/data/hilda_hilst_poetry.csv", "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    arrange(ANO)

books = hh_poetry %>%
  group_by(LIVRO) %>%
  summarise(
    QNT_POEMAS = n(),
    QNT_VERSOS = sum(QNT_VERSOS),
    TAM = sum(TAM_POEMA)
  )

lexico = read_csv("poesia_hilda/data/lexicos/oplexicon_v3.0/lexico_v3.0.csv")

hh_poetry$DECADA = ((hh_poetry$ANO - 1900) %/% 10) * 10

hh_tidy_books <- hh_poetry %>%
  unnest_tokens(palavra, POEMA) %>%
  group_by(DECADA) %>%
  mutate(palavra_index = row_number()) %>%
  ungroup()

hh_sentiment <- hh_tidy_books %>%
  inner_join( lexico %>% mutate(palavra = simbolo) ) %>%
  count(DECADA, index = palavra_index %/% 80, score) %>%
  spread(score, n, fill = 0) 

colnames(hh_sentiment)[3] <- "negative"
colnames(hh_sentiment)[4] <- "neutro"
colnames(hh_sentiment)[5] <- "positive"

hh_sentiment = hh_sentiment %>%
  mutate(sentiment = positive - negative) %>%
  select(c(DECADA, index, sentiment))

library(wesanderson)

ggplot(hh_sentiment, aes(index, sentiment, fill = as.factor(DECADA))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~DECADA, ncol = 2, scales = "free_x") +
  labs(title = "Análise de sentimento na obra poética de Hilda Hilst",
       subtitle = "Através das décadas de 50 a 90", y = "Sentimento", x = NULL) +
  theme_tufte(base_family = "Arial", base_size = 15, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.5, 1), guide = FALSE) +  
  scale_x_continuous(expand=c(0,0)) +
  #scale_fill_manual(values=wes_palette(n=5, name="Rushmore")) +
  #scale_fill_brewer(palette="RdBu") +
  scale_fill_manual(values=c("#660066", "#5E80C3", "#009999", "#13CD70", "#84FD9C")) +
  theme(legend.position="none") + theme_bw()

# Palavras positivas e negativas mais comuns

positive_words <- lexico %>% 
  filter(score == 1) %>%
  mutate(palavra = simbolo) %>%
  select(palavra)

negative_words <- lexico %>% 
  filter(score == -1) %>%
  mutate(palavra = simbolo) %>%
  select(palavra)

neutro_words <- lexico %>% 
  filter(score == 0) %>%
  mutate(palavra = simbolo) %>%
  select(palavra)

stopwords = stopwords("portuguese") %>% 
  rbind(c("cara", "irmã"))

more_posit = hh_tidy_books %>%
  inner_join(positive_words) %>%
  filter(! palavra %in% stopwords) %>%
  count(palavra, sort = TRUE)
more_posit$tipo = "POSITIVA"

more_negat = hh_tidy_books %>%
  inner_join(negative_words) %>%
  filter(! palavra %in% stopwords) %>%
  count(palavra, sort = TRUE)
more_negat$tipo = "NEGATIVA"

more_neutr = hh_tidy_books %>%
  inner_join(neutro_words) %>%
  filter(! palavra %in% stopwords) %>%
  count(palavra, sort = TRUE)

negat_and_posit = rbind(more_negat, more_posit) %>%
  rename(total = n)


negat_and_posit %>%
    group_by(tipo) %>%
    top_n(15, total) %>%
    ungroup() %>%
    mutate(palavra = reorder(palavra, total)) %>%
    ggplot(aes(palavra, total, color = tipo)) +
    geom_segment(aes(x = palavra, xend = palavra, y = 0, yend = total), 
                 size = 1.1, alpha = 0.6) +
    geom_point(size = 3.5) +
    coord_flip() +
    labs(x = NULL, 
         y = "Quantidade de ocorrências",
         title = "Palavras positivas e negativas mais comuns na poesia de Hilda Hilst",
         subtitle= "Top 15 de cada uma") +
    scale_color_manual(values=c("#FF6600", "#4F0C68"), name = "", labels = c("Negativa", "Positiva"))

