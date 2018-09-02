library(readr)
library(tidyverse)
library(tidytext)
library(tm) # para stopwords
library(stringr)

hh_poetry = read_delim("poesia_hilda/data/hilda_hilst_poetry.csv", "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    arrange(ANO)

hh_poetry$QNT_PALAVRAS = str_count(hh_poetry$POEMA,'\\w+')

books = hh_poetry %>%
  group_by(LIVRO) %>%
  summarise(
    QNT_POEMAS = n(),
    QNT_VERSOS = sum(QNT_VERSOS),
    TAM = sum(TAM_POEMA),
    QNT_PALAVRAS = sum(QNT_PALAVRAS)
  )

hh_poetry$index = rownames(hh_poetry)


hh_book_words = hh_poetry %>%
  unnest_tokens(word, POEMA) %>%
  count(LIVRO, word, sort = TRUE) %>%
  ungroup()

stopwords = stopwords("pt")
hh_book_words = hh_book_words %>%
  filter(! word %in% stopwords)

hh_total_words = hh_book_words %>% group_by(LIVRO) %>% summarize(total = sum(n))
hh_book_words <- left_join(hh_book_words, hh_total_words)

hh_book_words = hh_book_words %>%
  bind_tf_idf(word, LIVRO, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 

library(ggstance)
library(ggthemes)
library(viridis)

# Palavras mais relevantes na obra completa

hh_book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(25, tf_idf) %>%
  ggplot(aes(tf_idf, word, alpha = tf_idf)) +
  geom_barh(stat = "identity", fill = "#64007D") +
  geom_text(stat = "identity", size = 2.3, color = "white", alpha = 1, hjust = 1, 
            fontface = "bold", aes(label=LIVRO)) +
  labs(title = "Palavras mais relevantes na obra poética de Hilda Hilst",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.5, 1), guide = FALSE) +  
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position="none")


library(ptstem)

livros_pequenos = books %>% filter(QNT_PALAVRAS < 500) %>% select(LIVRO)

hh_book_stem = hh_book_words %>%
  mutate(stem = ptstem(word)) %>%
  count(LIVRO, stem, sort = TRUE) %>%
  ungroup()

hh_total_stem = hh_book_stem %>% group_by(LIVRO) %>% summarize(total = sum(nn))
hh_book_stem <- left_join(hh_book_stem, hh_total_stem)

hh_book_stem = hh_book_stem %>%
  bind_tf_idf(stem, LIVRO, nn) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 

hh_book_stem = hh_book_stem %>% 
  filter(! LIVRO %in% livros_pequenos$LIVRO)

hh_book_stem = hh_book_stem[ ! duplicated(hh_book_stem[, c('stem')]), ]

hh_book_stem %>%
  arrange(desc(tf_idf)) %>%
  mutate(stem = factor(stem, levels = rev(unique(stem)))) %>%
  top_n(26, tf_idf) %>%
  ggplot(aes(tf_idf, stem, alpha = tf_idf)) +
  geom_barh(stat = "identity", fill = "#EE7F01") +
  geom_text(stat = "identity", size = 2.3, color = "white", alpha = 1, hjust = 1, 
            fontface = "bold", aes(label=LIVRO)) +
  labs(title = "Palavras mais relevantes na obra poética de Hilda Hilst",
       subtitle = "Com stemming",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.5, 1), guide = FALSE) +  
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position="none")


# Palavras mais relevantes em obras específicas

filtered = c("POEMAS MALDITOS GOZOSOS E DEVOTOS",
             "DA MORTE. ODES MíNIMAS",
             "DO DESEJO",
             "AMAVISSE",
             "EXERCÍCIOS PARA UMA IDÉIA" ,
             "ODE DESCONTÍNUA E REMOTA PARA FLAUTA E OBOÉ. DE ARIANA PARA DIONÍSIO.", 
             "VIA ESPESSA", 
             "VIA VAZIA")

hh_book_words %>%
  filter(LIVRO %in% filtered) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(LIVRO) %>%
  top_n(4, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, word, fill = LIVRO)) +
  geom_barh(stat = "identity") +
  labs(title = "Palavras mais relevantes em alguns livros de poesia de Hilda Hilst",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 20, ticks = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") + 
  theme(legend.text=element_text(size=11))

hh_book_words %>%
  filter(LIVRO %in% filtered) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))%>% 
  group_by(LIVRO) %>% top_n(4) %>% ungroup() %>%
  ggplot(aes(tf_idf, word, fill = LIVRO, alpha = tf_idf)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Palavras mais relevantes em alguns livros de poesia de Hilda Hilst",
       y = NULL, x = "tf-idf") +
  facet_wrap(~LIVRO, ncol = 2, scales = "free") +
  theme_tufte(base_family = "Arial", base_size = 15, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))

