library(readr)
library(tidyverse)
library(tidytext)
library(tm) # para stopwords

hh_poetry = read_delim("poesia_hilda/data/hilda_hilst_poetry.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

books = hh_poetry %>%
  group_by(LIVRO) %>%
  summarise(
    QNT_POEMAS = n(),
    QNT_VERSOS = sum(QNT_VERSOS),
    TAM = sum(TAM_POEMA)
  )

hh_bigrams = hh_poetry %>%
    unnest_tokens(bigram, POEMA, token = "ngrams", n = 2)

pronouns = c("ele", "eles", "ela", "elas", "eu", "tu", "sou", "és", "deus", "deusa", "me", "te")
stopwords = stopwords("portuguese")

bigram_counts <- hh_bigrams %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("pronoun", "around"), sep = " ") %>%
  filter((pronoun %in% pronouns & ! around %in% stopwords))

bigram_counts = hh_bigrams %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("around", "pronoun"), sep = " ") %>%
  filter((pronoun %in% pronouns & ! around %in% stopwords)) %>%
  rbind(bigram_counts)

pronouns_fem = c("ela", "elas", "eu", "sou", "deusa", "te")
pronouns_masc = c("ele", "eles", "tu", "és", "deus", "me")

bigram_counts$pronoun[bigram_counts$pronoun %in% pronouns_masc] = "masc"
bigram_counts$pronoun[bigram_counts$pronoun %in% pronouns_fem] = "fem"

bigram_counts = bigram_counts %>%
  count(pronoun, around, wt = n, sort = TRUE) %>%
  rename(total = nn)

word_ratios <- bigram_counts %>%
    group_by(around) %>%
    filter(sum(total) > 2) %>%
    ungroup() %>%
    spread(pronoun, total, fill = 0) %>%
    mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
    mutate(logratio = log2(fem / masc)) %>%
    arrange(desc(logratio)) 

word_ratios %>%
    mutate(abslogratio = abs(logratio)) %>%
    group_by(logratio < 0) %>%
    top_n(15, abslogratio) %>%
    ungroup() %>%
    mutate(word = reorder(around, logratio)) %>%
    ggplot(aes(word, logratio, color = logratio < 0)) +
    geom_segment(aes(x = word, xend = word,
                     y = 0, yend = logratio), 
                 size = 1.1, alpha = 0.6) +
    geom_point(size = 3.5) +
    coord_flip() +
    labs(x = NULL, 
         y = "Quantidade de vezes que uma dada palavra aparece ao redor de \nsujeitos femininos em comparação com sujeitos masculinos",
         title = "Palavras ao redor de sujeitos masculinos e femininos \nna obra poética de Hilda Hilst") +
    scale_color_manual(values=c("#FF6600", "#4F0C68"), name = "", labels = c("Sujeitos femininos", "Sujeitos masculinos")) +
    scale_y_continuous(breaks = seq(-3, 3),
                       labels = c("0.125x", "0.25x", "0.5x", 
                                  "Igual", "2x", "4x", "8x")) 
