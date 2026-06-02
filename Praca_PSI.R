
 #install.packages("RColorBrewer")
 #install.packages("tm")
 #install.packages("tidyverse")
 #install.packages("tidytext")
 #install.packages("wordcloud")
 #install.packages("ggplot2")
 #install.packages("ggthemes")
 #install.packages("SentimentAnalysis")

# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# Załadowanie danych i utworzenie korpusu. ----
dane <- read.csv("Aplikacje_all.csv", stringsAsFactors = FALSE, header = FALSE, encoding = "UTF-8")
tresc <- dane[, 1]
corpus <- VCorpus(VectorSource(tresc))

# Przetwarzanie i oczyszczanie korpusu ze zbędnych znaków i wyrażeń ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usunięcue znaku @
corpus <- tm_map(corpus, toSpace, "@")

# Usunięcie interpunkcji z wyłączeniem myślniknów między słowami i apostrofów między słowami
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE,
                 preserve_intra_word_contractions = TRUE)

# Zamiana wszystkiego na małe litery 
corpus <- tm_map(corpus, content_transformer(tolower))

# Usunięcie wyrazów o prawie zerowej wartości informacyjnej
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Usunięcie liczb 
corpus <- tm_map(corpus, removeNumbers)

# Usunięcie niepotrzebnych wyrazów
corpus <- tm_map(corpus, removeWords, c("bank", "banking", "america", "american", "fargo",
                                        "chase", "wells", "capital", "boa", "citi", "morgan", "jp", "also", "sooooo"))

# Usunięcie nadmiarowych białych znaków z tekstu
corpus <- tm_map(corpus, stripWhitespace)


# Stworzenie macierzy Text Document Matrix  ----
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

# Zliczanie częstości słów
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

# Chmura słów ----

# Ustawienia graficzne
par(
  bg = "#292929",    
  family = "sans",    
  mar = c(0, 0, 0, 0) 
)

# Chmura słów
wordcloud(words = tdm_df$word,
          freq = tdm_df$freq,
          min.freq = 7, 
          max.words = 100,
          random.order = FALSE,
          scale = c(4.0, 1.0),
          colors = brewer.pal(8, "PiYG")
)

# Badanie asocjacji ----

# Wybór słów, których asocjacja zostanie zbadana.
# Proponowane słowa: password, staff, credit, client, download, update, phone, error
target_words <- c("password", "credit", "client", "download")
cor_limit <- 0.2

# Pętla dla wybranych słów
for (target_word in target_words) {
  
  # Obliczenie asocjacji dla danego słowa
  associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
  assoc_vector <- associations[[target_word]]
  assoc_sorted <- sort(assoc_vector, decreasing = TRUE)
  
  # Ramka danych
  assoc_df <- data.frame(
    word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
    score = assoc_sorted
  )
  
  # Ograniczenie danych, aby nie było chaosu informacyjnego
  assoc_30 <- head(assoc_df, 30)
  
  
  # Wykres lizakowy (lollipop chart)
  plot <- ggplot(assoc_30, aes(x = score, y = reorder(word, score))) +
    geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#FFBDF8", linewidth = 1.5, size = 1.2) +
    geom_point(color = "#FA61AE", size = 4) +
    geom_text(aes(label = round(score, 2)), hjust = -0.5, size = 3.0, color = "black") +
    scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste0("Asocjacje z terminem: '", target_word, "'"),
      subtitle = paste0("Próg r ≥ ", cor_limit),
      x = "Współczynnik korelacji Pearsona",
      y = "Słowo"
    ) +
    theme(
      plot.title = element_text(family = "mono", face = "bold", size = 16),
      axis.title.x = element_text(margin = margin(t = 10), family = "mono"),
      axis.title.y = element_text(margin = margin(r = 10), family = "mono"),
      axis.text.y = element_text(family = "mono", face = "bold", size = 11),
      plot.subtitle = element_text(family = "mono", size = 11, margin = margin(b = 15))
    )
  print(plot)
}




# Analiza sentymentu w czasie ----

library(SentimentAnalysis)
library(ggplot2)
library(ggthemes)
library(tidyverse)

# Wczytanie danych tekstowych
# Wczytujemy pierwszą kolumnę z pliku CSV ---
text <- read.csv("Aplikacje_all.csv", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")[, 1]



# Analiza sentymentu przy użyciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(text)


# odkomentuj i zobacz parametry funkcji:
# ?analyzeSentiment


### Słownik GI (General Inquirer) ----
#
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytaj słownik GI
data(DictionaryGI)
summary(DictionaryGI)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI)





# Wykres skumulowanego sentymentu kierunkowego w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")

# Usunięcie wierszy, które zawierają NA
df_GI <- na.omit(df_GI)

ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "deeppink", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### Słownik HE (Henry’s Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# pierwszy, jaki powstał w wyniku analizy komunikatów prasowych 
# dotyczących zysków w branży telekomunikacyjnej i usług IT
# DictionaryHE


# Wczytaj słownik HE
data(DictionaryHE)
summary(DictionaryHE)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem HE
sentimentHE <- convertToDirection(sentiment$SentimentHE)



# Wykres skumulowanego sentymentu kierunkowego w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")

# Usunięcie wierszy, które zawierają NA
df_HE <- na.omit(df_HE)

ggplot(df_HE, aes(x = value)) +
  geom_bar(fill = "mediumblue", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (HE)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### Słownik LM (Loughran-McDonald Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Wczytaj słownik LM
data(DictionaryLM)
summary(DictionaryLM)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem LM
sentimentLM <- convertToDirection(sentiment$SentimentLM)






# Wykres skumulowanego sentymentu kierunkowego w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")

# Usunięcie wierszy, które zawierają NA
df_LM <- na.omit(df_LM)

ggplot(df_LM, aes(x = value)) +
  geom_bar(fill = "darkorchid", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (LM)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




### Słownik QDAP (Quantitative Discourse Analysis Package) ----
#
# zawiera listę słów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytaj słownik QDAP
qdap <- loadDictionaryQDAP()
summary(qdap)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem QDAP
sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)



# Wykres skumulowanego sentymentu kierunkowego w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")

# Usunięcie wierszy, które zawierają NA
df_QDAP <- na.omit(df_QDAP)

ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "darkcyan", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()



# Porównanie sentymentu na podstawie różnych słowników ----


# Połączenie poszczególnych ramek w jedną ramkę
df_all <- bind_rows(df_GI, df_HE, df_LM, df_QDAP)

# Tworzenie wykresu z podziałem na słowniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment według słowników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podział na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "deeppink", 
                               "HE" = "mediumblue", 
                               "LM" = "darkorchid",
                               "QDAP" = "darkcyan" ))




# Agregowanie sentymentu z różnych słowników w czasie ----


# Sprawdzenie ilości obserwacji
length(sentiment[,1])


# Utworzenie ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)



# USUNIĘCIE BRAKUJĄCYCH WARTOŚCI
# gdyż wartości NA (puste) uniemożliwiają generowanie wykresu w ggplot
#

# Usunięcie wartości NA
# Wybranie tylko niekompletnych przypadków:
puste <- df_all[!complete.cases(df_all), ]


# Usunięcie pustych obserwacji
# np. dla zmiennej QDAP (wszystkie mają NA)
df_all <- df_all[!is.na(df_all$QDAP), ]


# Sprawdzenie, czy wartości NA zostały usunięte
# wtedy puste2 ma 0 wierszy:
puste2 <- df_all[!complete.cases(df_all), ]
puste2




# Wykresy przedstawiające ewolucję sentymentu w czasie ----



ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color= "darkcyan", size=1) +
  geom_line(aes(x=sentence, y=GI), color="deeppink", size=1) +
  geom_line(aes(x=sentence, y=HE), color="mediumblue", size=1) +
  geom_line(aes(x=sentence, y=LM), color="darkorchid", size=1) +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")



ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="darkcyan") +
  geom_smooth(aes(x=sentence, y=GI), color="deeppink") +
  geom_smooth(aes(x=sentence, y=HE), color="mediumblue") +
  geom_smooth(aes(x=sentence, y=LM), color="darkorchid") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")
