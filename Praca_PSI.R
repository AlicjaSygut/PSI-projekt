#' ---
#' title: "Projekt zaliczeniowy"
#' author: "Autor: Zofia Bocian, Julia Chmielecka, Alicja Sygut"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    # Kod domyślnie zwinięty (estetyczniej)
#'     number_sections: true # Numeruje nagłówki (lepsza nawigacja)
#'     css: "custom.css"     # Możliwość stworzenia własnego stylowania (opcjonalne)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)
# install.packages("RColorBrewer")
# install.packages("tm")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("wordcloud")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("SentimentAnalysis")
# install.packages("dplyr")
# install.packages("e1071")
# install.packages("stringr")

#'# Przygotowanie danych
# Przygotowanie danych ----

#'## Wymagane pakiety
## Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(SentimentAnalysis)
library(dplyr)
library(e1071)
library(stringr)

#'## Załadowanie danych i oczyszczenie korpusu. 
## Załadowanie danych i oczyszczenie korpusu. ----
dane <- read.csv("Aplikacje_all.csv", stringsAsFactors = FALSE, header = FALSE, encoding = "UTF-8")
tresc <- dane[, 1]
corpus <- VCorpus(VectorSource(tresc))

# Przetwarzanie i oczyszczanie korpusu ze zbędnych znaków i wyrażeń

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


# Stworzenie macierzy Text Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)

# Macierz częstości TDM z TF-IDF (dla Machine Learningu)
tdm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_tfidf_m <- as.matrix(tdm_tfidf)

# Zliczanie częstości słów
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)

#'# Eksploracyjna analiza tekstu
# Eksploracyjna analiza tekstu ----

#'## Chmura słów 
## Chmura słów ----

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

#'## Badanie asocjacji 
## Badanie asocjacji ----

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


#'# Analiza sentymentu 
# Analiza sentymentu ----


# Wczytanie danych tekstowych
# Wczytujemy pierwszą kolumnę z pliku CSV
text <- read.csv("Aplikacje_all.csv", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")[, 1]



# Analiza sentymentu przy użyciu pakietu SentimentAnalysis
sentiment <- analyzeSentiment(text)


#'## Słownik GI (General Inquirer)

## Słownik GI (General Inquirer) ----
#
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytanie słownika GI
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



#'## Słownik HE (Henry’s Financial dictionary) 
## Słownik HE (Henry’s Financial dictionary) ----
#
# Zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# dotyczących zysków w branży telekomunikacyjnej i usług IT
# DictionaryHE


# Wczytanie słownika HE
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



#'## Słownik LM (Loughran-McDonald Financial dictionary) 
## Słownik LM (Loughran-McDonald Financial dictionary) ----
#
# Zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Wczytanie słownika LM
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



#'## Słownik QDAP (Quantitative Discourse Analysis Package)
## Słownik QDAP (Quantitative Discourse Analysis Package) ----
#
# Zawiera listę słów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytanie słownika QDAP
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


#'## Porównanie sentymentu na podstawie różnych słowników 
## Porównanie sentymentu na podstawie różnych słowników ----


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

#'# Klasyfikacja (Machine Learning)
# Klasyfikacja (Machine Learning) ----
# Przygotowanie ocen z pliku ----

# Wczytanie pliku jako surowych linii tekstu
surowe_linie <- readLines("Aplikacje_all.csv", encoding = "UTF-8")

# Użycie pakietu stringr, żeby "złapać" tylko cyfrę z samego końca każdego wiersza
library(stringr)
oceny_gwiazdki <- as.numeric(str_extract(surowe_linie, "\\d+$"))

# Zamienienie gwiazdek na kategorie "yes" i "no" (4-5 to yes, 1-3 to no)
kategorie_polecenia <- ifelse(oceny_gwiazdki >= 4, "yes", "no")

# Utworzenie ramki danych dla modelu SVM
dtm_df <- as.data.frame(t(tdm_tfidf_m))
dtm_df$Recommended <- factor(kategorie_polecenia, levels = c("no", "yes"))

#'## Podział na zbiór treningowy/testowy: STRATYFIKOWANY 
## Podział na zbiór treningowy/testowy: STRATYFIKOWANY ----

yes_class <- dtm_df[dtm_df$Recommended == "yes", ]
no_class  <- dtm_df[dtm_df$Recommended == "no",  ]

set.seed(123)
yes_train_indices <- sample(1:nrow(yes_class), size = floor(0.8 * nrow(yes_class)))
no_train_indices  <- sample(1:nrow(no_class),  size = floor(0.8 * nrow(no_class)))

trainData <- rbind(yes_class[yes_train_indices, ], no_class[no_train_indices, ])
testData  <- rbind(yes_class[-yes_train_indices, ], no_class[-no_train_indices, ])

# Model klasyfikacji
svm_model <- svm(Recommended ~ ., data = trainData, kernel = "linear", probability = TRUE, scale= FALSE)

# Ocena modelu na zbiorze testowym
predictions <- predict(svm_model, newdata = testData)
confusion_matrix <- table(Predicted = predictions, Actual = testData$Recommended)
print(confusion_matrix)

# Wyciąganie TP, TN, FP, FN z confusion_matrix
# "yes" jako pozytywna klasa
TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FP <- confusion_matrix["yes", "no"]
FN <- confusion_matrix["no", "yes"]

cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")

# Obliczenie metryk
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)


cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla 'yes'):", round(precision, 2),
    "\nRecall (dla 'yes'):", round(recall, 2),
    "\nSpecificity (dla 'yes'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")


#' ## Wizualizacja metryk, podział STRATYFIKOWANY
## Wizualizacja metryk, podział STRATYFIKOWANY ----


# Przygotowanie danych do wykresu
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)
)


ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




# Przygotowanie do wizualizacji macierzy pomyłek
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")


# Oznaczenie poprawności klasyfikacji (Correct / Incorrect)
confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")


# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "#52B788",
  confusion_df$Label == "True Negative (TN)" ~ "#52B788",
  confusion_df$Label == "False Positive (FP)" ~ "#FFD166",
  confusion_df$Label == "False Negative (FN)" ~ "#EF476F"
)


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
# z przypisanymi kolorami i etykietami
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white", linewidth=1.5) +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 4, fontface = "bold", color = "#2B2D42") +
  scale_fill_identity(guide = "legend",
                      breaks = c("#52B788", "#FFD166", "#EF476F"),
                      labels = c("TP / TN (Poprawnie)",
                                 "False Positive (Błąd I typu)",
                                 "False Negative (Błąd II typu)"),
                      name = "Wynik klasyfikacji") +
  labs(title = "Macierz Pomyłek (Confusion Matrix)") +
  theme_minimal(base_size = 14)
