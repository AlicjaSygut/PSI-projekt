
# install.packages("RColorBrewer")
# install.packages("tm")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("wordcloud")
# install.packages("ggplot")
# install.packages("ggthemes")

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
                                        "chase", "wells", "capital", "boa", "citi", "morgan", "jp"))

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


#' ---
#' title: "Modelowanie tematów LDA"
#' author: " "
#' date:   " "
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
#'     code_folding: show    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)



#' # 0. Funkcja top_terms_by_topic_LDA
# 0. Funkcja top_terms_by_topic_LDA ----
# która wczytuje tekst 
# (wektor lub kolumna tekstowa z ramki danych)
# i wizualizuje słowa o największej informatywności
# przy metody użyciu LDA
# dla wyznaczonej liczby tematów



top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k tematów
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usuń wszystkie puste wiersze w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i) # pobierz indeks każdej unikalnej wartości
  DTM <- DTM[unique_indexes,]    # pobierz z DTM podzbiór tylko tych unikalnych indeksów
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności

    
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
  
  
}







                               

#' # Dane tekstowe
# Dane tekstowe ----

# Ustaw Working Directory!
# Załaduj dokumenty z folderu
docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj ścieżkę
# np.: docs <- DirSource("C:/User/Documents/textfolder2")


# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje się w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
# inspect(corpus)


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][7:9]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", 
                                        "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", 
                                        "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug", 
                                        "emily", "emilys", "matt", "matts", "steve", "steves", "chuck", "chucks",
                                        "joel", "joels", "mckenna", "gabriel", "gabriels", "erin", "erins",
                                        "dane", "danes", "george", "georges", "marshall", "marshalls",
                                        "cliff", "cliffs", "sathyamurthys", "robert", "roberts", "elsa", "elsas", "laura", "lauras", "ray", "rays",
                                        "throw", "alex", "alexs", "angela", "angelas", "garrett", "garrets",
                                        "sam", "sams", "michael", "michaels", "soren", "sorens", "deepika", "sergey", "sergeys", "bullock", "bullocks",
                                        "felicity", "felicitys", "victoria", "victorias", "madeline", "madelines", "andrew", "andrews",
                                        "hendrix", "hendrixs", "powell", "glenn", "glenns"
                                        ))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]



# Decyzja dotycząca korpusu ----
# do dalszej analizy użyj:
#
# - corpus (oryginalny, bez stemmingu)
#




#' # Tokenizacja
# Tokenizacja ----



# Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))



#' # 4. Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



#' # UCZENIE MASZYNOWE NIENADZOROWANE
# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)




#' # Modelowanie tematów: ukryta alokacja Dirichleta
# Modelowanie tematów: ukryta alokacja Dirichleta (LDA) ----




# Rysuj dziesięć słów 
# o największej informatywności według tematu
# dla wyznaczonej liczby tematów 


# Dobór liczby tematów
number_of_topics = 2
top_terms_by_topic_LDA(tdm_df$word)


# Zmień wyznaczoną liczbę tematów
number_of_topics = 3
top_terms_by_topic_LDA(tdm_df$word)


# Zmień wyznaczoną liczbę tematów
number_of_topics = 4
top_terms_by_topic_LDA(tdm_df$word)


# Zmień wyznaczoną liczbę tematów
number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)

