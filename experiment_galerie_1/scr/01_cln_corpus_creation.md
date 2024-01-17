# 01_cln_corpus_creation

## load packages

``` r
library(tidyverse)
```

    Warning: package 'ggplot2' was built under R version 4.3.1

    Warning: package 'lubridate' was built under R version 4.3.1

``` r
library(tidytext)
```

## load data

``` r
fh <- list.files("../corpus_raw/", full.names = T)

t <- tibble(
  path = fh,
  text = sapply(path, read_file)
)
```

## cleaning

### tokenisation

``` r
tokens <- t %>% 
  # separate conjunctions such as j'ai -> j ai 
  mutate(text = str_replace_all(text, "(\\w)'|’(\\w)", "\\1' \\2")) %>% 
  
  unnest_tokens(input = text, output = word, token = "words") %>% 
  filter(!str_detect(word, "\\d+")) 

head(tokens)
```

    # A tibble: 6 × 2
      path                                                                     word 
      <chr>                                                                    <chr>
    1 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… de   
    2 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… af   
    3 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… erg  
    4 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… me   
    5 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… fur  
    6 ../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer… un   

### mfw cleaning

``` r
tokens %>% 
  filter(nchar(word) > 1) %>% 
  count(word, sort = T) %>% head(300)
```

    # A tibble: 300 × 2
       word      n
       <chr> <int>
     1 de    37882
     2 la    23416
     3 les   17567
     4 le    17162
     5 que   13245
     6 il    12650
     7 des   11941
     8 qui   10125
     9 qu     9411
    10 en     9084
    # ℹ 290 more rows

``` r
tokens_cln <- tokens %>% 
  filter(nchar(word) > 1) %>% 
  mutate(word = str_replace_all(word, "^fa$", "sa"),
         word = str_replace_all(word, "^fur$", "sur"),
         word = str_replace_all(word, "^eft$", "est"),
         word = str_replace_all(word, "^fe$", "se"),
         word = str_replace_all(word, "^fi$", "si"),
         word = str_replace_all(word, "^fes$", "ses"),
         word = str_replace_all(word, "^fon$", "son"),
         word = str_replace_all(word, "^fans$", "sans"),
         word = str_replace_all(word, "^fol$", "sol"),
         word = str_replace_all(word, "^foit$", "soit"),
         word = str_replace_all(word, "^fous$", "sous"),
         word = str_replace_all(word, "^fera$", "sera"),
         word = str_replace_all(word, "^feroit$", "seroit"),
         word = str_replace_all(word, "^mefmer$", "mesmer"),
         word = str_replace_all(word, "^ut$", "et"),
         
         # questionable replacement:
         word = str_replace_all(word, "^font$", "sont")
  ) %>% 
  filter(!str_detect(word, "Digitized|by|knOOQLe|CnOOQie|Galerie|universelle|galerie|Universelle|Google"))
```

``` r
corpus_cln <- tokens_cln %>% 
  group_by(path) %>% 
  mutate(text = paste(word, collapse = " ")) %>% 
  ungroup() %>% 
  select(-word) %>% 
  distinct() %>% 
  mutate(path_new = str_replace(path, "corpus_raw/", "corpus_texts/"))

glimpse(corpus_cln)
```

    Rows: 37
    Columns: 3
    $ path     <chr> "../corpus_raw//Bergasse_1785_289-Observations sur un écrit d…
    $ text     <chr> "de af erg me sur un ecrit du do afe yj jer lettre de invente…
    $ path_new <chr> "../corpus_texts//Bergasse_1785_289-Observations sur un écrit…

## save corpus_texts/

``` r
for (i in 1:nrow(corpus_cln)) {
  write_file(corpus_cln$text[i], file = corpus_cln$path_new[i])
}
```

## corpus_authors/

``` r
glimpse(corpus_cln)
```

    Rows: 37
    Columns: 3
    $ path     <chr> "../corpus_raw//Bergasse_1785_289-Observations sur un écrit d…
    $ text     <chr> "de af erg me sur un ecrit du do afe yj jer lettre de invente…
    $ path_new <chr> "../corpus_texts//Bergasse_1785_289-Observations sur un écrit…

Extract author’s names from the metadata & filter texts in question

``` r
corpus_authors <- corpus_cln %>% 
  # create a column with author's name
  mutate(author = str_remove_all(path, "\\.\\./corpus_raw//"),
         author = str_extract(author, "^.*?_"),
         author = str_remove_all(author, "_")) %>% 
  # filter only ground truth authors
  filter(!author %in% c("G1785", "G1787")) 

length(unique(corpus_authors$author))
```

    [1] 11

### large texts

Count number of tokens in each text & select authors which texts should
be trimmed

``` r
n_tokens_text <- corpus_authors %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  count(path, author, sort = T) 

n_tokens_text
```

    # A tibble: 28 × 3
       path                                                            author      n
       <chr>                                                           <chr>   <int>
     1 ../corpus_raw//D'Alembert_1779_281-Éloges lus dans les séances… D'Ale… 102334
     2 ../corpus_raw//Delisle-de-Sales_1777_308-De la philosophie de … Delis…  59846
     3 ../corpus_raw//Delisle-de-Sales_1779_314-Histoire nouvelle de … Delis…  58131
     4 ../corpus_raw//Marmontel_1765_355-Contes moraux.txt             Marmo…  54578
     5 ../corpus_raw//Marmontel_1777_391-Les Incas, ou La destruction… Marmo…  50986
     6 ../corpus_raw//Delisle-de-Sales_1773_313-Essai philosophique s… Delis…  43603
     7 ../corpus_raw//D'Alembert_1752_274-Élemens de musique.txt       D'Ale…  36810
     8 ../corpus_raw//Garat_1778_316-Eloge de Michel de L'Hôpital.txt  Garat   34930
     9 ../corpus_raw//Sieyes_1789_169-Qu'est-ce que le Tiers-État.txt  Sieyes  31158
    10 ../corpus_raw//La-Harpe_1797_349-Réfutation du Livre de l'Espr… La-Ha…  31054
    # ℹ 18 more rows

``` r
large_texts <- n_tokens_text %>% 
  filter(n > 15000) %>%  # filter texts with more than 15,000 words
  pull(path)
  
large_texts
```

     [1] "../corpus_raw//D'Alembert_1779_281-Éloges lus dans les séances publiques.txt"               
     [2] "../corpus_raw//Delisle-de-Sales_1777_308-De la philosophie de la nature.txt"                
     [3] "../corpus_raw//Delisle-de-Sales_1779_314-Histoire nouvelle de tous les peuples du monde.txt"
     [4] "../corpus_raw//Marmontel_1765_355-Contes moraux.txt"                                        
     [5] "../corpus_raw//Marmontel_1777_391-Les Incas, ou La destruction de l'empire du Pérou.txt"    
     [6] "../corpus_raw//Delisle-de-Sales_1773_313-Essai philosophique sur le corps humain.txt"       
     [7] "../corpus_raw//D'Alembert_1752_274-Élemens de musique.txt"                                  
     [8] "../corpus_raw//Garat_1778_316-Eloge de Michel de L'Hôpital.txt"                             
     [9] "../corpus_raw//Sieyes_1789_169-Qu'est-ce que le Tiers-État.txt"                             
    [10] "../corpus_raw//La-Harpe_1797_349-Réfutation du Livre de l'Esprit.txt"                       
    [11] "../corpus_raw//Condorcet_1782_101-2_loge de M. le comte de Maurepas.txt"                    
    [12] "../corpus_raw//Bergasse_1785_289-Observations sur un écrit du Dr Mesmer.txt"                

### reshuffling

### large texts sampling

Select smaller samples from large texts

``` r
authors_large_texts_s <- corpus_authors %>% 
  filter(path %in% large_texts) %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  group_by(path) %>% 
  sample_n(size = 15000) 
```

### all texts samples

``` r
# glimpse(corpus_authors)
# glimpse(authors_large_texts_s)

corpus_authors_merged <- corpus_authors %>% 
  filter(!path %in% large_texts) %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  rbind(authors_large_texts_s) %>% 
  group_by(author) %>% 
  do(sample_n(., size = nrow(.))) %>% 
  summarise(text = paste(!!sym("word"), collapse = " ")) %>% 
  mutate(path = paste0("../corpus_authors//", author, ".txt"))

glimpse(corpus_authors_merged)
```

    Rows: 11
    Columns: 3
    $ author <chr> "Bergasse", "Brissot", "Condorcet", "D'Alembert", "Delisle-de-S…
    $ text   <chr> "lui réflexioas te co fois vous vérité comn dans lettres qu de …
    $ path   <chr> "../corpus_authors//Bergasse.txt", "../corpus_authors//Brissot.…

Number of tokens by each author

``` r
corpus_authors_merged %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  count(author, sort = T)
```

    # A tibble: 11 × 2
       author               n
       <chr>            <int>
     1 Delisle-de-Sales 45000
     2 D'Alembert       39512
     3 Marmontel        37610
     4 Condorcet        29779
     5 Garat            25168
     6 Sieyes           24810
     7 Bergasse         24034
     8 La-Harpe         22623
     9 Brissot          19745
    10 Villar           12400
    11 Gouges           11423

## save authors’ reshuffled texts

``` r
for (i in 1:nrow(corpus_authors_merged)) {
  write_file(file = corpus_authors_merged$path[i], corpus_authors_merged$text[i])
}
```
