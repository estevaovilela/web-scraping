

# Library -----------------------------------------------------------------

library(data.table)
library(tidyverse)
library(zoo)

# Reading -----------------------------------------------------------------

df <- fread("trabalhos_rbep.csv", header = TRUE, encoding = "UTF-8")
# o output não é exatamente como previsto... a ideia era
# que a coluna de "link" e "edicao" estivessem toda preenchida

# problemas para criar "tibble"
# Error: Column `df` must be a 1d atomic vector or a list
# solução alternativa
df_tibble <- tibble("link" = df$link,
                    "edicao" = df$edicao,
                    "tipo_trabalho" = df$tipo_trabalho,
                    "titulo" = df$titulo,
                    "autor" = df$autor,
                    "resumo" = df$resumo)

# Cleaning ----------------------------------------------------------------

# removendo os caracteres "\n" e "\t". o caracter "\" é um "escape caracter", 
# portanto, para removê-lo precisamos das barras invertidas duplas "\\"
# a função map_df é usada para aplicar esse procedimento em todas as colunas
# mais informações: pesquisa por tutoriais do pacote purrr
df_tibble <- df_tibble %>% 
  purrr::map_df(.f = str_remove_all, pattern = "\\n") %>% 
  purrr::map_df(.f = str_remove_all, pattern = "\\t")

df_tibble[df_tibble==''] <- NA

df_tibble %>% map_dbl(~sum(is.na(.)))

# links que não direcionam para nenhuma página, pois na sequência
# https://rebep.org.br/revista/article/view/i
# i [2, 1544) não há todos os trabalhos

df_tibble <- df_tibble %>% 
  # esses links não tem autor pois não há publicação
  filter(!is.na(autor)) %>% 
  # agora substituindo todos os NA pela célula logo acima
  # "replace na with last non na in R"
  # https://ajitjohnson.com/replace-na-with-last-non-na-R/
  # usando o pacote zoo e a função na.locf 
  mutate(link = zoo::na.locf(link),
         edicao = zoo::na.locf(edicao),
         tipo_trabalho = zoo::na.locf(tipo_trabalho),
         titulo = zoo::na.locf(titulo))

# publicação
df_publicacao <- df_tibble %>% 
  select(-autor, -resumo) %>% 
  distinct()

# autor
df_autor <- df_tibble %>% 
  select(link, autor)

# resumo
df_resumo <- df_tibble %>% 
  select(link, resumo) %>% 
  filter(!is.na(resumo))

# publicação completa com link, edição, tipo do trabalho,
# autor, título e resumo
df_publicacao_completo <- df_publicacao %>% 
  left_join(df_autor, by = c("link")) %>% 
  group_by(link, edicao, tipo_trabalho, titulo) %>% 
  summarise(autores = paste(autor, collapse = ", ")) %>%
  left_join(df_resumo, by = c("link")) %>% 
  group_by(link, edicao, tipo_trabalho, titulo, autores) %>% 
  summarise(resumo = paste(resumo, collapse = "/"))

# Output ------------------------------------------------------------------

# autor
write_delim(x = df_autor,
            path = "autor.txt",
            delim = "|")

# resumo
write_delim(x = df_resumo,
            path = "resumo.txt",
            delim = "|")

# publicação
write_delim(x = df_publicacao,
            path = "publicacao.txt",
            delim = "|")

# publicações completas
write_delim(x = df_publicacao_completo,
            path = "publicacao_completo.txt",
            delim = "|")


  