
# bibliotecas
library(data.table)
library(tidyverse)
library(zoo)

# leitura do arquivo
df <- fread("trabalhos_rbep.csv", header = TRUE, encoding = "UTF-8")
# o output não é exatamente como previsto... a ideia era
# que a coluna de "link" e "edicao" estivessem toda preenchida


# problemas para criar "tibble"
# Error: Column `df` must be a 1d atomic vector or a list
# solução alternativa
df_tibble <- tibble("link" = df$link,
                    "edicao" = df$edicao,
                    "autor" = df$autor,
                    "paginas" = df$paginas,
                    "tipo_trabalho" = df$tipo_trabalho,
                    "titulo" = df$titulo)

# removendo os caracteres "\n" e "\t". o caracter "\" é um "escape caracter", 
# portanto, para removê-lo precisamos das barras invertidas duplas "\\"
# a função map_df é usada para aplicar esse procedimento em todas as colunas
# mais informações: pesquisa por tutoriais do pacote purrr
df_tibble <- df_tibble %>% 
  purrr::map_df(.f = str_remove_all, pattern = "\\n") %>% 
  purrr::map_df(.f = str_remove_all, pattern = "\\t")

df_tibble[df_tibble==''] <- NA

# correção manual
# um título ocupou duas linhas, vi quando estava explorando 
# a página da RBEP, mas este código abaixo poderia ajudar
# pois indica que na coluna de autor tem um campo NA
df_tibble %>% map_dbl(~sum(is.na(.)))

# substituímos manualmente e depois excluímos a linha
titulo_corrigir <- df_tibble$titulo[805:812]
df_tibble$titulo[804:811] <- titulo_corrigir
df_tibble <- df_tibble[-812,]

# agora substituindo todos os NA pela célula logo acima
# "replace na with last non na in R"
# https://ajitjohnson.com/replace-na-with-last-non-na-R/
# usando o pacote zoo e a função na.locf é necessário uma correção manual 
# na primeira linha, # pois no site da RBEP este trabalho não tem o número 
# da página que inicia e termina
df_tibble$paginas[1] <- "1-2"

df_tibble <- zoo::na.locf(df_tibble)

write_delim(x = df_tibble,
            path = "trabalhos_rbep_final.csv",
            delim = "|")

# Análise de palavra ------------------------------------------------------

# analisar a frequência das palavras na coluna "titulo"
# objetivo: verificar quais palavras são usadas para facilitar
# a pesquisa.

# usar o pacote tidytext
# o primeiro capítulo do livro "Text mining with R"
# https://www.tidytextmining.com/
# é suficiente
# explorar a variável "titulo" do data frame "df_tibble"

# Número de páginas -------------------------------------------------------

# determinar quantas páginas tem cada trabalho.
# a coluna "paginas" indica o início e o final de cada trabalho
# (em algumas edições)




  