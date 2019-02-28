library(PNADcIBGE)
library(survey)
library(dplyr)

#Variáveis a serem puxadas
UF <- "UF"
sexo <- "V2007"
dia_nasc <- "V2008"
mes_nasc <- "V20081"
ano_nasc <- "V20082"
rend <- "VD4020" # Rendimento mensal efetivo de todos os trabalhos
# para pessoas de 14 anos ou mais de idade (apenas 
# para pessoas que receberam em dinheiro, produtos ou mercadorias
# em qualquer trabalho)
npessoas_dom <- "VD2003" # Número de componentes do domicílio (exclusive as
# pessoas cuja condição no domicílio era pensionista, empregado
# doméstico ou parente do empregado doméstico)
var <- c("UF", sexo, dia_nasc, mes_nasc, ano_nasc, rend, npessoas_dom)

sub_var <- c("UF") #Subset variables
condicionais <- c("Distrito Federal")
ana_var <- c(ano_nasc, sexo, rend) #Analysis variables

#pindorama_analysis <- function(sub_var, ana_var, match){
var <- c(sub_var, ana_var)
ano <- 2018

#Nomes fixos de variáveis utilizados mais a frente
v_renda <- "VD4020"
v_ano_nasc <- "V20082"

#Download dos dados da PNAD
dado_b <- get_pnadc(year = 2018,
                    quarter = 3,
                    vars = var,
                    design = FALSE,
                    labels = TRUE)

#Classificações
if(v_renda %in% names(dado_b)){
        faixa_rend <- function(rend, SM = 937){
        case_when(rend <= 2*SM ~ "E",
                  rend > 2*SM & rend <= 4*SM ~ "D",
                  rend > 4*SM & rend <= 10*SM ~ "C",
                  rend > 10*SM & rend <= 20*SM ~ "B",
                  rend > 20*SM ~ "A",
                  TRUE ~ NA_character_)
        }
        dado_b <- mutate(dado_b, Faixa.Renda = faixa_rend(v_renda))
        ana_var <- replace(ana_var,
                           list = grep(ana_var, pattern = v_renda),
                           values = "Faixa.Renda")
}
if(v_ano_nasc %in% names(dado_b)){
        faixa_idade <- function(num_ano_nasc, ano){
        idade <- ano - num_ano_nasc
        case_when(idade < 15 ~ "0 a 14 anos",
                  idade >= 15 & idade <= 19 ~ "15 a 19 anos",
                  idade >= 20 & idade <= 24 ~ "20 a 24 anos",
                  idade >= 25 & idade <= 29 ~ "25 a 29 anos",
                  idade >= 30 & idade <= 34 ~ "30 a 34 anos",
                  idade >= 35 & idade <= 39 ~ "35 a 39 anos",
                  idade >= 40 & idade <= 44 ~ "40 a 44 anos",
                  idade >= 45 & idade <= 49 ~ "45 a 49 anos",
                  idade >= 50 & idade <= 54 ~ "50 a 54 anos",
                  idade >= 55 & idade <= 59 ~ "55 a 59 anos",
                  idade >= 60 & idade <= 200 ~ "60+",
                  idade == 9999 ~ NA_character_,
                  TRUE ~ "Erro de classificação da idade.")
        }
        dado_b <- mutate(dado_b,
                         Faixa.Idade = faixa_idade(as.numeric(V20082), ano))
        ana_var <- replace(ana_var,
                           list = grep(ana_var, pattern = v_ano_nasc),
                           values = "Faixa.Idade")
}

#Aplica design de pesquisa complexa
dado_c <- pnadc_design(dado_b)

#Filtro para o DF
dado_c <- subset(dado_c, any(sub_var == condicionais))

#Combina sexo com idade
s1 <- "interaction_data <- svytotal(~ interaction("
s2 <- paste(ana_var, collapse = ",")
s3 <- "), design = dado_c, na.rm = TRUE)"
s <- paste(s1,s2,s3, sep="", collapse = NULL)
eval(parse(text = s)) #interaction_data <- svytotal(~ interaction(ana_var), design = dado_c, na.rm = TRUE)

ana_var_regex <- gsub(ana_var, pattern = "\\.", replacement = "\\\\.")

s1 <- "interaction\\("
s2 <- paste(ana_var_regex, collapse = ", ")
pad <- V2007, Faixa\\.Idade, Faixa\\.Renda\\)" #Regex ser removido das strings
ftable(interaction_data) %>%
        as_data_frame() %>%
        filter(Var2 == "A") %>%
        select(-Var2) %>%
        mutate(Var1 = stringr::str_remove(string = Var1, pattern = pad)) %>%
        tidyr::separate(col = Var1, into = c("Sexo","Faixa.Idade","Classe.Social"), sep = "\\.") %>%
        group_by(Sexo, Faixa.Idade, Classe.Social) %>%
        summarise(Individuos = sum(n)) %>%
        ungroup() %>%
        mutate(Individuos = round(Individuos))
#}


