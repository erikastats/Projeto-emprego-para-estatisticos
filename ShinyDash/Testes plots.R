Dados = read_csv("Dados.csv")
DadosUnicos = Dados %>% distinct(idVaga, Cidade, .keep_all = T)
Requisitos = names(DadosUnicos)[14:39] %>% str_replace_all("_", " ")

install.packages("")
######## Rede de Requisitos ############

install.packages("igraph")
install.packages("networkD3")

library(igraph)
library(networkD3)
library(tidyverse)
library(magrittr)


# create a dataset:
data <- data.frame(
  from=c("A", "A", "B", "D", "C", "D", "E", "B", "C", "D", "K", "A", "M"),
  to=c("B", "E", "F", "A", "C", "A", "B", "Z", "A", "C", "A", "B", "K")
)

# Plot
p <- simpleNetwork(data, height="100px", width="100px")
p

DadosR = DadosUnicos %>% filter(R == TRUE)

requi = sapply(DadosR[,14:39],function(x){sum(x, na.rm = T)}) %>% reshape2::melt()
requi %<>% mutate(Requi = row.names(requi))

data = data.frame(
  from = "R",
  to = requi$Requi
)

simpleNetwork(data, height="100px", width="100px", fontSize = requi$value)


#devtools::install_github("mattflor/chorddiag")
library(chorddiag)

m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)

# A vector of 4 colors for 4 groups
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")

# Build the chord diagram:
p <- chorddiag(m, groupColors = groupColors, groupnamePadding = 20)