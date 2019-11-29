#Abrir script para cargar datos, taboas e graficos
source(here::here("02_scripts/simca.R"),encoding = "UTF-8")
#Renderizar o arquivo "simca.rmd" que compila o arquivo o html do artigo
rmarkdown::render("04_rmd/simca.Rmd",encoding="UTF-8",
                  output_dir = "05_documentos")





