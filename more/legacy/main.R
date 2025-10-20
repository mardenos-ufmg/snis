library(reticulate)
use_virtualenv("old-versions/new/venv", required = TRUE)
source_python("old-versions/new/final.py")

df_features = readxl::read_excel("old-versions/new/DADOS_SCORE_21_09_refatoracao.xlsx", sheet="Planilha2")
df_features_dh = readxl::read_excel("old-versions/new/DADOS_SCORE_21_09_Pablo.xlsx", sheet="Planilha2")
df = readxl::read_excel("old-versions/new/dados tratados missing 5.xlsx")

Score(df,df_features,delta=0.05,normalizar_score=T,save_excel=F,rotacao='oblimin',verbose=T)

cidades =
  c(
    'Rio Doce',
    'Bom Sucesso',
    'Uberaba',
    'Mantena',
    'Papagaios',
    'Lagoa da Prata',
    'Carmópolis de Minas',
    'Patrocínio',
    'Monte Carmelo',
    'Machado',
    'Itaguara',
    'São José da Varginha',
    'Sacramento',
    'Japaraíba',
    'Arantina',
    'Caraí',
    'São Sebastião do Maranhão',
    'Setubinha',
    'São João da Ponte',
    'Presidente Bernardes',
    'São José do Jacuri',
    'Guaraciaba',
    'Luisburgo',
    'Serra Azul de Minas',
    'Icaraí de Minas',
    'Gonçalves',
    'Santo Antônio do Retiro',
    'Ladainha'
  )

cidades1 =
  c(
    "Uberlândia",
    "Araporã",
    "Divinópolis",
    "Pará de Minas",
    "Itabirito",
    "Caeté",
    "Cabeceira Grande",
    "Florestal",
    "Monjolos",
    "Pratinha"
  )

cidades2 = c("Nova Serrana")

munic = c()

for (i in cidades1) {
  munic =
    c(
      munic,
      df[df[['Nome_Município']]==i,]
      )
  
  munic.append()
}

score_filtred=df_score.iloc[munic]
score_filtred.insert(0, 'Ranking', range(1, 1 + len(score_filtred)))

###POR CONCEITO - TABELA SCORE FINAL E MUNICIPIO
ordenado = score_filtred.sort_values('Score_Medio', ascending=False)
ordenado.insert(0, 'Rank_media', range(1, 1 + len(ordenado)))
ordenado.sort_index(ascending=True, inplace=True)
#ordenado
