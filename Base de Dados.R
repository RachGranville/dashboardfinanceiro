# Carregar pacotes necessários
library(readr)
library(dplyr)
library(rvest)
library(stringr)
options(encoding = "UTF-8")

# Carregar o arquivo CSV com a lista de tickers / MUDE SEU DIRETÓRIO AQUI
acoes_b3 <- read.csv("C:/Users/User/Desktop/IntroducaoaoR/acoes-listadas-b3.csv")

# Base URL para acessar as informações detalhadas de cada ticker
base_url <- "https://www.dadosdemercado.com.br/acoes/"

# Criar um data frame para armazenar os resultados
dados_gerais <- data.frame(Empresa = character(), Ticker = character(), Setor = character(), stringsAsFactors = FALSE)

# Criar a barra de progresso
total_tickers <- nrow(acoes_b3)  # Número total de tickers
progress_bar <- txtProgressBar(min = 0, max = total_tickers, style = 3)  # Barra de progresso

# Loop para iterar sobre cada ticker e extrair as informações
for (i in 1:total_tickers) {
  
  # Atualizar a barra de progresso
  setTxtProgressBar(progress_bar, i)
  
  # Obter o ticker atual
  ticker <- acoes_b3$Ticker[i]
  
  # Construir a URL específica para o ticker
  url <- paste0(base_url, tolower(ticker))  # Converte o ticker para minúsculas, para uso no link
  
  # Ler a página do ticker e verificar a estrutura
  pagina <- tryCatch({
    read_html(url)
  }, error = function(e) {
    NULL  # Se ocorrer um erro, retorna NULL
  })
  
  # Verificar se a página foi carregada corretamente, para fins de back-testing
  if (is.null(pagina)) {
    next  # Pula para o próximo ticker se houver erro
  }
  
  # Extraindo o nome da empresa e a classificação setorial, usando o Xpath 
  empresa <- pagina %>% html_node("h1") %>% html_text(trim = TRUE)  # Extrai o nome da empresa
  setor <- pagina %>% html_node(xpath = "//div[span[contains(text(), 'Classificação setorial B3')]]/span[2]") %>% html_text(trim = TRUE)  # Extrai a classificação setorial
  
  # Verificar se os dados foram extraídos corretamente
  if (is.na(empresa) | is.na(setor)) {
    next  # Se não extrair corretamente, pula o ticker
  }
  
  # Adicionar os dados ao data frame 'dados_gerais'
  dados_gerais <- rbind(dados_gerais, data.frame(Empresa = empresa, Ticker = ticker, Setor = setor, stringsAsFactors = FALSE))
}

# Fechar a barra de progresso
close(progress_bar)

# Exibir os resultados
print(dados_gerais)

# Salvar os dados em um CSV
write.csv(dados_gerais, "C:/Users/User/Desktop/IntroducaoaoR/resultados.csv", row.names = FALSE)

# Carregar os arquivos CSV
resultados_tickers <- read_csv("C:/Users/User/Desktop/IntroducaoaoR/resultados.csv")

# Simplificar e padronizar os setores no arquivo 'resultados_tickers'
resultados_tickers <- resultados_tickers %>%
  mutate(Setor = tolower(str_split(Setor, "/", simplify = TRUE)[,1])) %>%  # Pegar a primeira palavra antes da barra
  mutate(Setor = str_trim(Setor))  # Remover espaços extras

# Leitura do o arquivo BDR.csv com delimitador ';' e codificação 'latin1'
dados_bdr <- read_delim("C:/Users/User/Desktop/IntroducaoaoR/BDR.csv", delim = ";", locale = locale(encoding = "latin1"))

# Criar o mapeamento de setores do BDR para o formato simplificado adotado pela B3
setor_mapeamento <- c(
  'conglomerado' = 'outros',
  'industrial' = 'bens industriais',
  'farmacêutico' = 'saúde',
  'equipamentos médicos' = 'saúde',
  'entretenimento' = 'consumo cíclico',
  'auto peças' = 'bens industriais',
  'financeiro' = 'financeiro',
  'utilidades' = 'utilidade pública',
  'materiais básicos' = 'materiais básicos',
  'locadora' = 'consumo cíclico',
  'comércio eletrônico' = 'consumo cíclico',
  'aviação' = 'bens industriais',
  'streaming' = 'comunicações',
  'comunicação' = 'comunicações',
  'perfuração' = 'petróleo, gás e biocombustíveis',
  'energia' = 'petróleo, gás e biocombustíveis',
  'biotecnologia' = 'saúde',
  'tecnologia' = 'tecnologia',
  'utilidade pública/energia elétrica' = 'utilidade pública',
  'produtos de uso pessoal e de limpeza' = 'consumo não cíclico',
  'bebidas/cervejas e refrigerantes' = 'consumo não cíclico',
  'alimentos diversos' = 'consumo não cíclico',
  'transporte marítimo' = 'bens industriais',
  'transporte rodoviário' = 'bens industriais',
  'material aeronáutico e de defesa' = 'bens industriais',
  'seguros' = 'financeiro',
  'consultoria' = 'bens industriais',
  'mineração' = 'materiais básicos',
  'construção' = 'materiais básicos',
  'produtos de consumo não duráveis' = 'consumo não cíclico',
  'prestador de serviços' = 'bens industriais',
  'serviços financeiros diversos' = 'financeiro',
  'comércio' = 'consumo cíclico',
  'bebidas' = 'consumo não cíclico',
  'hotelaria' = 'consumo cíclico',
  'lojas de departamento' = 'consumo cíclico',
  'automóvel' = 'bens industriais',
  'vestuário' = 'consumo cíclico',
  'alimentação, restaurantes' = 'consumo não cíclico',
  'beleza' = 'consumo não cíclico',
  'marketing' = 'comunicações',
  'pesquisas' = 'outros',
  'redes sociais' = 'comunicações',
  'assistência médica' = 'saúde',
  'tabaco' = 'consumo não cíclico',
  'telecomunicação' = 'comunicações',
  'agência governamental' = 'outros',
  'análise de dados' = 'tecnologia',
  'fabricante de eletrodomésticos' = 'bens industriais',
  'aeroespacial' = 'bens industriais',
  'telecomunicação' = 'comunicações',
  'imobiliário' = 'outros',
  'bens de consumo' = 'consumo não cíclico',
  'equipamentos veterinários' = 'saúde',
  'fabricante de brinquedos' = 'consumo cíclico',
  'higiene dental' = 'consumo não cíclico',
  'metais e siderurgia' = 'materiais básicos',
  'motocicletas' = 'bens industriais',
  'petróleo, gás e biocombustíveis/equipamentos e serviços' = 'petróleo, gás e biocombustíveis',
  'petróleo, gás e biocombustíveis/exploração, refino e distribuição' = 'petróleo, gás e biocombustíveis',
  'financeiro e outros/serviços financeiros diversos' = 'financeiro',
  'mineradora' = 'materiais básicos',
  'bens industriais/material de transporte/material aeronáutico e de defesa' = 'bens industriais',
  'financeiro e outros/intermediários financeiros/bancos' = 'financeiro',
  'financeiro e outros/serviços financeiros/gestão de recursos e investimentos' = 'financeiro',
  'petróleo' = 'petróleo, gás e biocombustíveis',
  'tecnologia da informação/programas e serviços' = 'tecnologia',
  'consumo não cíclico/bebidas/cervejas e refrigerantes' = 'consumo não cíclico',
  'financeiro e outros/holdings diversificadas' = 'financeiro',
  'higiene pessoal' = 'consumo não cíclico',
  'logística e transportes' = 'bens industriais',
  'logistica e transportes' = 'bens industriais',
  'saúde e biotecnologia' = 'saúde',
  'tecnologia da informação/computadores e equipamentos' = 'tecnologia',
  'comércio e distribuição' = 'consumo cíclico',
  'consumo cíclico/hoteis e restaurantes/restaurante e similares' = 'consumo cíclico',
  'consumo não cíclico/produtos de uso pessoal e de limpeza/produtos de uso pessoal' = 'consumo não cíclico',
  'fabricante de eletrodomésticos' = 'bens industriais',
  'financeiro e outros/previdência e seguros/seguradoras' = 'financeiro',
  'saúde/medicamentos e outros produtos/medicamentos e outros produtos' = 'saúde',
  'agência governamental - pequeno prazo' = 'outros',
  'bens industriais/máquinas e equipamentos/máq. e equip. construção e agrícolas' = 'bens industriais',
  'bens industriais/transporte/transporte rodoviário' = 'bens industriais',
  'comunicações/mídia/produção e difusão de filmes e programas' = 'comunicações',
  'consumo cíclico/comércio/produtos diversos' = 'consumo cíclico',
  'consumo cíclico/tecidos, vestuário e calçados/calcados' = 'consumo cíclico',
  'consumo não cíclico/alimentos/alimentos diversos' = 'consumo não cíclico',
  'consumo não cíclico/comércio e distribuição/alimentos' = 'consumo não cíclico',
  'materiais básicos/mineração/minerais metálicos' = 'materiais básicos',
  'metalurgia e siderurgia' = 'materiais básicos',
  'medicina' = 'saúde',
  'pacífico asiático, ex japão' = 'saúde',
  'consumo cíclico/hoteis e restaurantes /restaurante e similares' = 'consumo cíclico',
  'fabricante de eletrodomésticos' = 'bens industriais',
  'consumo cíclico/tecidos, vestuário e calçados/calados' = 'consumo cíclico',
  'não classificados' = 'outros',
  'não classificado' = 'outros',
  'sem classificação' = 'outros',
  'siderurgia' = 'materiais básicos',
  'telecomunicações' = 'comunicações',
  'tecnologia da informação' = 'tecnologia',
  'outros' = 'outros',
  'pacífico asiático, ex Japão' = 'saúde',
  'fabricante de eletrodomésticos' = 'bens industriais',
  'fabricante de elétrodomésticos' = 'bens industriais',
  'consumo cíclico/tecidos, vestuário e calçados/calçados' = 'consumo cíclico'
)



# Aplicar o mapeamento de setores no arquivo BDR, mantendo os setores que não estão no mapeamento
dados_bdr <- dados_bdr %>%
  mutate(Setor = tolower(Setor)) %>%
  
  
  # Aplicar o mapeamento, mantendo valores não mapeados
  mutate(Setor = ifelse(Setor %in% names(setor_mapeamento), setor_mapeamento[Setor], Setor)) %>%
  
  # Tratar valores indefinidos como "-"
  mutate(Setor = ifelse(Setor == "-", "não classificado", Setor))

# Unir os dados dos tickers com os dados dos BDRs
dados_merged <- bind_rows(resultados_tickers, dados_bdr)

# Unificar as colunas 'Ticker' e 'Ticker ' em uma única coluna chamada 'Ticker'
dados_merged <- dados_merged %>%
  mutate(Ticker = coalesce(Ticker, `Ticker `)) %>%  # Substituir os valores de NA de 'Ticker' pelos de 'Ticker '
  
  # Remover a coluna redundante 'Ticker ' e qualquer outra desnecessária
  select(-'Ticker ', -'...4')

# Exibir as primeiras linhas do dataframe resultante
print(head(dados_merged))

# Salvar o dataframe consolidado em um arquivo CSV
write_csv(dados_merged, "C:/Users/User/Desktop/IntroducaoaoR/merged_dados.csv")

# Selecionar apenas as 4 primeiras colunas do dataframe final
dados_empresas <- dados_merged[, 1:4]

# Criar o mapeamento específico
setor_mapeamento_personalizado <- c(
  'tecnologia da informação' = 'tecnologia',
  'não classificados' = 'outros',
  'consumo cíclico/tecidos, vestuário e calçados/calçados' =  'consumo cíclico'
)

# Aplicar o mapeamento nos setores do dataframe 'dados_empresas'
dados_empresas <- dados_empresas %>%
  mutate(Setor = tolower(Setor)) %>%  # Padronizar para minúsculas
  mutate(Setor = ifelse(Setor %in% names(setor_mapeamento_personalizado),
                        setor_mapeamento_personalizado[Setor], Setor))

# Exibir as primeiras linhas das 4 primeiras colunas
print(head(dados_empresas))

write_csv(dados_empresas, "C:/Users/User/Desktop/IntroducaoaoR/dados_empresas.csv")

