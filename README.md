# HydroStat Data Explorer

**Sistema de análise de dados hidrológicos**

[![R 4.6.0](https://img.shields.io/badge/R-4.6.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-app-0099F9)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Status](https://img.shields.io/badge/status-public-success)](https://019eca91-f20c-8c76-338c-88342a355318.share.connect.posit.cloud/)

> **[Abrir a aplicação online](https://019eca91-f20c-8c76-338c-88342a355318.share.connect.posit.cloud/)**
> Também é possível baixar este repositório e executar a aplicação localmente.

HydroStat Data Explorer é uma aplicação pública desenvolvida em R/Shiny para visualizar, consultar, fazer triagem e analisar dados hidrológicos associados a estações da Agência Nacional de Águas e Saneamento Básico (ANA).

A aplicação combina produtos compactos incorporados ao repositório — utilizados no mapa e nos módulos por estação — com séries diárias fornecidas ou obtidas pelo próprio usuário durante a sessão.

## Principais recursos

- mapa, filtros e busca de estações;
- cadastro e disponibilidade de produtos por estação;
- medições de descarga;
- curvas-chave e diagnósticos de triagem;
- seções transversais;
- upload e análise de séries fluviométricas;
- upload e análise de séries pluviométricas;
- estatísticas mensais e anuais;
- máximas anuais, mínimas anuais e análise POT descritiva;
- downloads autenticados da API ANA iniciados pelo usuário;
- exportação de tabelas e resultados em CSV.

## Formas de uso

### 1. Aplicação online

A versão pública está disponível em:

**[Abrir HydroStat Data Explorer](https://019eca91-f20c-8c76-338c-88342a355318.share.connect.posit.cloud/)**

Não é necessário instalar R para utilizar a versão online.

### 2. Execução local

A aplicação também pode ser executada diretamente a partir deste repositório.

#### Requisitos

- R 4.6.0 recomendado para reproduzir o ambiente atual;
- RStudio é opcional;
- acesso à internet para instalar pacotes, carregar tiles cartográficos e utilizar serviços externos da ANA;
- espaço temporário disponível para reconstruir o banco de publicação, com aproximadamente 149 MB.

**Git LFS não é necessário.** Os quatro arquivos que compõem o banco de publicação estão armazenados como arquivos Git normais no repositório.

#### Obter o projeto

Com Git:

```bash
git clone https://github.com/hydrostat/hydrostat-data-explorer.git
cd hydrostat-data-explorer
```

Também é possível usar **Code → Download ZIP** na página do GitHub e extrair todo o conteúdo do arquivo.

#### Instalar os pacotes de execução

Execute uma vez no R:

```r
runtime_packages <- c(
  "shiny",
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "ggplot2",
  "leaflet",
  "sf",
  "DT",
  "htmltools",
  "scales",
  "ragg",
  "digest",
  "tibble",
  "httr2",
  "jsonlite",
  "xml2",
  "plotly",
  "evd"
)

packages_to_install <- setdiff(
  runtime_packages,
  rownames(installed.packages())
)

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}
```

Os scripts da aplicação não instalam pacotes automaticamente.

#### Iniciar a aplicação

Abra `hydrostat-data-explorer.Rproj` no RStudio ou defina o diretório de trabalho como a raiz do repositório. Em seguida, execute:

```r
shiny::runApp()
```

O navegador deverá abrir a aplicação local. O endereço normalmente será semelhante a:

```text
http://127.0.0.1:xxxx
```

## Reconstrução automática do banco

O arquivo completo `exports/shiny_minimal.duckdb` não é incluído no Git.

O repositório contém:

```text
exports/database_parts/database_parts_manifest.csv
exports/database_parts/shiny_minimal.duckdb.part001
exports/database_parts/shiny_minimal.duckdb.part002
exports/database_parts/shiny_minimal.duckdb.part003
exports/database_parts/shiny_minimal.duckdb.part004
```

Durante a inicialização, a aplicação:

1. verifica a presença e o tamanho das quatro partes;
2. valida o SHA-256 de cada parte;
3. reconstrói o DuckDB em uma pasta temporária;
4. valida o tamanho e o SHA-256 do banco reconstruído;
5. abre o banco somente para leitura.

Propriedades validadas do banco atual:

```text
Tamanho: 149172224 bytes
SHA-256: fb3b9a0a2dd30f6d6f14a67548a3b3c51a6ad438d4f9bf4c86fcd2200fbc7756
Estações: 37584
```

A primeira inicialização de uma nova sessão R pode levar mais tempo por causa da validação e reconstrução do banco.

## Dados incorporados e dados da sessão

O banco de publicação contém produtos derivados e compactos usados pelo mapa e pelos módulos por estação. Ele não contém as séries diárias completas de vazão, cota ou precipitação utilizadas nas análises de séries temporais.

As séries enviadas pelo usuário ou baixadas durante o uso:

- permanecem somente na sessão ativa;
- não são gravadas no DuckDB;
- não são adicionadas ao repositório;
- não são armazenadas em cache persistente pela aplicação.

Consulte:

- [`DATA_NOTICE.md`](DATA_NOTICE.md);
- [`PRIVACY.md`](PRIVACY.md);
- [`docs/DATA_SOURCES_AND_LIMITATIONS.md`](docs/DATA_SOURCES_AND_LIMITATIONS.md).

## Credenciais e token da ANA

Não são necessárias credenciais para explorar os produtos já incorporados à aplicação.

Quando o usuário escolhe o download autenticado da API ANA:

1. o identificador e a senha são usados somente para solicitar um token;
2. os campos de credenciais são limpos após a autenticação;
3. o token permanece somente na memória da sessão;
4. credenciais, token, respostas parciais e séries baixadas não são persistidos;
5. o download pode ser retomado na mesma sessão após a renovação do token.

Não abra issues contendo CPF/CNPJ, senhas, tokens, cabeçalhos de autorização ou arquivos privados.

## Solução de problemas na execução local

### Pacote não encontrado

Execute novamente o bloco de instalação de pacotes e reinicie a sessão R.

### Partes do banco ausentes ou inválidas

Confirme a presença dos quatro arquivos `.part` e de `database_parts_manifest.csv` em:

```text
exports/database_parts/
```

Em caso de download incompleto, faça novamente o clone ou baixe um novo ZIP do repositório.

### O mapa-base não aparece

Os produtos e as camadas espaciais locais podem continuar disponíveis, mas os tiles do mapa-base dependem de conexão com a internet e do serviço externo utilizado.

### O download da ANA falha

A API ANA é uma dependência externa. Verifique a conexão, a validade do token e as mensagens apresentadas pela aplicação.

## Fonte e atribuição

Fonte principal:

> Agência Nacional de Águas e Saneamento Básico — ANA. HidroWebService e serviços hidrológicos relacionados.

Este projeto é independente e não representa um produto oficial, certificação ou classificação de qualidade da ANA.

Os diagnósticos e alertas apresentados são instrumentos de triagem e revisão visual. Eles não substituem a verificação dos dados originais, a documentação da ANA ou a avaliação de um profissional qualificado.

## Estrutura do repositório

```text
hydrostat-data-explorer/
├── app.R
├── R/                         Código de execução do Shiny
├── www/                       CSS e recursos estáticos
├── exports/                   Dados e camadas espaciais de publicação
├── pipeline/                  Pipeline público de reconstrução
├── docs/                      Documentação técnica e operacional
├── tools/                     Ferramentas de publicação e validação
├── manifest.json              Dependências da implantação
├── START_HERE.md              Guia rápido de uso
├── CITATION.cff
├── LICENSE
├── DATA_NOTICE.md
├── PRIVACY.md
├── SECURITY.md
└── CONTRIBUTING.md
```

A pasta `pipeline/` é disponibilizada para transparência e reprodutibilidade, mas não integra o bundle de execução da aplicação.

## Reconstrução dos produtos de publicação

O pipeline completo depende de dados locais não incluídos no repositório, credenciais próprias da ANA e pacotes adicionais.

Consulte [`pipeline/README_pipeline.md`](pipeline/README_pipeline.md).

Não execute scripts de aquisição sem revisar os parâmetros, os caminhos locais e as regras de acesso da ANA.

## Implantação e manutenção

A implantação pública atual utiliza Posit Connect Cloud.

As instruções destinadas à manutenção e à implantação estão em:

- [`docs/DEPLOYMENT.md`](docs/DEPLOYMENT.md).

Essas rotinas não são necessárias para executar a aplicação localmente.

## Como citar

Use os metadados disponíveis em [`CITATION.cff`](CITATION.cff). O GitHub também apresenta a opção **Cite this repository**.

## Contribuições e problemas

Consulte [`CONTRIBUTING.md`](CONTRIBUTING.md).

Relatos devem conter passos reproduzíveis e nunca dados sensíveis.

## Licença

O código-fonte é disponibilizado sob a licença MIT. Consulte [`LICENSE`](LICENSE).

A licença MIT não relicencia automaticamente dados da ANA, produtos derivados, marcas, documentação de terceiros ou serviços externos. Consulte [`DATA_NOTICE.md`](DATA_NOTICE.md).
