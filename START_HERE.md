# Comece aqui

O **HydroStat Data Explorer** pode ser utilizado de duas maneiras:

1. diretamente pela aplicação pública;
2. localmente, a partir deste repositório.

## Usar a aplicação online

Abra:

**[HydroStat Data Explorer](https://019eca91-f20c-8c76-338c-88342a355318.share.connect.posit.cloud/)**

Nenhuma instalação é necessária.

## Executar localmente

### 1. Obtenha o repositório

Com Git:

```bash
git clone https://github.com/hydrostat/hydrostat-data-explorer.git
cd hydrostat-data-explorer
```

Alternativamente, use **Code → Download ZIP** no GitHub e extraia todo o conteúdo.

Git LFS não é necessário.

### 2. Instale R e os pacotes

Recomenda-se R 4.6.0. RStudio é opcional.

Execute no R:

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

### 3. Inicie a aplicação

Abra `hydrostat-data-explorer.Rproj` ou defina o diretório de trabalho como a raiz do repositório.

Execute:

```r
shiny::runApp()
```

### 4. Aguarde a preparação do banco

O DuckDB completo não está armazenado como um único arquivo no Git.

Na inicialização, a aplicação valida as quatro partes presentes em `exports/database_parts/`, reconstrói o banco em uma pasta temporária, confirma seu SHA-256 e o abre somente para leitura.

A primeira inicialização de uma nova sessão R pode demorar um pouco mais.

## Arquivos necessários

Não remova:

```text
app.R
R/
www/
exports/database_parts/
exports/spatial_layers/shiny_spatial_layers.rds
```

Em `exports/database_parts/` devem existir:

```text
database_parts_manifest.csv
shiny_minimal.duckdb.part001
shiny_minimal.duckdb.part002
shiny_minimal.duckdb.part003
shiny_minimal.duckdb.part004
```

## Credenciais da ANA

Credenciais não são necessárias para consultar os produtos incorporados ao aplicativo.

Elas são solicitadas apenas quando o usuário escolhe um download autenticado da API ANA. Nesse caso, credenciais, token e dados baixados permanecem somente na sessão ativa e não são persistidos pela aplicação.

## Problemas comuns

### `there is no package called ...`

Instale o pacote indicado ou execute novamente o bloco de instalação.

### Erro relacionado às partes do banco

Confirme que o download do repositório foi concluído e que os quatro arquivos `.part` estão presentes.

### Mapa-base ausente

Verifique a conexão com a internet. Os tiles cartográficos são fornecidos por serviço externo.

### Falha em downloads da ANA

Verifique a conexão, o token e a mensagem apresentada na interface. A API ANA é uma dependência externa.

## Mais informações

Consulte o [`README.md`](README.md) para a descrição completa do projeto, dados, privacidade, limitações, citação e licença.

As instruções de manutenção e implantação ficam separadas em [`docs/DEPLOYMENT.md`](docs/DEPLOYMENT.md).
