# HydroStat Data Explorer

**Sistema de análise de dados hidrológicos**

HydroStat Data Explorer é uma aplicação pública desenvolvida em R/Shiny para visualizar, consultar, fazer triagem e analisar dados hidrológicos associados a estações da Agência Nacional de Águas e Saneamento Básico (ANA).

A aplicação combina uma base local compacta para mapas e produtos por estação com séries diárias fornecidas ou obtidas pelo próprio usuário durante a sessão.

## Estado do projeto

Este repositório contém o candidato à primeira publicação pública do aplicativo.

O aplicativo inclui:

- mapa e busca de estações;
- cadastro e disponibilidade de produtos por estação;
- medições de descarga;
- curvas-chave e diagnósticos de triagem;
- seções transversais;
- aquisição e análise de séries fluviométricas em sessão;
- aquisição e análise de séries pluviométricas em sessão;
- estatísticas mensais e anuais;
- máximas anuais, mínimas anuais e análise POT descritiva;
- downloads autenticados da API ANA iniciados pelo usuário.

## Aplicação local

### Requisitos

- R 4.6.0 para reproduzir o ambiente de publicação atual;
- pacotes registrados em `manifest.json` para a implantação;
- Git LFS para obter `exports/shiny_minimal.duckdb` a partir do repositório;
- acesso à internet para tiles cartográficos e para fluxos de download solicitados pelo usuário.

### Arquivos obrigatórios de execução

```text
app.R
R/
www/
exports/shiny_minimal.duckdb
exports/spatial_layers/shiny_spatial_layers.rds
```

### Execução

Abra o projeto no RStudio, instale previamente os pacotes necessários e execute:

```r
shiny::runApp()
```

Os scripts do aplicativo não instalam pacotes automaticamente.

## Dados incorporados e dados em sessão

A base `exports/shiny_minimal.duckdb` contém produtos derivados e compactos usados pelo mapa e pelos módulos por estação. Ela não contém as séries diárias completas de vazão, cota ou precipitação usadas nas análises em sessão.

Séries enviadas pelo usuário ou baixadas durante o uso são mantidas somente na sessão ativa. O aplicativo não as grava no DuckDB, no repositório ou em cache persistente.

Consulte:

- [`DATA_NOTICE.md`](DATA_NOTICE.md);
- [`PRIVACY.md`](PRIVACY.md);
- [`docs/DATA_SOURCES_AND_LIMITATIONS.md`](docs/DATA_SOURCES_AND_LIMITATIONS.md).

## Credenciais e token da ANA

O aplicativo nunca usa credenciais do autor do projeto.

Quando o usuário escolhe o download autenticado:

1. identificador e senha são usados somente para solicitar um token à ANA;
2. os campos são limpos após a autenticação;
3. o token permanece somente na memória da sessão;
4. token, credenciais, respostas parciais e séries baixadas não são persistidos pelo aplicativo;
5. o download pode ser retomado na mesma sessão após a renovação do token.

Não abra issues contendo CPF/CNPJ, senhas, tokens, cabeçalhos de autorização ou arquivos privados.

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
├── exports/                   Base e camada espacial de publicação
├── pipeline/                  Pipeline público de reconstrução
├── docs/                      Documentação técnica e decisões
├── tools/                     Ferramentas de publicação e validação
├── manifest.json              Dependências do Posit Connect Cloud
├── CITATION.cff
├── LICENSE
├── DATA_NOTICE.md
├── PRIVACY.md
├── SECURITY.md
└── CONTRIBUTING.md
```

A pasta `pipeline/` é pública para transparência e reprodutibilidade, mas não integra o bundle de execução do aplicativo.

## Reconstrução dos produtos

O pipeline completo depende de dados locais não incluídos no repositório, credenciais próprias da ANA e pacotes adicionais. Consulte [`pipeline/README_pipeline.md`](pipeline/README_pipeline.md).

Não execute scripts de aquisição sem revisar os parâmetros, os caminhos locais e as regras de acesso da ANA.

## Implantação

O alvo inicial é o Posit Connect Cloud. O arquivo `manifest.json` deve permanecer no mesmo diretório de `app.R`.

Instruções de preparação, geração do manifesto e validação estão em [`docs/DEPLOYMENT.md`](docs/DEPLOYMENT.md).

## Como citar

Use os metadados em [`CITATION.cff`](CITATION.cff). O GitHub também apresentará a opção “Cite this repository”.

## Contribuições e problemas

Consulte [`CONTRIBUTING.md`](CONTRIBUTING.md). Relatos devem conter passos reproduzíveis e nunca dados sensíveis.

## Licença

O código-fonte é disponibilizado sob a licença MIT. Consulte [`LICENSE`](LICENSE).

A licença MIT não relicencia automaticamente dados da ANA, produtos derivados, marcas, documentação de terceiros ou serviços externos. Consulte [`DATA_NOTICE.md`](DATA_NOTICE.md).
