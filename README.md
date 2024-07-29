# RHCgen

O RHCgen é um pacote desenvolvido para a linguagem de programação R, concebido com o objetivo de automatizar e padronizar o processamento de dados provenientes do Registro Hospitalar de Câncer (RHC) no Brasil. Este pacote foi criado para abordar os desafios inerentes à manipulação de grandes volumes de dados heterogêneos, característicos dos registros hospitalares de câncer, proporcionando uma ferramenta eficiente, reprodutível e acessível para pesquisadores e profissionais de saúde.

---

### Funções
O RHCgen é um pacote de software para R que automatiza o processamento e transformação de dados do Registro Hospitalar de Câncer no Brasil. Ele fornece um conjunto de funções que permitem:
1. **Leitura e combinação de arquivos**: Facilita a leitura de múltiplos arquivos DBF do RHC e os combina em um único dataframe, assegurando a consistência das colunas.
2. **Renomeação de colunas**: Simplifica a leitura e análise dos dados através da renomeação de colunas com base em mapeamentos predefinidos.
3. **Modificação de tipos de variáveis**: Garante que cada coluna do dataframe tenha o tipo de dado apropriado para análises subsequentes.
4. **Recodificação de variáveis**: Permite a transformação de variáveis categóricas conforme os padrões estabelecidos no dicionário de variáveis do RHC.
5. **Análise de completude**: Avalia a completude dos dados, identificando e quantificando dados ausentes, e gera relatórios visuais para facilitar a interpretação.
6. **Filtragem e construção de bases de dados**: Automatiza a filtragem de dados e a construção de bases de dados finalizadas, prontos para análises detalhadas.

---

### Instalação

### Versão de desenvolvimento do GitHub

```r
# install.packages("remotes")
remotes::install_github("andersonlineu/RHCgen")
```

---

### Dúvidas e sugestões

Email: andersonlineu@gmail.com
