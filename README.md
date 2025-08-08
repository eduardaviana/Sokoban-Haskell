# Sokoban-Haskell

gif do nosso joguinho :)

## Descrição do Projeto

Este projeto é uma implementação em terminal do famoso jogo de quebra-cabeças e lógica, **Sokoban**, utilizando a linguagem de programação funcional Haskell. O objetivo principal foi criar uma versão interativa e modular do jogo, permitindo aos usuários escolher diferentes níveis de dificuldade através de um menu inicial e jogar diretamente no terminal.

## Como executar em sua máquina:

Para compilar e rodar este projeto, você precisará ter o Haskell e o Cabal instalados em seu sistema.

1. Acesse este link para instalar 
```
https://www.haskell.org/ghcup/ 
```
2. As versoes utilizadas nesse projeto são 
```
cabal 2.4
```
3. Instalar Dependências e Compilar

Se você não for usuario de linux ou não tiver a wsl instalda em sua máquina precisara fazer uma modificação dentro do IO.hs tirando os caracteres especiais (não compilados em windows) e colocando os que estão comentados. 
```
Troque 𖠋 por P, '≢' troque por =, '≡' troque por #. 
```

Na raiz do seu projeto, execute os comandos:
```
cabal update
```
```
cabal build
```
4. Executar jogo 

Após a compilação bem-sucedida, você pode iniciar o jogo, execute o comando: 
```
cabal run
```
## Estrutura do Repositorio

O projeto é organizado modularmente para separar as responsabilidades e facilitar a manutenção, seguindo a seguinte estrutura:

* `app/Main.hs`: Ponto de entrada da aplicação, responsável por iniciar o menu principal do jogo.
* `data/maps/`: Contém os arquivos **JSON** dos mapas de jogo.
* `src/Game/`: Contém os módulos da biblioteca do projeto, onde a lógica central do jogo é desenvolvida.
    * `GameLoop.hs`: Gerencia o loop principal do jogo, incluindo a seleção de fase e o processamento de comandos do usuário;
    * `IO.hs`: Contém funções relacionadas a operações de entrada e saída, como leitura de caracteres únicos do terminal e limpeza de tela;
    * `Logic.hs`: Contém a lógica central do jogo;
    * `SokobanMap.hs`: Lida com a estrutura do mapa, verificação de limites e o carregamento de mapas via JSON;
    * `Types.hs`: Definições de tipos de dados fundamentais e funções básicas de conversão.
* `test/`: 
* `Sokoban-Haskell.cabal`: Arquivo de configuração principal do projeto Cabal, que gerencia as dependências e o processo de build.


