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
## Estrutura do Projeto
O projeto foi refatorado para seguir uma arquitetura limpa, com cada módulo tendo uma única e clara responsabilidade.

* `app/Main.hs`: Ponto de entrada da aplicação. Gerencia apenas o menu inicial, delegando a execução do jogo para o Controller, GameLoop. É um módulo "porteiro", desacoplado da lógica do jogo.

* `data/maps/`: Contém os arquivos **JSON** dos mapas de jogo.

* `src/Game/`: Contém os módulos da biblioteca do projeto, onde a lógica central do jogo é desenvolvida.
    * `Types.hs`: Define todas as estruturas de dados centrais do projeto, como GameState, GameConfig, Action, Direction e Tile. É a fundação do nosso modelo de dados.

    * `Engine.hs`: O cérebro do jogo. Contém a lógica de regras pura e sem efeitos colaterais (I/O). Sua função principal, update, recebe um estado e uma ação e retorna o novo estado. É o "motor" testável que define como o mundo do jogo evolui.

    * `View.hs`: A camada de apresentação. Sua única tarefa é receber o estado do jogo (GameState) e desenhá-lo no terminal, com todos os painéis e formatação. Não contém nenhuma lógica de regras.

    * `GameLoop.hs`: O maestro da aplicação. Gerencia o fluxo e o loop do jogo, orquestrando os outros módulos: captura o input do usuário, envia ações para o Engine e passa o novo estado para a View renderizar.

    * `MapLoader`: Responsável por carregar e decodificar os arquivos de mapa .json, transformando-os nos tipos de dados que o jogo entende.

    * `Utils`: Módulo opcional para funções utilitárias genéricas (ex: capitalize, getCharInstant) que podem ser reutilizadas em diferentes partes do projeto.

* `test/`: 
* `Sokoban-Haskell.cabal`: Arquivo de configuração principal do projeto Cabal, que gerencia as dependências e o processo de build.


