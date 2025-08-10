# Sokoban-Haskell

![Gameplay do Sokoban](https://github.com/eduardaviana/Sokoban-Haskell/blob/main/data/sokoban-haskell.gif)

## Descrição do Projeto

Este projeto é uma implementação em terminal do clássico jogo de lógica **Sokoban**, desenvolvida em **Haskell**.  
O jogador deve empurrar todas as caixas (`B`) até suas posições corretas (`x`) no mapa, usando o menor número de movimentos possível.

A aplicação foi projetada com foco em **modularidade** e **arquitetura limpa**, incluindo:
- Menu inicial para seleção de nível (**fácil**, **médio** ou **difícil**).
- Execução totalmente no terminal.
- Estrutura de código clara e de fácil manutenção.

---

## Como Executar

### 1. Instalar Haskell e Cabal
Baixe e instale via [GHCup](https://www.haskell.org/ghcup/).

> Versão utilizada neste projeto:  
> `cabal 2.4`

---

### 2. Ajuste para Windows
Se você **não** estiver em Linux ou **não** tiver WSL, será necessário editar o módulo para trocar caracteres especiais incompatíveis no Windows:

| Caractere Original | Substituir por |
|--------------------|----------------|
| `𖠋`               | `P`            |
| `≢`               | `=`            |
| `≡`               | `#`            |

---

### 3. Instalar Dependências e Compilar
Na raiz do projeto, execute:
```bash
cabal update
cabal build
```

---

### 4. Executar o Jogo
Após compilar, rode:
```bash
cabal run
```

---

## Estrutura do Projeto

```
Sokoban-Haskell/
├── app/
│   └── Main.hs          # Ponto de entrada; gerencia o menu inicial.
├── data/
│   └── maps/            # Mapas em formato JSON.
│       ├── facil.json
│       ├── medio.json
│       └── dificil.json
├── src/
│   ├── Game/            # Lógica central do jogo.
│   │   ├── Types.hs     # Tipos e estruturas de dados.
│   │   ├── Engine.hs    # Lógica pura (regras, atualização de estado).
│   │   ├── View.hs      # Renderização no terminal.
│   │   ├── GameLoop.hs  # Loop principal do jogo.
│   │   ├── MapLoader.hs # Carregamento e parsing dos mapas JSON.
│   │   └── Utils.hs     # Funções utilitárias.
├── Sokoban-Haskell.cabal# Configuração do projeto Cabal.
├── LICENSE              # Licença BSD 3-Clause.
└── CHANGELOG.md         # Registro de mudanças.
```

---

## 🛠 Arquitetura
O projeto segue **responsabilidade única** para cada módulo:
- **Main**: Exibe o menu inicial e inicia o loop do jogo.
- **Engine**: Coração da lógica, sem efeitos colaterais.
- **View**: Apenas exibe o estado atual do jogo.
- **GameLoop**: Controla input, atualização e renderização.
- **MapLoader**: Converte mapas JSON em estruturas do jogo.
- **Utils**: Funções auxiliares reutilizáveis.

---

## Equipe de Desenvolvimento
- João Antonio  
- Eduarda Viana  
- Tamires Santiago  
- Diogo Medeiros  
- Anthony Willy  

---

## 📜 Licença
Este projeto é distribuído sob a licença BSD 3-Clause. Consulte o arquivo `LICENSE` para mais informações.
