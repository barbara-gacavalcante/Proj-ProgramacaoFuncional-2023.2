import Data.List (transpose, findIndex)

-- Definição doo tipo enumerado para os jogadores, que permite que sejam comparados
-- e convertidos em string
data Player = Player1 | Player2 deriving (Eq, Show)

-- Definição do tipo sinônimo para o tabuleiro, utilizando uma lista de listas de caracteres
-- [char] = linhas; [[char]] = lista de linhas (colunas)
type Board = [[Char]]

-- Constantes para o número de linhas e colunas do tabuleiro, com dados padrões dos tabuleiros
numRows :: Int
numRows = 6
numCols :: Int
numCols = 7

-- Cria um tabuleiro vazio, preenchido com espaços
-- a função replicate (definida posteriormente) é usada aqui para criar uma linha de células vazias
-- e depois essas linhas são replicadas novamente, para formar o tabuleiro
emptyBoard :: Board
emptyBoard = replicate numRows (replicate numCols ' ')

-- Imprime o tabuleiro, linha por linha
printBoard :: Board -> IO ()
printBoard board = mapM_ printRow board

-- Imprime uma única linha do tabuleiro
printRow :: [Char] -> IO ()
printRow row = putStrLn $ " | " ++ concatMap (\x -> [x] ++ " | ") row

-- Tenta fazer um movimento no tabuleiro na coluna especificada pelo jogador
-- Primeiro, verifica se a coluna informada tá dentro dos limites do tabuleiro (maior que zero e menor que o número de colunas)
-- Usa findEmptyRow pra encontrar a primeira linha vazia na coluna a partir do fundo do tabuleiro
-- Se encontrar, aí chama replace2D pra colocar a peça na posição
makeMove :: Player -> Int -> Board -> Maybe Board
makeMove player col board
    | col < 0 || col >= numCols = Nothing
    | otherwise = case findEmptyRow col board of
        Just row -> Just (replace2D row col (piece player) board)
        Nothing  -> Nothing
    where
        piece Player1 = 'X'
        piece Player2 = 'O'
        findEmptyRow :: Int -> Board -> Maybe Int
        findEmptyRow col b = 
            let colData = map (!! col) b
            in findIndex (== ' ') (reverse colData) >>= \idx -> Just (numRows - 1 - idx)

-- Substitui um elemento em uma lista na posição 'n' com o valor correspondente ao jogador
-- se n == 0, vai substituir diretamente no cabeçalho da linha
-- se não, chama recursivamente ela mesma p/ resto da lista e decrementa até ser 0
replace :: Int -> a -> [a] -> [a]
replace n newVal (x:xs)
    | n == 0 = newVal : xs
    | otherwise = x : replace (n-1) newVal xs

-- Substitui um elemento em uma matriz (lista de listas - 2D)
-- usa replace pra substituir a linha inteira, e o elemento da coluna informada
-- com o valor do jogador
replace2D :: Int -> Int -> Char -> [[Char]] -> [[Char]]
replace2D n m newVal xs = replace n (replace m newVal (xs !! n)) xs

-- Avalia se o jogador atual ganhou verificando todas as linhas, colunas e diagonais do tabuleiro
checkWin :: Player -> Board -> Bool
checkWin player board = any (checkLine player) (rows ++ cols ++ diags)
    where
        rows = board
        cols = transpose board
        diags = diagonals board

-- Verifica se há uma linha contínua de quatro peças iguais
checkLine :: Player -> [Char] -> Bool
checkLine player line = any (== 4) $ map (length . filter (== piece)) (grouped 4 line)
    where piece = if player == Player1 then 'X' else 'O'

-- Agrupa elementos em sub-listas de tamanho n para criar todos os subconjuntos possíveis de 4 elementos numa lista
grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n list@(x:xs)
    | length list < n = []
    | otherwise = take n list : grouped n xs

-- Calcula todas as diagonais relevantes do tabuleiro
diagonals :: Board -> [[Char]]
diagonals b = concat [rightDiagonals b, leftDiagonals b]

-- Calcula diagonais p/ a direita e p/ cima
rightDiagonals :: Board -> [[Char]]
rightDiagonals b = [diagonal b x y (-1, 1) | x <- [0..numRows-1], y <- [0..numCols-1], x >= 3, y <= numCols-4]

-- Calcula diagonais p/ a esquerda e p/ cima
leftDiagonals :: Board -> [[Char]]
leftDiagonals b = [diagonal b x y (-1, -1) | x <- [0..numRows-1], y <- [0..numCols-1], x >= 3, y >= 3]

-- Função auxiliar para calcular uma diagonal a partir de um ponto inicial c/ direção específica
diagonal :: Board -> Int -> Int -> (Int, Int) -> [Char]
diagonal b startX startY (dx, dy) =
    takeWhileValid (startX, startY) (dx, dy) []
  where
    takeWhileValid (x, y) (dx, dy) acc
        | x < 0 || x >= numRows || y < 0 || y >= numCols = acc
        | otherwise = takeWhileValid (x + dx, y + dy) (dx, dy) (b !! x !! y : acc)

-- Função principal para iniciar o jogo
-- Exibe mensagem de boas vindas, os jogadores e o tabuleiro
playGame :: IO ()
playGame = do
    putStrLn "Bem-vindo ao Quatro em Linha!"
    putStrLn "Jogador 1: X | Jogador 2: O"
    playTurn Player1 emptyBoard

-- Função recursiva para gerenciar turnos dos jogadores
-- Exibe o estado atual do tabuleiro e controla o fluxo p/ identificar vitória ou empate
-- Primeiro mostra o tabuleiro e dps solicita a jogada do player
-- Converte a entrada em inteiro e chama makeMove
-- Mostra qual jogador venceu ou então empate se o tabuleiro tá cheio e sem vitórias
-- Se makeMove retorna Nothing, então exibe a mensagem de q a jogada é inválida
playTurn :: Player -> Board -> IO ()
playTurn player board = do
    putStrLn $ "\nTabuleiro atual:"
    printBoard board
    putStrLn $ "Jogador " ++ show player ++ ", faça sua jogada (coluna de 0 a " ++ show (numCols - 1) ++ "):"
    column <- getLine
    let col = read column :: Int
    case makeMove player col board of
        Just newBoard -> if checkWin player newBoard
                            then do
                                putStrLn $ "\nTabuleiro atual:"
                                printBoard newBoard
                                putStrLn $ "O jogador " ++ show player ++ " ganhou!"
                            else if all (\row -> all (/= ' ') row) newBoard
                                    then putStrLn "Empate!"
                                    else playTurn (nextPlayer player) newBoard
        Nothing -> do
            putStrLn "Jogada inválida. Por favor, escolha uma coluna válida."
            playTurn player board

-- Alterna entre os jogadores
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

-- Função principal que inicia o jogo
main :: IO ()
main = playGame
