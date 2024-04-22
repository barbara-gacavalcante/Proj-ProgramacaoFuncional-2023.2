import Data.List (transpose, findIndex)

-- Define um tipo enumerado para os jogadores, que possibilita comparações e conversões para string
data Player = Player1 | Player2 deriving (Eq, Show)

-- Define um sinônimo de tipo para o tabuleiro, usando uma lista de listas de caracteres
type Board = [[Char]]

-- Constantes para o número de linhas e colunas do tabuleiro
numRows :: Int
numRows = 6
numCols :: Int
numCols = 7

-- Gera um tabuleiro vazio preenchido com espaços
emptyBoard :: Board
emptyBoard = replicate numRows (replicate numCols ' ')

-- Imprime o tabuleiro no console, linha por linha
printBoard :: Board -> IO ()
printBoard board = mapM_ printRow board

-- Auxiliar para imprimir uma única linha do tabuleiro
printRow :: [Char] -> IO ()
printRow row = putStrLn $ " | " ++ concatMap (\x -> [x] ++ " | ") row

-- Realiza uma jogada na coluna especificada, retornando um novo tabuleiro se possível
makeMove :: Player -> Int -> Board -> Maybe Board
makeMove player col board
    | col < 0 || col >= numCols = Nothing
    | otherwise = case findEmptyRow col board of
        Just row -> Just (replace2D row col (piece player) board)
        Nothing  -> Nothing
    where
        piece Player1 = 'X'
        piece Player2 = 'O'

-- Encontra a primeira linha vazia em uma coluna específica
findEmptyRow :: Int -> Board -> Maybe Int
findEmptyRow col b = 
    let colData = map (!! col) b
    in findIndex (== ' ') (reverse colData) >>= \idx -> Just (numRows - 1 - idx)

-- Substitui um elemento em uma lista na posição especificada
replace :: Int -> a -> [a] -> [a]
replace n newVal (x:xs)
    | n == 0 = newVal : xs
    | otherwise = x : replace (n-1) newVal xs

-- Substitui um elemento em uma matriz na posição especificada
replace2D :: Int -> Int -> Char -> [[Char]] -> [[Char]]
replace2D n m newVal xs = replace n (replace m newVal (xs !! n)) xs

-- Avalia se o jogador atual ganhou verificando linhas, colunas e diagonais do tabuleiro
checkWin :: Player -> Board -> Bool
checkWin player board = any (checkLine player) (rows ++ cols ++ diags)
    where
        rows = board
        cols = transpose board
        diags = diagonals board

-- Verifica se uma linha contém quatro peças consecutivas do mesmo jogador
checkLine :: Player -> [Char] -> Bool
checkLine player line = any (== 4) $ map (length . filter (== piece)) (grouped 4 line)
    where piece = if player == Player1 then 'X' else 'O'

-- Agrupa elementos em sub-listas de tamanho especificado
grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n list@(x:xs)
    | length list < n = []
    | otherwise = take n list : grouped n xs

-- Calcula todas as diagonais relevantes no tabuleiro
diagonals :: Board -> [[Char]]
diagonals b = concat [rightDiagonals b, leftDiagonals b]

-- Calcula diagonais ascendentes à direita
rightDiagonals :: Board -> [[Char]]
rightDiagonals b = [diagonal b x y (-1, 1) | x <- [0..numRows-1], y <- [0..numCols-1], x >= 3, y <= numCols-4]

-- Calcula diagonais ascendentes à esquerda
leftDiagonals :: Board -> [[Char]]
leftDiagonals b = [diagonal b x y (-1, -1) | x <- [0..numRows-1], y <- [0..numCols-1], x >= 3, y >= 3]

-- Gera uma diagonal do tabuleiro dado um ponto de início e direção
diagonal :: Board -> Int -> Int -> (Int, Int) -> [Char]
diagonal b startX startY (dx, dy) =
    takeWhileValid (startX, startY) (dx, dy) []
  where
    takeWhileValid (x, y) (dx, dy) acc
        | x < 0 || x >= numRows || y < 0 || y >= numCols = acc
        | otherwise = takeWhileValid (x + dx, y + dy) (dx, dy) (b !! x !! y : acc)

-- Inicia o jogo exibindo uma mensagem de boas-vindas e gerenciando os turnos dos jogadores
playGame :: IO ()
playGame = do
    putStrLn "Bem-vindo ao Quatro em Linha!"
    putStrLn "Jogador 1: X | Jogador 2: O"
    playTurn Player1 emptyBoard

-- Gerencia turnos recursivamente, alternando entre jogadores e verificando condições de vitória ou empate
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

-- Ponto de entrada principal que inicia o jogo
main :: IO ()
main = playGame
