main = do 
    descricao<-readFile "descricao.txt"
    let descricao_1 = map (words) (lines descricao)
    exemplos<-readFile "base.txt"
    let base = map (words) (lines exemplos)
    let classe =(last (head base))
    let base_1 = convertToExemplo base descricao_1
    putStrLn $ (show (base))
    putStrLn $ (show (somenteUmaClasse (base_1) classe))


{--------------- tipos utilizados no trabalho -----------------}
data Caracteristicas = Numerico String [String]| Nominal String [String] deriving (Show,Eq)
data Exemplo = Vazio | Descricao [Caracteristicas] deriving (Show,Eq)
data ArvDecision a = Null | No a [ArvDecision a] deriving (Show,Eq)
---------------------------------------------------------------------------------------------------

{-Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo-}
convertToCaracteristicas [] = []
convertToCaracteristicas (xs:xss) | length xs > 1 = [Nominal (head xs) (tail xs)]++convertToCaracteristicas xss
                                  | otherwise = [Numerico (head xs) [] ]++convertToCaracteristicas xss
convertToExemplo' [] [] = []
convertToExemplo' (caract:ex) (dec:descricao) | length dec>1 = [Nominal (head dec) [caract]]++convertToExemplo' ex descricao
                                              | otherwise = [Numerico (head dec) [caract]]++convertToExemplo' ex descricao

convertToExemplo [] _ = []
convertToExemplo (ex:exemplos) (descricoes) = [Descricao (convertToExemplo' ex descricoes)] ++ convertToExemplo exemplos descricoes

-----------------------------------------------------------------------------------------------------------------------

-- Função auxiliar para retornar um valor de alguma caracteristica, ex Nominal "Temperatura" ["sol"], a função retorna "sol"--
retornaClasse (Nominal atributo tipos) = head tipos
retornaClasse (Numerico atributo tipos) = head tipos

verificarClassBaseEx [] classe = []
verificarClassBaseEx (Descricao car:base) classe | (retornaClasse (last car)) == classe = [True]++ verificarClassBaseEx base classe
                                                 | otherwise = [False]++verificarClassBaseEx base classe

somenteUmaClasse base classe | and (verificarClassBaseEx base classe) = No classe []
                             | otherwise = Null 