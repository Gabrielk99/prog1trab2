import Data.List

main = do 
    descricao<-readFile "descricao.txt"
    let descricao_1 = map (words) (lines descricao)
    exemplos<-readFile "base.txt"
    let base = map (words) (lines exemplos)
    let classe =(tail (last descricao_1))
    let caracteristicas = (convertToCaracteristicas descricao_1)
    let base_1 = convertToExemplo base descricao_1
    let maisComum = maioria base_1 (tail(last descricao_1))
    putStrLn $ show (arvoreDecisao base_1 (init caracteristicas) maisComum classe)
    putStrLn $ show (entropia base_1 classe)


{--------------- tipos utilizados no trabalho -----------------}
data Caracteristicas = Numerico String [String]| Nominal String [String] deriving (Show,Eq)
data Exemplo = Vazio | Descricao [Caracteristicas] deriving (Show,Eq)
data ArvDecision a = Null | No a [ArvDecision a] deriving (Show,Eq)
---------------------------------------------------------------------------------------------------

{-Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo-}
convertToCaracteristicas [] = []
convertToCaracteristicas (xs:xss) | length xs > 1 = (Nominal (head xs) (tail xs)):convertToCaracteristicas xss
                                  | otherwise = Numerico (head xs) [] : convertToCaracteristicas xss
convertToExemplo' [] [] = []
convertToExemplo' (caract:ex) (dec:descricao) | length dec>1 = Nominal (head dec) [caract]:convertToExemplo' ex descricao
                                              | otherwise = Numerico (head dec) [caract]:convertToExemplo' ex descricao

convertToExemplo [] _ = []
convertToExemplo (ex:exemplos) (descricoes) = Descricao (convertToExemplo' ex descricoes): convertToExemplo exemplos descricoes

-----------------------------------------------------------------------------------------------------------------------

-- Função auxiliar para retornar um valor de alguma caracteristica, ex Nominal "Temperatura" ["sol"], a função retorna "sol"--
retornaClasse (Nominal atributo tipos) = head tipos
retornaClasse (Numerico atributo tipos) = head tipos

verificarClassBaseEx [] classe = []
verificarClassBaseEx (Descricao car:base) classe | (retornaClasse (last car)) == classe = True:verificarClassBaseEx base classe
                                                 | otherwise = False:verificarClassBaseEx base classe


filtraClasse classe [] = 0
filtraClasse classe (Descricao car:base) | classe == (retornaClasse (last car)) = 1+filtraClasse classe base
                                         |otherwise = filtraClasse classe base

maioria base [] = []
maioria base classes = maiscomum
                    where 
                        maiscomum = (snd(last filtroSort))
                        filtroSort = sort filtro
                        filtro = [((filtraClasse ci base),ci)|ci<-classes]

arvoreDecisao base caracteristicas maisComum classes = 
                                                if null base then (No maisComum [])
                                                else if and(verificarClassBaseEx base (maioria base classes)) then (No maisComum [])
                                                else if null caracteristicas then (No (maioria base classes) [])
                                                else Null
                                                


{-Funções para calculo de razão de ganho de informação-}
entropia base [] = 0
entropia base (ci:classes) = (-percentagem)*(logBase(2)(percentagem)) + entropia base classes
                    where 
                        percentagem = (filtraClasse ci base)/(fromIntegral(length base)) 
