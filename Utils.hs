module Utils where 
    import Data.List
    import qualified Data.Map as Map
    import System.IO
    --TIPOS CRIADOS PARA O TRABALHO--
    data Caracteristicas = Numerico (String,[(Double,Double)]) | Nominal (String,[String]) deriving (Show,Eq,Ord)
    data Valores = Escrito String | Numero Double deriving (Show,Eq)
    data Exemplo = Descricao [(String,Valores)] deriving (Show,Eq)
    data Decisao =  Nom String | Num (Double,Double) deriving (Show,Eq)
    data ArvDecision = Resultado String |Caracteristica String [ArvDecision]| Valor Decisao ArvDecision deriving (Show,Eq)
        
    {---------------Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo----------------}
    --Entrada: uma lista de descrição
    --Saida: uma lista de caracteristicas
    --convertToCaracteristicas :: [[String]]->[Caracteristicas]
    convertToCaracteristicas [] = []
    convertToCaracteristicas (xs:xss) | length xs > 1 = (Nominal ((head xs),(tail xs))):convertToCaracteristicas xss
                                    | otherwise = (Numerico ((head xs),[])) : convertToCaracteristicas xss
            
    --Entrada: Uma lista de caracteristicas de um exemplo de entrada e uma lista de descricoes
    --Saida: uma lista de tuplas (nome da caracteristica,valor do exemplo)
    --convertToExemplo'::[String]->[[String]]->[(String,String)]
    convertToExemplo' [] [] = []
    convertToExemplo' _ [] = []
    convertToExemplo' [] _ = []
    convertToExemplo' (caract:ex) (Numerico (nome,valores):caracteristicas)= (nome,(Numero ((read caract)::Double))):convertToExemplo' ex caracteristicas
    convertToExemplo' (caract:ex) (Nominal (nome,valores):caracteristicas) = (nome,(Escrito caract)):convertToExemplo' ex caracteristicas
            
    --Entrada: Uma lista de exemplos e uma lista de descrições
    --Saida: Uma base de exemplos datadas
    --convertToExemplo:: [[String]]->[[String]]->[Exemplo]  
    convertToExemplo [] _ = []
    convertToExemplo _ [] = []
    convertToExemplo [[]] _ =[]
    convertToExemplo (ex:exemplos) caracteristicas = Descricao (convertToExemplo' ex caracteristicas): convertToExemplo exemplos caracteristicas
            
    -----------------------------------------------------------------------------------------------------------------------------------
    
    -------------------------------------------------------------------------------------------------------------
                                            
        
    {-Funções de filtragem, retornam algo presente no conjunto-}
    --Entrada: Uma classe e uma base de exemplos
    --Saida: Quantidade em que essa classe apareceu em toda a base
    --filtraClasse :: String -> [Exemplo] -> Double  
    filtraClasse _ [] _ = 0
    filtraClasse classe (Descricao mapa:base) nome |classe == retornaValor ((Map.fromList mapa)Map.!nome) = 1+filtraClasse classe base nome
                                                   |otherwise = filtraClasse classe base nome
                                                
    --Entrada: Nome da caracteristica, valor da caracteristica e uma base
    --Saida: Conjunto de base onde possuem os mesmos valores da caracteristica buscada
    --filtraAtributo::String->String->[Exemplo]->[Exemplo]
    filtraAtributo _ _ [] = []
    filtraAtributo nome atributo (Descricao mapa:base) | retornaValor ((Map.fromList mapa) Map.! nome) == atributo = (Descricao mapa):filtraAtributo nome atributo base
                                                       | otherwise = filtraAtributo nome atributo base
    filtraAtributo2 _ _ [] = 0
    filtraAtributo2 nome atributo (Descricao mapa:base) | retornaValor ((Map.fromList mapa) Map.! nome) == atributo = 1+filtraAtributo2 nome atributo base
                                                       | otherwise = filtraAtributo2 nome atributo base
        
    --Entrada: Base, nome da caracteristica e nome da classificação
    --Saida: Uma lista de valores e qual classe ela gera
    --retornaListaCaract :: [Exemplo]->String->String->[(Double,String)]
    retornaListaCaract [] _ _ = []
    retornaListaCaract (Descricao mapa:base) nome classe = (valor,classe1): retornaListaCaract base nome classe
                                                    where
                                                        valor = retornaValorNum((Map.fromList mapa) Map.! nome)
                                                        classe1 = retornaValor ((Map.fromList mapa) Map.! classe)
        
    --Entrada: Caracteristica
    --Saida: Nome da caracteristica
    --retornaNomeCarac:: Caracteristicas->String
    retornaNomeCarac (Numerico (nome,atributo)) = nome
    retornaNomeCarac (Nominal (nome,atributo)) = nome
        
    --Essa função é semelhante a filtaAtributo, mas ela é especifica para o caso de caracteristicas numéricas--
    --Entrada:Nome da caracteristica, um intervalo, uma basa
    --Saida: Uma nova base onde o valor da caracterisca presente na base esta dentro do intervalo
    --filtraAtributoNum::String->(String,String)->[Exemplo]->[Exemplo]
    filtraAtributoNum _ _ [] = []                  
    filtraAtributoNum nome (a,b) (Descricao mapa : base) | valor <= b && valor >a = (Descricao mapa):filtraAtributoNum nome (a,b) base
                                                        |otherwise = filtraAtributoNum nome (a,b) base
                                                    where 
                                                        valor =  retornaValorNum((Map.fromList mapa) Map.! nome)
    filtraAtributoNum2 _ _ [] = 0                 
    filtraAtributoNum2 nome (a,b) (Descricao mapa : base) | valor <= b && valor >a = 1+filtraAtributoNum2 nome (a,b) base
                                                          |otherwise = filtraAtributoNum2 nome (a,b) base
                                                    where 
                                                    valor =  retornaValorNum((Map.fromList mapa) Map.! nome)   
                                                        
    ----------------------------------- Função para remover uma caracteristica de uma lista de caracteristicas --------------------------
    --recebe o nome da caracteristica e a lista de caracteristicas e retorna uma lista nova sem aquele elemento identificado pelo nome 
    --Entrada: Nome de uma caracteristica e uma lista de caracteristicas
    --Saida: Uma nova lista sem a que estava sendo buscada
    --removeCaract::String->[Caracteristicas]->[Caracteristicas]                           
    removeCaract _ [] = []
    removeCaract nome_1 (Nominal (nome,atributo):caract) | nome_1 == nome = caract
                                                        |otherwise = (Nominal (nome,atributo)):removeCaract nome_1 caract
    removeCaract nome_1 (Numerico (nome,atributo):caract) | nome_1 == nome = caract
                                                        | otherwise = (Numerico (nome,atributo)):removeCaract nome_1 caract
        
    retornaValor (Escrito valor) = valor
    retornaValorNum (Numero valor) = valor
    ---------------------------------------------------------------------------------------------------------------------------------
    {-Funções para discretização-}
        
    --Função para transforma a lista de intervalos de mudança de classe das caracteristicas numerais e formatar em tuplas de intervalos corretos
    --Entrada: Uma lista de intervalos em String e um valor anterior que fara parte de um novo intervalo, tipo [1,2,3] -Infinity
    --Saida: Uma lista de intervalos formatados por tuplas tipo [(-Infinity,1),(1,2),(2,3),(3,Infinity)]
    --converteIntervalos::[String]->String->[(String,String)]
    converteIntervalos [] x = (x,(read "Infinity")::Double):[]
    converteIntervalos (x:xs) ant = (ant,x):converteIntervalos xs x
        
    --Função auxiliar para o processo de discretização, ela verifica se houve mudança nas classes da lista de valores de uma caracteristica numeral
    --Caso ocorra, exemplo (23.0,Va) (24.0,NaoVa), ele calcula a media entre 23 e 24 e inclui em uma lista
    --Entrada: Uma lista de tupla de (Valor,Classe) e uma tupla (Valor,Classe) anterior a um elemento retirado da lista
    --Saida: Uma lista de medias de valores numerais pela condição estabelecida
    --retornaListMed :: [(String,String)]->(String,String)->[String]
    retornaListMed [] _ = []
    retornaListMed ((a,b):dados) (x,c) | c/=b = ((a+x)/2.0) : retornaListMed dados (a,b)
                                    |otherwise = retornaListMed dados (a,b)
        
    -------------------------------------------------------------------------------------------------------------------------------------------
    {-Funções auxiliares para o sortBy-}
    ordena y x | (fst y)>(fst x) = LT
                | otherwise = GT

    ordenaD y x |  ((fst y) <= (fst x))  = LT
                | otherwise = GT
        
        
    discretiza [] _ _ = []
    discretiza (Nominal (nome,atributo): caract) base classe = (Nominal (nome,atributo)):discretiza caract base classe           
    discretiza (Numerico (nome,atributo): caract) base classe = (Numerico (nome,newAtb)): discretiza caract base classe
                                    where 
                                        list = sortBy (ordenaD) (retornaListaCaract base nome classe)
                                        listaPreIntervalo = (retornaListMed (tail list) (head list))
                                        newAtb = converteIntervalos listaPreIntervalo ((read "-Infinity")::Double)
        
