module Utils where 
    import Data.List
    import qualified Data.Map as Map
    
    --TIPOS CRIADOS PARA O TRABALHO--
    data Caracteristicas = Numerico (String,[(String,String)]) | Nominal (String,[String]) deriving (Show,Eq,Ord)
    data Exemplo = Descricao [(String,String)] deriving (Show,Eq)
    data Decisao =  Nom String | Num (Double,Double) deriving (Show,Eq)
    data ArvDecision = Resultado String |Caracteristica String [ArvDecision]| Valor Decisao ArvDecision deriving (Show,Eq)
    
    {---------------Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo----------------}
    --Entrada: uma lista de descrição
    --Saida: uma lista de caracteristicas
    convertToCaracteristicas :: [[String]]->[Caracteristicas]
    convertToCaracteristicas [] = []
    convertToCaracteristicas (xs:xss) | length xs > 1 = (Nominal ((head xs),(tail xs))):convertToCaracteristicas xss
                                          | otherwise = (Numerico ((head xs),[])) : convertToCaracteristicas xss
        
    --Entrada: Uma lista de caracteristicas de um exemplo de entrada e uma lista de descricoes
    --Saida: uma lista de tuplas (nome da caracteristica,valor do exemplo)
    convertToExemplo'::[String]->[[String]]->[(String,String)]
    convertToExemplo' [] [] = []
    convertToExemplo' _ [] = []
    convertToExemplo' [] _ = []
    convertToExemplo' (caract:ex) (dec:descricao)= (((head dec),caract):convertToExemplo' ex descricao)
        
    --Entrada: Uma lista de exemplos e uma lista de descrições
    --Saida: Uma base de exemplos datadas
    convertToExemplo:: [[String]]->[[String]]->[Exemplo]  
    convertToExemplo [] _ = []
    convertToExemplo (ex:exemplos) (descricoes) = Descricao (convertToExemplo' ex descricoes): convertToExemplo exemplos descricoes
        
    convertToDecisaoNum (a,b) = Num (((read a)::Double),((read b)::Double))
    convertToDecisaoNom valor = Nom valor
    -----------------------------------------------------------------------------------------------------------------------------------
    
    {-Função para vereficar se todas as classes em uma dada base de exemplos são iguais-}
    --Entrada: a classe mais comum de uma dada base de exemplos e a base de exemplos
    --Saida: uma lista booleana
    verificarClassBaseEx::[Exemplo]->String->[Bool]
    verificarClassBaseEx [] classe = []
    verificarClassBaseEx (Descricao mapa:base) classe |(snd(last mapa)) == classe = True:verificarClassBaseEx base classe
                                                      | otherwise = False:verificarClassBaseEx base classe
    -------------------------------------------------------------------------------------------------------------
                                        
    
    {-Funções de filtragem, retornam algo presente no conjunto-}
    --Entrada: Uma classe e uma base de exemplos
    --Saida: Quantidade em que essa classe apareceu em toda a base
    filtraClasse :: String -> [Exemplo] -> Double  
    filtraClasse _ [] = 0
    filtraClasse classe (Descricao mapa:base) |classe == (snd(last mapa)) = 1+filtraClasse classe base
                                              |otherwise = filtraClasse classe base
                                            
    --Entrada: Nome da caracteristica, valor da caracteristica e uma base
    --Saida: Conjunto de base onde possuem os mesmos valores da caracteristica buscada
    filtraAtributo::String->String->[Exemplo]->[Exemplo]
    filtraAtributo _ _ [] = []
    filtraAtributo nome atributo (Descricao mapa:base) | (Map.fromList mapa) Map.! nome == atributo = (Descricao mapa):filtraAtributo nome atributo base
                                                       | otherwise = filtraAtributo nome atributo base
    
    --Entrada: Base, nome da caracteristica e nome da classificação
    --Saida: Uma lista de valores e qual classe ela gera
    retornaListaCaract :: [Exemplo]->String->String->[(String,String)]
    retornaListaCaract [] _ _ = []
    retornaListaCaract (Descricao mapa:base) nome classe = ((Map.fromList mapa) Map.! nome,(Map.fromList mapa) Map.! classe ): retornaListaCaract base nome classe
    
    --Entrada: Caracteristica
    --Saida: Nome da caracteristica
    retornaNomeCarac:: Caracteristicas->String
    retornaNomeCarac (Numerico (nome,atributo)) = nome
    retornaNomeCarac (Nominal (nome,atributo)) = nome
    
    --Essa função é semelhante a filtaAtributo, mas ela é especifica para o caso de caracteristicas numéricas--
    --Entrada:Nome da caracteristica, um intervalo, uma basa
    --Saida: Uma nova base onde o valor da caracterisca presente na base esta dentro do intervalo
    filtraAtributoNum::String->(String,String)->[Exemplo]->[Exemplo]
    filtraAtributoNum _ _ [] = []                  
    filtraAtributoNum nome (a,b) (Descricao mapa : base) | valor <=((read b)::Double) && valor >((read a)::Double) = (Descricao mapa):filtraAtributoNum nome (a,b) base
                                                         |otherwise = filtraAtributoNum nome (a,b) base
                                                where 
                                                    valor = (read ((Map.fromList mapa) Map.! nome))::Double
    
                                                    
    ----------------------------------- Função para remover uma caracteristica de uma lista de caracteristicas --------------------------
    --recebe o nome da caracteristica e a lista de caracteristicas e retorna uma lista nova sem aquele elemento identificado pelo nome 
    --Entrada: Nome de uma caracteristica e uma lista de caracteristicas
    --Saida: Uma nova lista sem a que estava sendo buscada
    removeCaract::String->[Caracteristicas]->[Caracteristicas]                           
    removeCaract _ [] = []
    removeCaract nome_1 (Nominal (nome,atributo):caract) | nome_1 == nome = caract
                                                         |otherwise = (Nominal (nome,atributo)):removeCaract nome_1 caract
    removeCaract nome_1 (Numerico (nome,atributo):caract) | nome_1 == nome = caract
                                                          | otherwise = (Numerico (nome,atributo)):removeCaract nome_1 caract
    
    ---------------------------------------------------------------------------------------------------------------------------------
    {-Funções para discretização-}
    
    --Função para transforma a lista de intervalos de mudança de classe das caracteristicas numerais e formatar em tuplas de intervalos corretos
    --Entrada: Uma lista de intervalos em String e um valor anterior que fara parte de um novo intervalo, tipo [1,2,3] -Infinity
    --Saida: Uma lista de intervalos formatados por tuplas tipo [(-Infinity,1),(1,2),(2,3),(3,Infinity)]
    converteIntervalos::[String]->String->[(String,String)]
    converteIntervalos [] x = (x,"Infinity"):[]
    converteIntervalos (x:xs) ant = (ant,x):converteIntervalos xs x
    
    --Função auxiliar para o processo de discretização, ela verifica se houve mudança nas classes da lista de valores de uma caracteristica numeral
    --Caso ocorra, exemplo (23.0,Va) (24.0,NaoVa), ele calcula a media entre 23 e 24 e inclui em uma lista
    --Entrada: Uma lista de tupla de (Valor,Classe) e uma tupla (Valor,Classe) anterior a um elemento retirado da lista
    --Saida: Uma lista de medias de valores numerais pela condição estabelecida
    retornaListMed :: [(String,String)]->(String,String)->[String]
    retornaListMed [] _ = []
    retornaListMed ((a,b):dados) (x,c) | c/=b = show(((read a :: Double) + (read x::Double))/2.0) : retornaListMed dados (a,b)
                                       |otherwise = retornaListMed dados (a,b)
    
    -------------------------------------------------------------------------------------------------------------------------------------------
    {-Funções auxiliares para o sortBy-}
    ordena ys xs | ys>xs = LT
                 | otherwise = GT
                   
    ordenaD y x | ((read (fst y) :: Double) <= (read (fst x)::Double))  = LT
                  | otherwise = GT
    
    
    discretiza [] _ _ = []
    discretiza (Nominal (nome,atributo): caract) base classes = (Nominal (nome,atributo)):discretiza caract base classes            
    discretiza (Numerico (nome,atributo): caract) base classes = (Numerico (nome,newAtb)): discretiza caract base classes
                                where 
                                    list = sortBy (ordenaD) (retornaListaCaract base nome (head classes))
                                    listaPreIntervalo = retornaListMed (tail list) (head list)
                                    newAtb = converteIntervalos listaPreIntervalo "-Infinity"
    
