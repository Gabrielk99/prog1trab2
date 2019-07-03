module Utils where 
--TIPOS CRIADOS PARA O TRABALHO--
data Caracteristicas = Numerico (String,[String]) | Nominal (String,[String]) deriving (Show,Eq,Ord)
data Exemplo = Descricao [(String,String)] deriving (Show,Eq)
data Decisao =  Nom String | Num (Double,Double) deriving (Show,Eq)
data ArvDecision = Resultado String |Caracteristica String [ArvDecision]| Valor Decisao ArvDecision deriving (Show,Eq)

{---------------Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo----------------}
convertToCaracteristicas [] = []
convertToCaracteristicas (xs:xss) | length xs > 1 = (Nominal ((head xs),(tail xs))):convertToCaracteristicas xss
                                      | otherwise = (Numerico ((head xs),(tail xs))) : convertToCaracteristicas xss
    
    
convertToExemplo' [] [] = []
convertToExemplo' (caract:ex) (dec:descricao)= (((head dec),caract):convertToExemplo' ex descricao)
    
    
convertToExemplo [] _ = []
convertToExemplo (ex:exemplos) (descricoes) = Descricao (convertToExemplo' ex descricoes): convertToExemplo exemplos descricoes
    
convertToDecisaoNum (a,b) = Num (((read a)::Double),((read b)::Double))
convertToDecisaoNom valor = Nom valor
-----------------------------------------------------------------------------------------------------------------------------------