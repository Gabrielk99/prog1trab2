import Data.List
import Debug.Trace
import Utils
import qualified Data.Map as Map
main = do 
    descricao<-readFile "descricao.txt"
    let descricao_1 = map (words) (lines descricao)
    exemplos<-readFile "base.txt"
    let base = map (words) (lines exemplos)
    let classe =(last descricao_1)
    let caracteristicas = (convertToCaracteristicas descricao_1)
    let base_1 = convertToExemplo base descricao_1
    let maisComum = maioria base_1 (tail(last descricao_1))
    let tamBase = length base_1
    putStrLn $ show (arvoreDecisao base_1 (init caracteristicas) maisComum classe)
    --putStrLn $ show (iGR base_1 (length base_1) (init caracteristicas) classe)


---------------------------------------------------------------------------------------------------

{-Convertendo a entrada I/O para os tipos criados Caracteristicas e Exemplo-}
{-convertToCaracteristicas [] = []
convertToCaracteristicas (xs:xss) | length xs > 1 = (Nominal ((head xs),(tail xs))):convertToCaracteristicas xss
                                  | otherwise = (Numerico ((head xs),(tail xs))) : convertToCaracteristicas xss


convertToExemplo' [] [] = []
convertToExemplo' (caract:ex) (dec:descricao)= (((head dec),caract):convertToExemplo' ex descricao)


convertToExemplo [] _ = []
convertToExemplo (ex:exemplos) (descricoes) = Descricao (convertToExemplo' ex descricoes): convertToExemplo exemplos descricoes

convertToDecisaoNum (a,b) = Num (((read a)::Double),((read b)::Double))
convertToDecisaoNom valor = Nom valor-}
-----------------------------------------------------------------------------------------------------------------------
ordena ys xs | ys>xs = LT
             |otherwise = GT

----------------------------------------------------------------------------------------------------------------------

-- Função auxiliar para retornar um valor de alguma caracteristica, ex Nominal "Temperatura" ["sol"], a função retorna "sol"--

verificarClassBaseEx [] classe = []
verificarClassBaseEx (Descricao mapa:base) classe |(snd(last mapa)) == classe = True:verificarClassBaseEx base classe
                                                 | otherwise = False:verificarClassBaseEx base classe

{-Funções de filtragem, retornam algo presente no conjunto-}
filtraClasse _ [] = 0
filtraClasse classe (Descricao mapa:base) | classe == (snd(last mapa)) = 1+filtraClasse classe base
                                         |otherwise = filtraClasse classe base
filtraAtributo _ _ [] = []
filtraAtributo nome atributo (Descricao mapa:base) | (Map.fromList mapa) Map.! nome == atributo = (Descricao mapa):filtraAtributo nome atributo base
                                                   | otherwise = filtraAtributo nome atributo base
retornaListaCaract [] _ _ = []
retornaListaCaract (Descricao mapa:base) nome classe = ((Map.fromList mapa) Map.! nome,(Map.fromList mapa) Map.! classe ): retornaListaCaract base nome classe

retornaNomeCarac (Numerico (nome,atributo)) = nome
retornaNomeCarac (Nominal (nome,atributo)) = nome

removeCaract _ [] = []
removeCaract nome_1 (Nominal (nome,atributo):caract) | nome_1 == nome = caract
                                                     |otherwise = (Nominal (nome,atributo)):removeCaract nome_1 caract
removeCaract nome_1 (Numerico (nome,atributo):caract) | nome_1 == nome = caract
                                                      | otherwise = (Numerico (nome,atributo)):removeCaract nome_1 caract
---------------------------------------------------------------------------------
{-chamada recursiva para as sub-arvores-}

avaliaValores (Nominal (nome,atributo)) base caracteristicas classes maisComum = [Valor (convertToDecisaoNom x) (arvoreDecisao (filtraAtributo nome x base) caracteristicas maisComum classes) | x<-atributo]

avaliaValores (Numerico (nome,atributo)) base caracteristicas classes maisComum = [Valor (convertToDecisaoNum (a,b)) (arvoreDecisao (filtraAtributoNum nome (a,b) base) caracteristicas maisComum classes) | (a,b)<-intervalos]
                                                where 
                                                    intervalos = converteIntervalos atributo "-Infinity"
                                                    
                                       
--------------------------------------------------------------------------------------------------------------------------------------------
maioria base [] = []
maioria base classes = maiscomum
                    where 
                        maiscomum = (snd(last filtroSort))
                        filtroSort = sort filtro
                        filtro = [((filtraClasse ci base),ci)|ci<-classes]

arvoreDecisao [] _ maisComum _ = Resultado maisComum
arvoreDecisao _ [] maisComum _ = Resultado maisComum
arvoreDecisao base caracteristicas maisComum classes | and(verificarClassBaseEx base maisComun) = Resultado maisComum
                                                     |otherwise = arvore
                                               -- if null base then (Resultado maisComum)
                                               -- else if and(verificarClassBaseEx base maisComun) then (Resultado maisComum)
                                               -- else if null caracteristicas then (Resultado maisComun)
                                              --  else arvore
                                                where 
                                                    tamBase = length base
                                                    melhorT  =  melhorTeste base tamBase caracteristicas classes
                                                    arvore = Caracteristica (nome) (avaliaValores melhorT base caractAtt classes maisComun)
                                                    nome = retornaNomeCarac melhorT
                                                    caractAtt = removeCaract nome caracteristicas
                                                    maisComun = maioria base (tail classes) 

----- MELHOR TESTE ----
melhorTeste  base tamBase caracteristicas classes = snd(head(sortBy (ordena) igr_list))
                                where 
                                    igr_list = iGR base tamBase caracteristicas classes
{-------------------------------------------------------Calculo Do IGR-------------------------------------------------------}
iGR _ _ [] _ = []
iGR base tamBase (Nominal (nome,atributo):caract) classes |ig /=0 && iv/=0 = (ig/iv,(Nominal (nome,atributo))):iGR base tamBase caract classes
                                                          |otherwise = (0,(Nominal (nome,atributo))):iGR base tamBase caract classes
                                                where 
                                                    iv = iV base tamBase nome atributo
                                                    ig = iG base tamBase nome atributo (tail classes)

iGR base tamBase (Numerico (nome,atributo):caract) classes |ig/=0 && iv/= 0 =  (ig/iv,(Numerico (nome,listaPreIntervalo))):iGR base tamBase caract classes
                                                           |otherwise = (0,(Numerico (nome,listaPreIntervalo))):iGR base tamBase caract classes
                             where 
                            discreti = sort(retornaListaCaract base nome (head classes))
                            listaPreIntervalo = retornaListMed (tail discreti) (head discreti)
                            intervalos = converteIntervalos listaPreIntervalo "-Infinity"
                            ig = iGnumerico base tamBase nome intervalos (tail classes) 
                            iv = iVnumerico base tamBase nome intervalos
                                                                 

----------------------------------------- Gerando intervalos de discreção ------------------------------------------------------
retornaListMed :: [(String,String)]->(String,String)->[String]
retornaListMed [] _ = []
retornaListMed ((a,b):dados) (x,c) | c/=b = show(((read a :: Double) + (read x::Double))/2.0) : retornaListMed dados (a,b)
                                   |otherwise = retornaListMed dados (a,b)

converteIntervalos [] x = (x,"Infinity"):[]
converteIntervalos (x:xs) ant = (ant,x):converteIntervalos xs x
--------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------

{----------Funções para calculo de razão de ganho de informação----------}
entropia base [] = 0
entropia base (ci:classes) |percentagem /=0 = (-percentagem)*(logBase(2)(percentagem)) + entropia base classes
                           |otherwise = entropia base classes
                        where 
                        percentagem = (filtraClasse ci base)/(fromIntegral(length base)) 

{--------------Calculo de razão de ganho para valores nominais---------------------}

valoresIG _ [] _ _ _ = 0
valoresIG nome (atb:atributos) base tamBase classes | tamConjunto /=0 && tamBase /=0 = ((tamConjunto/(fromIntegral tamBase))*(entropiaX))+(valoresIG nome atributos base tamBase  classes) 
                                                    | otherwise = valoresIG nome atributos base tamBase classes 
                            where
                                novoConjunto = filtraAtributo nome atb base
                                tamConjunto = fromIntegral(length (novoConjunto))
                                entropiaX = entropia novoConjunto classes

iG base tamBase nome atributo classes = entropiaX -  valoresX
            where
            entropiaX = entropia base classes
            valoresX = valoresIG nome atributo base tamBase classes

iV _ _ _ [] = 0
iV base tamBase nome (atb:atributo) | tamConjunto /=0 && tamBase /=0 = -percentagem*logBase(2)(percentagem) + (iV base tamBase nome atributo)
                                    |otherwise =  (iV base tamBase nome atributo)
                    where 
                        novoConjunto = filtraAtributo nome atb base
                        tamConjunto = fromIntegral (length(novoConjunto))
                        percentagem = tamConjunto/(fromIntegral tamBase)
------------------------------------------------------------------------------------------------------------------------------------------------
{----------------------------------Calculo de razão de ganho para valores númericos------------------------------------}
iGnumerico base tamBase nome atributo classes = entropiaX -  valoresX
            where
            entropiaX = entropia base classes
            valoresX = valoresIGnumerico nome atributo base tamBase classes

valoresIGnumerico _ [] _ _ _ = 0
valoresIGnumerico nome (atb:atributos) base tamBase classes  |tamConjunto /=0 && tamBase /=0 = ((tamConjunto/(fromIntegral tamBase))*(entropiaX))+(valoresIGnumerico nome atributos base tamBase  classes) 
                                                             | otherwise = valoresIGnumerico nome atributos base tamBase classes 
                            where
                                novoConjunto = filtraAtributoNum nome atb base
                                tamConjunto = fromIntegral(length (novoConjunto))
                                entropiaX = entropia novoConjunto classes
filtraAtributoNum _ _ [] = []                  
filtraAtributoNum nome (a,b) (Descricao mapa : base) | valor <=((read b)::Double) && valor >((read a)::Double) = (Descricao mapa):filtraAtributoNum nome (b,a) base
                                                     |otherwise = filtraAtributoNum nome (b,a) base
                                            where 
                                                valor = (read ((Map.fromList mapa) Map.! nome))::Double
iVnumerico _ _ _ [] = 0
iVnumerico base tamBase nome (atb:atributo) | tamConjunto /=0 && tamBase /=0 = -percentagem*logBase(2)(percentagem) + (iVnumerico base tamBase nome atributo)
                                            |otherwise =  (iVnumerico base tamBase nome atributo)
                  where 
                      novoConjunto = filtraAtributoNum nome atb base
                      tamConjunto = fromIntegral (length(novoConjunto))
                      percentagem = tamConjunto/(fromIntegral tamBase)
