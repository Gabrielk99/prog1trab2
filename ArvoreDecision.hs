module ArvoreDecision where
    import Utils
    import Data.List
    import Debug.Trace
    import qualified Data.Map as Map
    
    --Função para retornar uma classe que mais ocorre numa base de exemplos
    --Entrada: Base, Classes possíveis da descrição
    --Saida: A classe que mais ocorre na base disponível
    --maioria::[Exemplo]->[String]->String 
    maioria base [] _ = []
    maioria base classes classe = maiscomum
                        where 
                            maiscomum = (snd(head (filtroSort)))
                            filtroSort = sortBy (ordena) filtro
                            filtro = [((filtraClasse ci base classe),ci)|ci<-classes]
    
    --Entrada: Base de exemplos, tamanho da base, uma lista de caracteristicas e as classes possíveis
    --Saida: Caracteristica melhor de escolha, onde possuí a maior razão de ganho
    --melhorTeste:: [Exemplo]->Int->[Caracteristicas]->[String]->Caracteristicas
    melhorTeste  base tamBase caracteristicas classes = snd(head(trace("melhor = "++(show igr_list))(sortBy (ordena) igr_list)))
                              where 
                              igr_list = iGR base tamBase caracteristicas (tail classes) (head classes)
    
    --Função para avaliar os valores de uma caracteristica selecionada no processo de construção da arvore para gerar as subarvores
    --Entrada: Caracteristica, uma base, uma lista de caracteristicas atualizada, classes possíveis e a classe mais comum
    --Saida: uma lista de arvores, ou seja subarvores que são filhas da arvore mãe caracteristica
    --avaliaValores::Caracteristicas->[Exemplo]->[Caracteristicas]->[String]->String->[ArvDecision]
    avaliaValores (Nominal (nome,atributo)) base caracteristicas classes maisComum = [Valor (Nom x) (arvoreDecisao (filtraAtributo nome x base) caracteristicas maisComum classes) | x<-atributo]
    
    avaliaValores (Numerico (nome,atributo)) base caracteristicas classes maisComum = [Valor (Num (a,b)) (arvoreDecisao (filtraAtributoNum nome (a,b) base) caracteristicas maisComum classes) | (a,b)<-atributo]                                       
    ------------------------------------------------------------------------------------------------------------------------------------------------------
    {- Função principal que ira contruir a arvore -}
    --Entrada: uma base de exemplos, uma lista de caracteristicas, a classe mais comum da base de exemplos e a lista de classes presente na descrição
    --Saida: Uma arvore de decisão
    --arvoreDecisao::[Exemplo]->[Caracteristicas]->String->[String]->ArvDecision                                                   
    arvoreDecisao base caracteristicas maisComum classes |null base = Resultado maisComum
                                                         |qtdClassMaisComum==(fromIntegral tamBase) = Resultado maisComun
                                                         |null caracteristicas = Resultado maisComun 
                                                         |otherwise = arvore
                                                where 
                                                 tamBase = length base
                                                 discretizado = discretiza caracteristicas base (head classes)
                                                 melhorT  =  melhorTeste base tamBase discretizado classes 
                                                 arvore = Caracteristica (nome) (avaliaValores melhorT base caractAtt classes maisComun)
                                                 nome = retornaNomeCarac melhorT
                                                 caractAtt = removeCaract nome caracteristicas
                                                 maisComun = maioria base (tail classes) (head classes)
                                                 qtdClassMaisComum = filtraClasse maisComun base (head classes)
    -------------------------------------------------------------------------------------------------------------------------------------------------------
    {---------------------- Função do calculo de razão de ganho para selecionar a melhor caracteristica a ser utilizada---------------------}
    --Função principal para o calculo de razão de ganho
    --Entrada: Base, tamanho da base, uma lista de caracteristicas e classes possíveis
    --Saida: Uma lista de caracteristica associada a sua razão de ganho
    --iGR :: [Exemplo]->Int->[Caracteristicas]->[String]->[(Double,Caracteristicas)]
    iGR _ _ [] _ _ = []
    iGR base tamBase (Nominal (nome,atributo):caract) classes classe |iv/=0 = (ig/iv,(Nominal (nome,atributo))):iGR base tamBase caract classes classe
                                                                     |otherwise = (0,(Nominal (nome,atributo))):iGR base tamBase caract classes classe
                                                    where 
                                                        iv = - (iV base tamBase nome atributo)
                                                        ig = iG base tamBase nome atributo classes classe
    
    iGR base tamBase (Numerico (nome,atributo):caract) classes classe |iv/=0 =  (ig/iv,(Numerico (nome,atributo))):iGR base tamBase caract classes classe
                                                                      |otherwise = (0,(Numerico (nome,atributo))):iGR base tamBase caract classes classe
                                 where 
                                ig = (iGnumerico base tamBase nome atributo classes classe)
                                iv = - (iVnumerico base tamBase nome atributo) 
    -------------------------------------------------------------------------------------------------------------------------------------------------------
    --------------Calculo de razão de ganho para valores nominais---------------------
    --Função auxiliar para calcular o iG
    --Entrada: nome da caracteristica, os atributos possíveis, uma base de exemplo, o tamanho da base e as classes possíveis
    --Saida: Um valor númerico para o calculo do iG
    --valoresIG :: String->[String]->[Exemplo]->Int->[String]->Double
    valoresIG _ [] _ _ _ _= 0
    valoresIG nome (atb:atributos) base tamBase classes classe| tamConjunto /=0 && tamBase /=0 = ((tamConjunto/(fromIntegral tamBase))*(entropiaX))+(valoresIG nome atributos base tamBase  classes classe) 
                                                              | otherwise = valoresIG nome atributos base tamBase classes classe
                                where
                                    novoConjunto = filtraAtributo nome atb base
                                    tamConjunto = fromIntegral(length (novoConjunto))
                                    entropiaX = entropia novoConjunto tamConjunto classes classe
    --Função para o calculo do iG
    --Entrada: Base, tamanho da base, nome da caracteristica, os atributos e as possíveis classes
    --Saida: Um valor númerico que represanta o iG
    --iG :: [Exemplo]->Int->String->[String]->[String]->Double
    iG base tamBase nome atributo classes classe = entropiaX -  valoresX
                where
                entropiaX = entropia base (fromIntegral tamBase) classes classe
                valoresX = valoresIG nome atributo base tamBase classes classe
    
    --Função para calcular o iV
    --Entrada: Base, tamanho da base, nome da caracteristica e o conjunto de valores
    --Saida: Um valor númerico representando o iV
    --iV::[Exemplo]->Int->String->[String]->Double
    iV _ _ _ [] = 0
    iV base tamBase nome (atb:atributo) | tamConjunto /=0 && tamBase /=0 = percentagem*logBase(2)(percentagem) + (iV base tamBase nome atributo)
                                        |otherwise =  (iV base tamBase nome atributo)
                        where 
                            tamConjunto = fromIntegral (filtraAtributo2 nome atb base)
                            percentagem = tamConjunto/(fromIntegral tamBase)
    ------------------------------------------------------------------------------------------------------------------------------------------------
    ----------------------------------Calculo de razão de ganho para valores númericos------------------------------------
    --Função do calculo de iG especifico para caracteristicas numericas
    --Entrada: Base, tamanho da base, nome da caracteristica, o conjunto de valores (intervalos) e as possíveis classes
    --Saida: Um valor númerico representando iG
    --iGnumerico::[Exemplo]->Int->String->[(String,String)]->[String]->Double
    iGnumerico base tamBase nome atributo classes classe = entropiaX -  valoresX
                where
                entropiaX = entropia base (fromIntegral tamBase) classes classe
                valoresX = valoresIGnumerico nome atributo base tamBase classes classe
    --Função auxiliar para o calculo do iG especifico para caracteristicas númericas
    --Entrada: Nome da caracteristica, list de valores possíveis (intervalos), uma base, tamanho da base e as possíveis classes
    --Saida: Valor representativo para o calculo do iG
    --valoresIGnumerico::String->[(String,String)]->[Exemplo]->Int->[String]->Double
    valoresIGnumerico _ [] _ _ _ _ = 0
    valoresIGnumerico nome (atb:atributos) base tamBase classes classe |tamConjunto /=0 && tamBase /=0 = ((tamConjunto/(fromIntegral tamBase))*entropiaX)+(valoresIGnumerico nome atributos base tamBase  classes classe) 
                                                                       | otherwise = valoresIGnumerico nome atributos base tamBase classes classe
                                where
                                    novoConjunto = filtraAtributoNum nome atb base
                                    tamConjunto = fromIntegral(length (novoConjunto))
                                    entropiaX = (entropia novoConjunto tamConjunto classes classe)
    --Função especifica para o calculo do iV de caracteristicas numericas
    --Entrada:Base, tamanho da base, nome da caracteristica e uma lista de intervalos numericos, que são os valores
    --Saida: Valor númerico representativo de iV
    --iVnumerico::[Exemplo]->Int->String->[(String,String)]->Double
    iVnumerico _ _ _ [] = 0
    iVnumerico base tamBase nome (atb:atributo) | tamConjunto /=0 && tamBase /=0 = percentagem*logBase(2)(percentagem) + (iVnumerico base tamBase nome atributo)
                                                |otherwise =  (iVnumerico base tamBase nome atributo)
                      where 
                          tamConjunto = fromIntegral (filtraAtributoNum2 nome atb base)
                          percentagem = tamConjunto/(fromIntegral tamBase)
    -------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ---------------------------------------------Calculo da entropia-----------------------------------------------
    --Entrada: Base e possíveis classes
    --Saida: Valor númerico representativo da entropia
    --entropia:: [Exemplo]->[String]->Double
    entropia base _ [] _ = 0
    entropia base tamBase (ci:classes) classe |percentagem /=0 = -(percentagem)*(logBase(2)(percentagem)) + entropia base tamBase classes classe
                                              |otherwise = entropia base tamBase classes classe
                            where 
                            percentagem = (qtdDeExVal)/(tamBase)
                            qtdDeExVal = fromIntegral(filtraClasse ci base classe)
    ----------------------------------------------------------------------------------------------------------------
    
