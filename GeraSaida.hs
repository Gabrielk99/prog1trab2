module GeraSaida where 
import Utils
import System.IO
import Data.List
import qualified Data.Map as Map 

{-- FUNÇÕES PARA GERAR A SAIDA FORMATADA NO PADRAO --}

--Gera result -- 
andaVal :: [(String,Valores)]->String->[ArvDecision]->String
andaVal caracteristicas nome (Valor (Nom v) arvDec : valores) | valor == v = caminhaArv caracteristicas arvDec
                                                              |otherwise = andaVal caracteristicas nome valores
                                                          where
                                                            valor = retornaValor ((Map.fromList caracteristicas) Map.! nome)

andaVal caracteristicas nome (Valor (Num (min,max)) arvDec:valores) | valor>min && valor<=max = caminhaArv caracteristicas arvDec
                                                                    | otherwise = andaVal caracteristicas nome valores
                                                          where 
                                                            valor = retornaValorNum ((Map.fromList caracteristicas) Map.! nome)
caminhaArv :: [(String,Valores)]->ArvDecision->String
caminhaArv _ (Resultado valor) = valor
caminhaArv caracteristicas (Caracteristica nome valores) = andaVal caracteristicas nome valores

gerarResposta::[Exemplo]->ArvDecision->[String]
gerarResposta [] _ = []
gerarResposta (Descricao caracteristicas:exemplos) arv = caminhaArv caracteristicas arv : gerarResposta exemplos arv 
formataSaida xs = concat (intersperse "\n" xs)


-----------------------------------------------------------------------------------------------------------------------------------------------

--Gera a arvore--
geraFimSe _ 0 = ""
geraFimSe [] _ = "fim-se\n"
geraFimSe tabact tam = tabact++"fim-se\n"++geraFimSe (drop 3 tabact) (tam-1)
geraSaidaTextual' [] _ tabact tam = geraFimSe  (drop 3 tabact) tam
geraSaidaTextual' [(Valor (Nom valor) filha)] nome tabact tam= tabact ++ "se "++ nome ++" = "++ valor++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++geraSaidaTextual' [] nome (tabact++"   ") tam
geraSaidaTextual' [(Valor (Num (min,max))filha)] nome tabact tam| min == ((read "-Infinity")::Double) =(tabact)++"se "++nome ++ " <= "++(show max)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++geraSaidaTextual' [] nome (tabact++"   ") tam
                                                                     | max == ((read "Infinity")::Double) =(tabact)++"se "++nome ++ " > "++(show min)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++geraSaidaTextual' [] nome (tabact++"   ") tam
                                                                     | otherwise = (tabact)++"se "++ (show min)++" < "++ nome ++ " <= "++(show max)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++geraSaidaTextual' [] nome (tabact++"   ") tam

geraSaidaTextual' (Valor (Nom valor) filha :values) nome tabact tam= tabact ++ "se "++ nome ++" = "++ valor++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++tabact++"senao\n"++geraSaidaTextual' values nome (tabact++"   ") tam
geraSaidaTextual' (Valor (Num (min,max))filha:values) nome tabact tam| min == ((read "-Infinity")::Double) =(tabact)++"se "++nome ++ " <= "++(show max)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++tabact++"senao\n" ++geraSaidaTextual' values nome (tabact++"   ") tam
                                                                     | max == ((read "Infinity")::Double) =(tabact)++"se "++nome ++ " > "++(show min)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++tabact++"senao\n"++geraSaidaTextual' values nome (tabact++"   ") tam
                                                                     | otherwise = (tabact)++"se "++ (show min)++" < "++ nome ++ " <= "++(show max)++" entao \n"++(geraSaidaTextual filha (tabact++"   "))++tabact++"senao\n" ++geraSaidaTextual' values nome (tabact++"   ") tam

geraSaidaTextual (Resultado resposta) tabact = (tabact)++"retorne "++(resposta)++"\n"
geraSaidaTextual (Caracteristica nome filhas) tabact = geraSaidaTextual' filhas nome tabact (length filhas)
                                            
