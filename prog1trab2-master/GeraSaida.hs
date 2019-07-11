module GeraSaida where 
import Utils
import System.IO
import Data.List
import qualified Data.Map as Map 

andaVal caracteristicas nome (Valor (Nom v) arvDec : valores) | valor == v = caminhaArv caracteristicas arvDec
                                                              |otherwise = andaVal caracteristicas nome valores
                                                          where
                                                            valor = retornaValor ((Map.fromList caracteristicas) Map.! nome)

andaVal caracteristicas nome (Valor (Num (min,max)) arvDec:valores) | valor>min && valor<=max = caminhaArv caracteristicas arvDec
                                                                    | otherwise = andaVal caracteristicas nome valores
                                                          where 
                                                            valor = retornaValorNum ((Map.fromList caracteristicas) Map.! nome)

caminhaArv _ (Resultado valor) = valor
caminhaArv caracteristicas (Caracteristica nome valores) = andaVal caracteristicas nome valores
                                                       
gerarResposta [] _ = []
gerarResposta (Descricao caracteristicas:exemplos) arv = caminhaArv caracteristicas arv : gerarResposta exemplos arv 
formataSaida xs = concat (intersperse "\n" xs)