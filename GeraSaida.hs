module GeraSaida where 
import Utils
import qualified Data.Map as Map 

andaVal caracteristicas valor (Valor (Nom v) arvDec : valores) | valor == v = caminhaArv caracteristicas arvDec
                                                               |otherwise = andaVal caracteristicas valor valores
andaVal caracteristicas valor (Valor (Num (min,max)) arvDec:valores) | valor1>=min && valor1<max = caminhaArv caracteristicas arvDec
                                                                     | otherwise = andaVal caracteristicas valor valores
                                                        where 
                                                            valor1 = ((read valor) :: Double)
caminhaArv _ (Resultado valor) = valor
caminhaArv caracteristicas (Caracteristica nome valores) = andaVal caracteristicas valor valores
                                                        where 
                                                            valor = (Map.fromList caracteristicas) Map.! nome
gerarResposta [] _ = []
gerarResposta ((Descricao caracteristicas):exemplos) arv  = caminhaArv caracteristicas arv : gerarResposta exemplos arv 

formataSaida [] = []
formataSaida (x:xs) = x ++ "\n" ++ formataSaida xs