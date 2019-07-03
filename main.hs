import Data.List
import Debug.Trace
import Utils
import ArvoreDecision
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
{-
geraSaidaTextual' (Valor (Num (min,max))filha) | min == ((read "-Infinity")::Double) = "<= "++(show max)++geraSaidaTextual filha
                                                | max == ((read "Infinity")::Double) = ">"++(show min)++geraSaidaTextual filha
                                                | otherwise = ">"++ (show min) ++ "<="++(show max)++geraSaidaTextual filha 
geraSaidaTextual (Caracteristica nome filhas) = "se " ++ nome ++ saida
                                            where 
                                                saida = (lines[geraSaidaTextual' valor | valor <- filhas])-}
