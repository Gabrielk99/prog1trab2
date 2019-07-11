import Data.List
import Debug.Trace
import Utils
import ArvoreDecision
import GeraSaida
import System.IO
import qualified Data.Map as Map
main = do 
    descricao<-readFile "descricao.txt"
    let descricao_1 = map (words) (lines descricao)
    exemplos<-readFile "base.txt"
    let base = map (words) (lines exemplos)
    let classe =(last descricao_1)
    let caracteristicas = (convertToCaracteristicas descricao_1)
    let base_1 = convertToExemplo base caracteristicas
    let maisComum = maioria base_1 (tail classe) (head classe)
    let arvD = arvoreDecisao base_1 (init caracteristicas) maisComum classe
    caso<-readFile "caso.txt"
    let casos = convertToExemplo (map (words) (lines caso)) caracteristicas
    let result = gerarResposta casos arvD
    writeFile "classe.txt" (formataSaida result) 
    --putStrLn $ show(discretiza caracteristicas base_1 (head classe))
    writeFile "arvore.txt" (geraSaidaTextual arvD "")
    putStrLn $ show(arvD)
---------------------------------------------------------------------------------------------------
