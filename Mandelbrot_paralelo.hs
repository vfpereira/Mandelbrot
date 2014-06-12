import Graphics.UI.GLUT
import System.Environment
import Data.Complex
import Data.Time.Clock
import Control.DeepSeq
import Foreign.C.Types
import Control.Parallel.Strategies
import Data.List
import Data.List.Split
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- definiçao das constantes e dos numeros complexos que serão iterados mais tarde no programa
imageHeight' = 4.0:: Double
imageWidth' =  imageHeight'  

numeroIteracaoes = 50	
tela = 512                   

plano passo = [(height,width) | width<-[-imageWidth'/2,-imageWidth'/2+passo..imageWidth'/2],height<-[-imageHeight'/2,-imageHeight'/2+passo..imageHeight'/2]]
planoComplexo passo= map(\(real,imaginario)->real:+imaginario) (plano passo)

type Iteracao           = Int
type Width'             = Double
type Height'            = Double
type Cor                = Int
type Pixel              = (Height',Width',Cor)
type Pontos             = [Pixel]
type NroComplexo        = Complex Double
type PlanoComplexo      = [Complex Double]
type NumChunks          = Int
type TamanhoChunks      = Int
type TamanhoLinha       = Int
type NumBlocos          = Int

instance NFData CDouble -- Necessário para usar deepseq em GLfloat


rdeepPar :: NFData a => Strategy a
rdeepPar pontos = rpar(force pontos)
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
mandelbrot :: [[NroComplexo]]->Iteracao->Eval [Pixel]
mandelbrot [] _  =return []
mandelbrot (listaPonto:plano) numeroIteracaoes = do 
	p1 <-rdeepPar(map(\pixel->iteracaoPontoMandelbrot numeroIteracaoes pixel pixel ) listaPonto) 
	p2 <- mandelbrot plano numeroIteracaoes  
	return (p1++p2)  

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- função que faz a iteração Zn+1=Zn²+c e verifica se o valor absoluto de z e menor que dois se apos todas as iteraçoes for <= 2  retorna o ponto x y com a cor como sendo o valor 0 se nao for devolve o ponto com uma cor sendo o valor da iteração no qual e maior que 2
iteracaoPontoMandelbrot::Iteracao->NroComplexo->NroComplexo->Pixel
iteracaoPontoMandelbrot  0 complexo complexoIterado=  (realPart (complexo),imagPart (complexo),0) 
iteracaoPontoMandelbrot iterador complexo complexoIterado
		| magnitude complexoIterado > 2 =(realPart (complexo),imagPart (complexo),iterador)
		| otherwise = iteracaoPontoMandelbrot(iterador-1) complexo  zn
		where
			zn = (complexoIterado^2)+(complexo)
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 
linhaNormal :: PlanoComplexo->TamanhoChunks->[[NroComplexo]]
linhaNormal plano tamanhoChunk= chunksOf tamanhoChunk plano

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 

linhaRoundRobin :: PlanoComplexo->NumChunks->Bool->[[NroComplexo]]
linhaRoundRobin [] _ _ =  []
linhaRoundRobin plano numSubLists ultimaRecursao =  if ultimaRecursao == False then ([take numSubLists plano] ++ linhaRoundRobin (drop numSubLists plano) numSubLists False)
						     else transpose ([take numSubLists plano] ++ linhaRoundRobin (drop numSubLists plano) numSubLists False) 					 

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 

colunaNormal :: PlanoComplexo->TamanhoLinha->TamanhoChunks->[[NroComplexo]]
colunaNormal plano tamanhoLinha tamanhoChunk = do 
						   let planoColuna =concat(transpose (chunksOf tamanhoLinha plano))
						   chunksOf tamanhoChunk planoColuna
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 

colunaRoundRobin :: PlanoComplexo->TamanhoLinha->NumChunks->Bool->[[NroComplexo]]
colunaRoundRobin [] _ _ _ =  []
colunaRoundRobin plano tamanhoLinha numSubLists ultimaRecursao =  
   if ultimaRecursao == False then ([take numSubLists plano] ++ colunaRoundRobin (drop numSubLists plano) tamanhoLinha numSubLists False)
   else do let planoColuna =concat(transpose (chunksOf tamanhoLinha plano))
	   transpose ([take numSubLists planoColuna] ++ colunaRoundRobin (drop numSubLists planoColuna) tamanhoLinha numSubLists False) 	
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 						   


--blocoNormal :: PlanoComplexo->TamanhoLinha->NumChunks->[[NroComplexo]]
--blocoNormal [] _ _  =  []
--blocoNormal plano tamanhoLinha numSubLists =  do let planoColuna =concat(transpose (chunksOf tamanhoLinha plano))
--       						 [take numSubLists planoColuna] ++ blocoNormal (drop numSubLists planoColuna) tamanhoLinha numSubLists 		


							   
main :: IO ()
main = do
	
	(_, args) <- getArgsAndInitialize 
	--args<- getArgs
  	let (cores,totalPontos,particao) = processaArgs args
	
	let telaArg =  totalPontos
	
        let passoArg =imageHeight'/telaArg	
	let numIteracoes = 50
	let numeroPontos = length(planoComplexo passoArg) 
        let tamanhoChunks = (length(planoComplexo passoArg) `div` (cores))+1
	t0 <- getCurrentTime
	let planoParticao particao = case particao of
					0 -> linhaNormal (planoComplexo passoArg) tamanhoChunks
					1 -> linhaRoundRobin (planoComplexo passoArg) (cores) True
					2 -> colunaNormal (planoComplexo passoArg)  (floor telaArg) tamanhoChunks
					_ -> colunaRoundRobin (planoComplexo passoArg)  (floor telaArg) (cores) True
					

	t1 <- (planoParticao particao)  `deepseq` getCurrentTime
	let avaliacao = mandelbrot (planoParticao particao)  numIteracoes
        t2 <-runEval(avaliacao)  `deepseq` getCurrentTime	
 	
	let tipoParticao particao = case particao of
					0 -> "Linha Normal"
					1 -> "Linha RoundRobin"
					2 -> "Coluna Normal"
					_ -> "Coluna RoundRobin"
        
	putStrLn $ (show $ (cores))  ++ ", " ++ (show $ (numeroPontos)) ++ ", " ++(show $ tipoParticao particao)  ++ ", " ++ (show $ diffUTCTime t1 t0) ++ ", " ++ (show $ diffUTCTime t2 t1) ++ "\n"
	
 where
   -- define parâmetros default caso não sejam especificados
   processaArgs args = case args of
     (a:b:c:_) -> (read a,read b,read c)
     _           -> (4,512,0)
