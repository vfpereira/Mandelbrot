--import Graphics.UI.GLUT
import System.Environment
import Data.Complex
import Data.Time.Clock
import Control.DeepSeq
import Foreign.C.Types
import Control.Parallel.Strategies
import Data.List as List
import Data.List.Split as Split
import qualified Data.Array.Repa as Repa
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- definiçao das constantes e dos numeros complexos que serão iterados mais tarde no programa
imageHeight' = 4.0:: Double
imageWidth' =  imageHeight'  

numeroIteracaoes = 50	
tela = 512                   

plano passo = [(height,width) | width<-[-imageWidth'/2,-imageWidth'/2+passo..imageWidth'/2],height<-[-imageHeight'/2,-imageHeight'/2+passo..imageHeight'/2]]
planoComplexo passo= map(\(real,imaginario)->real:+imaginario) (plano passo)

type ArrayRepaU = Repa.Array Repa.U Repa.DIM2 (Complex Double)
type ArrayRepaD = Repa.Array Repa.D Repa.DIM2 (Complex Double)

planoComplexoRepa passo linha coluna = Repa.fromFunction (Repa.Z Repa.:. linha Repa.:.coluna ) (\(Repa.Z Repa.:. i Repa.:.j)-> (((-imageWidth'/2)+ (fromIntegral i)* passo):+ ((-imageHeight'/2)+ (fromIntegral j)*passo))   :: (Complex Double)):: Repa.Array Repa.D Repa.DIM2 (Complex Double)

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
type Tamanho            = Int
type Indice             = Int
type Linha              = Int
type Coluna             = Int
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

repa :: ArrayRepaU->NumChunks->NumChunks->Bool->[[NroComplexo]]-- [[NroComplexo]]
repa _ 0 _ _ = []
repa array numChunksIterado numChunks False = do let z = Repa.slice array (Repa.Any Repa.:. ((numChunks-numChunksIterado):: Int) Repa.:. Repa.All ) 
						 [Repa.toList z]++ repa array (numChunksIterado-1) numChunks False

--repa array numChunksIterado numChunks False = do z <- Repa.computeUnboxedP $ Repa.slice array (Repa.Any Repa.:. ((numChunks-numChunksIterado):: Int) Repa.:. Repa.All ) 

-------------------------------------------------------------------------------------------------------------------------------------------------------

linhaNormal :: ArrayRepaD->Int->Int-> IO (ArrayRepaU)
linhaNormal array cores numeroPontos= do let linha = cores
					 let coluna = numeroPontos `div` cores
					 let arrayFinal  = (Repa.reshape (Repa.Z Repa.:. linha Repa.:.coluna ) array)
				 	 Repa.computeUnboxedP arrayFinal
	 
-------------------------------------------------------------------------------------------------------------------------------------------------------

linhaRoundRobin :: ArrayRepaD->Int->Int-> IO (ArrayRepaU)
linhaRoundRobin array cores numeroPontos= let linha = numeroPontos `div` cores
					  let coluna = cores
					  let arrayReshape  = (Repa.reshape (Repa.Z Repa.:. linha Repa.:.coluna ) array)
				 	  let arrayFinal = Repa.transpose arrayReshape
					  Repa.computeUnboxedP arrayFinal
	 
-------------------------------------------------------------------------------------------------------------------------------------------------------

colunaNormal :: ArrayRepaD->Int->Int-> IO (ArrayRepaU)
colunaNormal array cores numeroPontos = let newArray = Repa.transpose array
					linhaNormal newArray cores numeroPontos 

-------------------------------------------------------------------------------------------------------------------------------------------------------

colunaRoundRobin :: ArrayRepaD->Int->Int-> IO (ArrayRepaU)
colunaRoundRobin array cores numeroPontos = let newArray = Repa.transpose array
						linhaRoundRobin newArray cores numeroPontos
-------------------------------------------------------------------------------------------------------------------------------------------------------

		   
main :: IO ()
main = do
	
	args<- getArgs
  	let (cores,totalPontos,particao) = processaArgs args
	
	let telaArg =  totalPontos
        let passoArg =imageHeight'/telaArg	

	let numIteracoes = 50
	let numeroPontos = round ((totalPontos)*(totalPontos))
        let tamanhoChunks = numeroPontos `div` cores 

	t0 <- getCurrentTime

	let arrayPlanoComplexoRepa = planoComplexoRepa passoArg (round totalPontos) (round totalPontos)

	arrayPlanoComplexoRepaParallel <- case particao of 
		  0->  linhaNormal arrayPlanoComplexoRepa cores numeroPontos 
		  1->  linhaRoundRobin arrayPlanoComplexoRepa cores numeroPontos 
		  2->  colunaNormal arrayPlanoComplexoRepa cores numeroPontos 
		  _->  colunaRoundRobin arrayPlanoComplexoRepa cores numeroPontos 

	let planoParticao particao = repa arrayPlanoComplexoRepaParallel  cores cores False 
	
	t1 <- ((planoParticao particao))  `deepseq` getCurrentTime
	
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
