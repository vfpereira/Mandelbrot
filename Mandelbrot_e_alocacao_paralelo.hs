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
type P0Linha          = Double
type P0Coluna          = Double
type Contador          = Double
type Passo           = Double
type Total          = Double
type TamanhoColuna  = Double
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
planoLinhaNormal:: NumChunks->NumChunks->P0Linha->Passo->TamanhoLinha->Eval [[NroComplexo]]
planoLinhaNormal _ 0 _ _ _ = return []
planoLinhaNormal numSublistas numSublistasIterado linha passo tamanhoLinha= do 
	let contador =numSublistas - numSublistasIterado +1 		
	let fimLinha = (-imageWidth'/2)+(passo*(fromIntegral(contador*tamanhoLinha `div` numSublistas)::Double)-passo)	
	let subLista = [(height,width) | width<-[linha,(linha+passo)..(fimLinha)],height<-[-imageHeight'/2,(-imageHeight'/2)+passo..imageHeight'/2]]
 	p1 <-rdeepPar([map(\(real,imaginario)->real:+imaginario) subLista]) 
	p2 <- planoLinhaNormal numSublistas (numSublistasIterado-1) (fimLinha+passo) passo tamanhoLinha
	return (p1 ++ p2)			
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 						   
planoLinhaRoundRobin :: NumChunks->NumChunks->P0Coluna->Passo->Eval [[NroComplexo]]
planoLinhaRoundRobin  _ 0 _ _ = return []
planoLinhaRoundRobin numSublistas numSublistasIterado coluna passo = do
	let contador = fromIntegral (numSublistas - numSublistasIterado) 
	let newPasso = (fromIntegral numSublistas)*passo 
	let subLista =[(height,width) | width<-[-imageWidth'/2,(-imageWidth'/2)+passo..(imageWidth'/2)],height<-[coluna+contador*passo,coluna+contador*passo+newPasso..imageHeight'/2+contador*passo - newPasso]]
	p1 <-rdeepPar([map(\(real,imaginario)->real:+imaginario) subLista]) 
	p2 <-planoLinhaRoundRobin numSublistas (numSublistasIterado-1) coluna passo 		
	return (p1 ++ p2)	

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 	
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 						   
planoColunaNormal:: NumChunks->NumChunks->P0Linha->Passo->TamanhoLinha->Eval [[NroComplexo]]
planoColunaNormal _ 0 _ _ _ = return []
planoColunaNormal numSublistas numSublistasIterado linha passo tamanhoLinha= do 
	let contador =numSublistas - numSublistasIterado +1 		
	let fimLinha = (-imageWidth'/2)+(passo*(fromIntegral(contador*tamanhoLinha `div` numSublistas)::Double)-passo)	
	let subLista = [(width,height) | width<-[linha,(linha+passo)..(fimLinha)],height<-[-imageHeight'/2,(-imageHeight'/2)+passo..imageHeight'/2]]
 	p1 <-rdeepPar([map(\(real,imaginario)->real:+imaginario) subLista]) 
	p2 <- planoLinhaNormal numSublistas (numSublistasIterado-1) (fimLinha+passo) passo tamanhoLinha
	return (p1 ++ p2)			
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 						   
planoColunaRoundRobin :: NumChunks->NumChunks->P0Coluna->Passo->Eval [[NroComplexo]]
planoColunaRoundRobin  _ 0 _ _ = return []
planoColunaRoundRobin numSublistas numSublistasIterado coluna passo = do
	let contador = fromIntegral (numSublistas - numSublistasIterado) 
	let newPasso = (fromIntegral numSublistas)*passo 
	let subLista =[(width,height) | width<-[-imageWidth'/2,(-imageWidth'/2)+passo..(imageWidth'/2)],height<-[coluna+contador*passo,coluna+contador*passo+newPasso..imageHeight'/2+contador*passo - newPasso]]
	p1 <-rdeepPar([map(\(real,imaginario)->real:+imaginario) subLista]) 
	p2 <-planoLinhaRoundRobin numSublistas (numSublistasIterado-1) coluna passo 		
	return (p1 ++ p2)	

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- 	
						   
main :: IO ()
main = do
	
	(_, args) <- getArgsAndInitialize 
	--args<- getArgs
  	let (cores,totalPontos,particao) = processaArgs args
	let telaArg =  totalPontos -1
	let numeroPontos = totalPontos*totalPontos	
    let passoArg =imageHeight'/telaArg
	let nroSubLinhas = cores 
	
	let numIteracoes = 50
        
	t0 <- getCurrentTime
	let planoParticao particao = case particao of
					0 -> planoLinhaNormal cores cores (-imageWidth'/2) passoArg (round totalPontos)
					1 -> planoLinhaRoundRobin  cores cores (-imageHeight'/2) passoArg	
					2 -> planoColunaNormal cores cores (-imageWidth'/2) passoArg (round totalPontos)
					_ -> planoColunaRoundRobin  cores cores (-imageHeight'/2) passoArg	

	let alocacao = runEval (planoParticao particao)
	t1 <- (alocacao)  `deepseq` getCurrentTime
	let avaliacao = mandelbrot alocacao  numIteracoes
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
