import Graphics.UI.GLUT
import System.Environment
import Data.Complex
import Data.Time.Clock
import Control.DeepSeq
import Foreign.C.Types
--263169
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- definiçao das constantes e dos numeros complexos que serão iterados mais tarde no programa
imageHeight' = 4.0:: GLdouble
imageWidth' =  imageHeight'  
tela = 512                   
numeroIteracaoes = 50	

plano passo = [(width,height) | width<-[-imageWidth'/2,-imageWidth'/2+passo..imageWidth'/2],height<-[-imageHeight'/2,-imageHeight'/2+passo..imageHeight'/2]]
planoComplexo passo= map(\(real,imaginario)->real:+imaginario) (plano passo)


type Iteracao           = Int
type Width'             = GLdouble
type Height'            = GLdouble
type Cor                = Int
type Pixel              = (Height',Width',Cor)
type Pontos             = [Pixel]
type NroComplexo        = Complex GLdouble
type PlanoComplexo      = [Complex GLdouble]

instance NFData CDouble -- Necessário para usar deepseq em GLfloat

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- função que faz a iteração Zn+1=Zn²+c e verifica se o valor absoluto de z e menor que dois se apos todas as iteraçoes for <= 2  retorna o ponto x y com a cor como sendo o valor 0 se nao for devolve o ponto com uma cor sendo o valor da iteração no qual e maior que 2
iteracaoMandelbrot::Iteracao->NroComplexo->NroComplexo->Pixel
iteracaoMandelbrot  0 complexo complexoIterado=  (realPart (complexo),imagPart (complexo),0) 
iteracaoMandelbrot iterador complexo complexoIterado
		| magnitude complexoIterado > 2 =(realPart (complexo),imagPart (complexo),iterador)
		| otherwise = iteracaoMandelbrot(iterador-1) complexo  zn
		where
			zn = (complexoIterado^2)+(complexo)

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- função que pega todos os pontos do plano complexo e para cada ponto chama a função iteraçãoMandelbrot retornando todos os pontos que fazem parte do mandelbrot com suas respectivas cores
mandelbrot l = map(\pixel->iteracaoMandelbrot numeroIteracaoes pixel pixel ) l

--------------------------------------------------------------------------------------------------------------------------------------------------------
-- função principal do programa t
main :: IO ()
main = do
	args<- getArgs
	let (tamanho) = processaArgs args
	let telaArg =  tamanho
        let passoArg =imageHeight'/telaArg	
	
        t0 <- getCurrentTime
	t1 <- (planoComplexo passoArg)  `deepseq` getCurrentTime
	t2 <- mandelbrot  (planoComplexo passoArg) `deepseq` getCurrentTime 
	let totalPontos = length(planoComplexo passoArg)
	-- deepseq é uma expressao que força uma avaliação completa do primeiro parametro e retorna o segundo
	putStrLn $ (show $ (totalPontos))  ++ ", " ++  (show $ diffUTCTime t1 t0) ++ ", " ++ (show $ diffUTCTime t2 t1) ++ "\n"

processaArgs args = case args of
     (a:_) -> (read a)
     _           -> (512)
