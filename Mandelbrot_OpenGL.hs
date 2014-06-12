import Graphics.UI.GLUT
import Data.Complex
import Data.Time.Clock
import Control.DeepSeq
import Foreign.C.Types
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- definiçao das constantes e dos numeros complexos que serão iterados mais tarde no programa
imageHeight' = 4.0:: GLdouble
imageWidth' =  imageHeight'  
tela = 512                   

passo = imageHeight'/(fromIntegral tela)

numeroIteracaoes = 50	

plano = [(width,height) | width<-[-imageWidth'/2,-imageWidth'/2+passo..imageWidth'/2],height<-[-imageHeight'/2,-imageHeight'/2+passo..imageHeight'/2]]
planoComplexo = map(\(real,imaginario)->real:+imaginario) plano


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
--verificaPlanoComplexo::PlanoComplexo->Pontos
--verificaPlanoComplexo [] = []
--verificaPlanoComplexo (pixel:plano) =verificaPlanoComplexo plano ++ iteracaoMandelbrot numeroIteracaoes pixel pixel

-- Transformado em função para separar geração da lista de entrada e cálculo
verificaPlanoComplexo l = map(\pixel->iteracaoMandelbrot numeroIteracaoes pixel pixel ) l

--------------------------------------------------------------------------------------------------------------------------------------------------------
-- função principal do programa na qual chama o opengl para desenhar o mandelbrot
main :: IO ()
main = do
	let tamanho = length(planoComplexo)
	print (tamanho) 
        t0 <- getCurrentTime
	t1 <- planoComplexo `deepseq` getCurrentTime
	t2 <- verificaPlanoComplexo  planoComplexo `deepseq` getCurrentTime 
	-- deepseq é uma expressao que força uma avaliação completa do primeiro parametro e retorna o segundo
	putStrLn $ (show $ diffUTCTime t1 t0) ++ ", " ++ (show $ diffUTCTime t2 t1) ++ "\n"
        
	
	(_progName, _args) <- getArgsAndInitialize
	initialDisplayMode $= [ RGBAMode ]
  	initialWindowSize $= Size tela tela
	_window <- createWindow "Mandelbrot"
	let x0 = (-imageWidth'/2)
	    xf = imageWidth'/2
	    y0 = (-imageHeight'/2)
	    yf = imageHeight'/2
	ortho2D x0 xf y0 yf
  	displayCallback $= display(passo/2)
  	mainLoop

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- função opengl onde se define os pontos que serão desenhadas na tela e suas respectivas cores
display :: GLdouble-> DisplayCallback
display passo= do  
  clear [ ColorBuffer ]
  let imagem =  verificaPlanoComplexo planoComplexo
  renderPrimitive Quads $ do
	mapM_(\(x,y,z)-> preservingMatrix $ do
		let numeroCores = 30
		let resto =(fromIntegral (z `mod` numeroCores)::GLdouble)
		let diferenca = 3/fromIntegral numeroCores 		
		let cor =   diferenca* resto
		let b = 3.0-cor
		let g = 2.0-cor
		if cor>2 then color $ (Color3 (1.0::GLdouble) 1 b)
		else if cor<=2 && cor>1 then color $ (Color3 (1.0::GLdouble) g 0)
		else color $ (Color3 ( cor::GLdouble) 0 0)	

		vertex $ Vertex2 (x-passo) (y+passo) 
         	vertex $ Vertex2 (x+passo) (y+passo)
         	vertex $ Vertex2 (x+passo) (y-passo)               
         	vertex $ Vertex2 (x-passo) (y-passo)		
					
		)  imagem
    	
  flush
