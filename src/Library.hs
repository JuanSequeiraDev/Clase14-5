module Library where
import PdePreludat

data Heroe = Heroe
  { defensa :: Number,
    salud :: Number,
    ataque :: Number
  } deriving (Show, Eq)

type Modificacion = Number -> Number

pepita :: Heroe
pepita = Heroe {defensa = 21, salud = 1001, ataque = 51}


type Pocion = Heroe -> Heroe

poder :: Heroe -> Number
poder heroe = 3 * defensa heroe + ataque heroe + salud heroe / 2

cambiarSalud :: Modificacion -> Heroe -> Heroe
cambiarSalud modificacion heroe = heroe { salud = modificacion (salud heroe) }

cambiarDefensa :: Modificacion -> Heroe -> Heroe
cambiarDefensa modificacion heroe = heroe { defensa = modificacion (defensa heroe) }

cambiarAtaque :: Modificacion  -> Heroe -> Heroe
cambiarAtaque modificacion heroe = heroe { ataque = modificacion (ataque heroe) }

sumarVida :: Number -> Heroe -> Heroe
sumarVida cantidad heroe = cambiarSalud (+ cantidad) heroe

-- EJ4
pocionBase :: Pocion
pocionBase heroe = sumarVida 10 heroe

pocionPremium :: Pocion
pocionPremium = pocionBase.pocionBase

crazyPotion :: Pocion
crazyPotion =  cambiarSalud (*1.33).pocionPremium.cambiarAtaque (* 2) 

agua :: Pocion
agua = id

esPoderoso :: Heroe -> Bool
esPoderoso heroe = poder heroe > 100

pocionElite :: Pocion
pocionElite heroe | esPoderoso heroe = cambiarDefensa (*10) heroe
                  | otherwise = agua heroe

pocionArtesanal :: Number -> Pocion
pocionArtesanal cant = pocionBase.cambiarAtaque (/cant).pocionBase

pocionArriesgada :: Pocion
pocionArriesgada = crazyPotion.(\heroe-> heroe {defensa = 3}).pocionBase

licuadoDePociones :: Pocion -> Pocion
licuadoDePociones pocion = pocionBase.pocion.pocionArtesanal (10)

pocionGradual :: Pocion 
pocionGradual heroe | ataque heroe > 100 = (cambiarDefensa (+50).pocionArtesanal (5)) heroe
                    | ataque heroe >= 50 = (cambiarDefensa (+30).pocionArtesanal (5)) heroe
                    | ataque heroe < 50 = (cambiarDefensa (+10).pocionArtesanal(5)) heroe

pocionVampirica :: Pocion
pocionVampirica heroe | defensa heroe > salud heroe = (cambiarDefensa (subtract(20)).cambiarSalud (+20)) heroe
                      | otherwise = (cambiarSalud(subtract(10)).cambiarAtaque(+30)) heroe
