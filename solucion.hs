data Persona = UnaPersona {
    acciones :: [String],
    trabajo :: Trabajo,
    salud :: Float,
    esperanza :: Bool
} deriving Show

data Trabajo = 
    Desempleado |
    Informal Float |
    Registrado Float Float
    deriving Show
    
data Santo = UnSanto {
   requisito :: [String],
   bendicion :: Persona -> Persona
} 

salarioBasico,salarioBajo::Float
salarioBasico = 10000
salarioBajo = 15000

--Santos
sanCayetano, difuntaCorrea, angelelli::Santo
sanCayetano = UnSanto {
  requisito = ["compartir","esfuerzo","tener una estampita"], 
  bendicion = mejorarTrabajo}

difuntaCorrea = UnSanto {
  requisito = ["dejar una botella"],
  bendicion = mejorarSalud}

angelelli = UnSanto {
  requisito = ["ayudar a la gente","donacion"],
  bendicion = darEsperanza}

-- Bendiciones de los santos
mejorarTrabajo::Persona->Persona
mejorarTrabajo persona = persona{trabajo = mejoraSituacion (trabajo persona)}

mejoraSituacion::Trabajo->Trabajo
mejoraSituacion Desempleado = Informal salarioBasico
mejoraSituacion (Informal ingreso) 
  | ingreso < salarioBajo = Informal (2*ingreso)
  | otherwise = Registrado ingreso 0
mejoraSituacion (Registrado ingreso adicional) = Registrado ingreso (ingreso + adicional) 

mejorarSalud::Persona->Persona
mejorarSalud persona = persona {salud = salud persona + 1}

darEsperanza::Persona->Persona
darEsperanza persona = persona {esperanza = True}

-- Personas
pepe,luis::Persona
pepe = UnaPersona {
  acciones = ["dejar una botella","esfuerzo"],
  trabajo = Informal 20000,
  salud = 90,
  esperanza = True}
  
luis = UnaPersona {
  acciones = ["ayudar a la gente","compartir"],
  trabajo = Desempleado,
  salud = 30,
  esperanza = True}
  
-- Visita al santuario
visitaAlSantuario :: Santo -> Persona -> Persona
visitaAlSantuario santo persona 
  | cumpleRequisito persona santo = (bendicion santo) persona
  | otherwise = persona

cumpleRequisito :: Persona -> Santo -> Bool
cumpleRequisito persona santo = any (`elem` (requisito santo) ) (acciones persona)

-- Fiesta patronal
fiestaPatronal ::  Santo -> [Persona] -> [Persona]
fiestaPatronal santo personas =  map (visitaAlSantuario santo.visitaAlSantuario santo) personas

-- Festival
festival::[Santo] -> [Persona] -> [Persona]
festival santos personas =  map (bendicionSucesiva santos) (filter esperanza personas)

bendicionSucesiva::[Santo]->Persona->Persona
bendicionSucesiva santos persona = foldl (flip visitaAlSantuario) persona santos 

-- Nuevo santo
maradona::Santo
maradona = UnSanto{
  requisito = ["eeeeeeh"],
  bendicion = darEsperanza }
