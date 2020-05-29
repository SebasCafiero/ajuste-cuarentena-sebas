import Text.Show.Functions
import Data.List

--1ra PARTE
--De cada empleado se conoce su nombre, su rol y cuánto gana por mes. 
data Empleado = UnEmpleado{
    nombreEmpleado::String,
    rolEmpleado::String,
    gananciaEmpleado::Float
} deriving (Show,Eq)

--Algunos empleados de ejemplo
palermo::Empleado
palermo = UnEmpleado "Martin Palermo" "Gerente de Produccion" 100000
riquelme::Empleado
riquelme = UnEmpleado "Juan Roman Riquelme" "Analista de Sistemas" 30000
bianchi::Empleado
bianchi = UnEmpleado "Carlos Bianchi" "Chief Executive Officer" 250000
delgado::Empleado
delgado = UnEmpleado "Marcelo Delgado" "Subgerente de Produccion" 85000
insua::Empleado
insua = UnEmpleado "Federico Insua" "Backend Developer" 80000
ibarra::Empleado
ibarra = UnEmpleado "Hugo Ibarra" "Ingeniero Capo Master" 55000
nandez::Empleado
nandez = UnEmpleado "Nahitan Nandez" "Carnero" 40000

--A los empleados se les pueden hacer propuestas laborales, que consisten en un nuevo rol y una función a aplicar sobre el salario.
cambiarRolA::String->Empleado->Empleado
cambiarRolA nuevoRol empleado = empleado{rolEmpleado = nuevoRol}

cambiarSalarioA::Float->Empleado->Empleado
cambiarSalarioA nuevoSalario empleado = empleado{gananciaEmpleado = nuevoSalario}

aumentarSalarioEn::Float->Empleado->Empleado
aumentarSalarioEn cantidad empleado = empleado{gananciaEmpleado = gananciaEmpleado empleado + cantidad}
disminuirSalarioEn::Float->Empleado->Empleado
disminuirSalarioEn cantidad empleado = empleado{gananciaEmpleado = gananciaEmpleado empleado - cantidad}

propuestaLaboral::String->(Empleado->Empleado)->Empleado->Empleado
propuestaLaboral nuevoRol funcion empleado = ((cambiarRolA nuevoRol).funcion) empleado

--Algunas propuestas de ejemplo
granAscenso::Empleado->Empleado
granAscenso = propuestaLaboral "Subgerente de Produccion" (cambiarSalarioA 85000)
mejorAscenso::Empleado->Empleado
mejorAscenso = propuestaLaboral "Chief Executive Officer" (cambiarSalarioA 300000)
irresponsable::Empleado->Empleado
irresponsable = disminuirSalarioEn 5000
suspension::Empleado->Empleado
suspension = propuestaLaboral "Suspendido" (cambiarSalarioA 0) 
eficiente::Empleado->Empleado
eficiente empleado = propuestaLaboral (rolEmpleado empleado ++ " Plus") (aumentarSalarioEn (gananciaEmpleado empleado)) empleado
cambioSegunSueldo::Empleado->Empleado
cambioSegunSueldo empleado
 | gananciaEmpleado empleado > 80000 = disminuirSalarioEn 10000 empleado
 | gananciaEmpleado empleado > 0 = empleado
 | otherwise = cambiarSalarioA 25000 empleado
serCarnero::Empleado->Empleado
serCarnero = cambiarRolA "Carnero"


--1) Saber si una propuesta es ilegal para una persona, que sucede cuando esa propuesta implica una disminución del salario.
propuestaIlegal::(Empleado->Empleado)->Empleado->Bool
propuestaIlegal propuesta empleado = gananciaEmpleado (propuesta empleado) < gananciaEmpleado empleado


--2) Saber si una propuesta es dudosa para un grupo de trabajadores, que sucede cuando para al menos un trabajador esa propuesta implica una disminución, 
--pero para al menos un otro trabajador, es un aumento (o el salario se mantiene igual).
propuestaDudosa::(Empleado->Empleado)->[Empleado]->Bool
propuestaDudosa propuesta empleados = any (propuestaIlegal propuesta) empleados


--3) Dada una lista de propuestas, saber cuánto ganaría ahora el trabajador si elige la que más le conviene.
aplicarPropuestasAEmpleado::[(Empleado->Empleado)]->Empleado->[Empleado]
aplicarPropuestasAEmpleado propuestas empleado = map (\propuesta->propuesta empleado) propuestas 

listaGananciasDeEmpleados::[Empleado]->[Float]
listaGananciasDeEmpleados empleados = map gananciaEmpleado empleados

gananciaDeMejorPropuestaAEmpleado::[(Empleado->Empleado)]->Empleado->Float
gananciaDeMejorPropuestaAEmpleado propuestas empleado = (maximum.listaGananciasDeEmpleados.aplicarPropuestasAEmpleado propuestas) empleado


--4) nosAhorramosGuita: saber cuánta plata se ahorra la empresa si hace una propuesta única a muchos empleados al mismo tiempo y todos la aceptan.
nosAhorramosGuita::[Empleado]->(Empleado->Empleado)->Float
nosAhorramosGuita empleados propuesta = sum (map gananciaEmpleado empleados) - sum (map gananciaEmpleado (map propuesta empleados))


--5) Realizar ciertas transformaciones sobre una lista de empleados:
--a) conLosOjosCerrados: quedarse con un solo “Ingeniero Capo Master” y el resto de “Backend developer” que haya. 
--(Quedarse con el primer o el ultimo “Ingeniero Capo Master” queda a libre decisión).
esRol::String->Empleado->Bool
esRol rol empleado = rolEmpleado empleado == rol

conLosOjosCerrados::[Empleado]->[Empleado]
conLosOjosCerrados empleados = (head (filter esIngeniero empleados)):(filter esDeveloper empleados)
 where esIngeniero empleado = ((esRol "Ingeniero Capo Master" empleado) || (esRol "Ingeniero Capo Master Plus" empleado))
       esDeveloper empleado = ((esRol "Backend Developer" empleado) || (esRol "Backend Developer Plus" empleado))
--QUIZAS HAY UNA MEJOR FORMA SIN FILTRAR DOS VECES LA LISTA DE EMPLEADOS


--b) reducciónViolenta: reducir en un número fijo todos los salarios de la empresa.
reduccionViolenta::Float->[Empleado]->[Empleado]
reduccionViolenta cantidad empleados = map (disminuirSalarioEn cantidad) empleados


--c) propuestaGeneral: darle una lista de propuestas a todos los trabajadores de una empresa, donde cada uno elija la que más le conviene y la acepte. 
--Podríamos por ejemplo darle a todos los empleados la elección entre cambiar su rol y ganar 10000 pesos más o cambiar su rol y ganar el doble que antes.
--La función debe retornar cómo quedan los empleados luego de aceptar las propuestas.
aplicarPropuestasAEmpleados::[(Empleado->Empleado)]->[Empleado]->[[Empleado]]
aplicarPropuestasAEmpleados propuestas empleados = map (aplicarPropuestasAEmpleado propuestas) empleados

esGananciaMaxima::[Empleado]->Empleado->Bool
esGananciaMaxima empleados empleado = maximum (listaGananciasDeEmpleados empleados) == gananciaEmpleado empleado

empleadoConMejorGanancia::[Empleado]->Empleado
empleadoConMejorGanancia empleados = (head.filter (esGananciaMaxima empleados)) empleados
--empleadoConMejorGanancia2 empleados = filter (\empleado->(maximum.listaGananciasDeEmpleados) empleados == gananciaEmpleado empleado) empleados

propuestaGeneral::[(Empleado->Empleado)]->[Empleado]->[Empleado]
propuestaGeneral propuestas empleados =  map empleadoConMejorGanancia ((aplicarPropuestasAEmpleados propuestas) empleados)


--d) soloLosQueCobranPoco: quedarse sólo con los que ganan menos que el promedio.
--Por ejemplo, podríamos usarla diciendo: soloLosQueCobranPoco [empleado1, empleado2]
ganaMenosQue::Float->Empleado->Bool
ganaMenosQue cantidad empleado = gananciaEmpleado empleado < cantidad

soloLosQueCobranPoco::[Empleado]->[Empleado]
soloLosQueCobranPoco empleados = filter (ganaMenosQue promedio) empleados 
 where promedio =  gananciasTotales / cantEmpleados
       gananciasTotales = ((sum.map gananciaEmpleado) empleados)
       cantEmpleados = fromIntegral (length empleados)
--Quise probar los usos del where


--e) Inventar una nueva transformación que en al menos una parte use una lambda con aplicación parcial.

--Para usar aplicacion parcial con la lambda cree otra funcion, pero podría haber usado la variable empleado directamente en disminuirSalarioEn
listaDeDisminuciones::[Empleado]->[(Empleado->Empleado)]
listaDeDisminuciones empleados = map (\empleado-> disminuirSalarioEn (gananciaEmpleado empleado / 2)) empleados

reducirALaMitad::[Empleado]->[Empleado]
reducirALaMitad empleados = zipWith ($) (listaDeDisminuciones empleados) empleados 
--PENSAR UNA MEJOR TRANSFORMACION


--2da PARTE
--Techín decide acelerar su proceso de cambio. Además, nos pide hacer medidas para cada una de sus empresas subsidiarias. 
--De cada una, se conoce su lista de empleados y su presupuesto para salarios.

data Empresa = UnaEmpresa{
    empleadosEmpresa::[Empleado],
    presupuestoEmpresa::Float
} deriving (Show,Eq)

--Algunas empresas de ejemplo
grosa,chica,pobre,pyme::Empresa
grosa = UnaEmpresa [bianchi,palermo,delgado] 500000
chica = UnaEmpresa [palermo,riquelme] 200000
pobre = UnaEmpresa [insua,ibarra,riquelme] 100000
pyme = UnaEmpresa [ibarra,insua,nandez] 160000

--1) Dada una lista de transformaciones (propuestaGeneral, soloLosQueCobranPoco, etc), 
--saber si luego de aplicar todos en serie ahora la subsidiaria puede pagar todos los salarios con su presupuesto
aplicarEnSerie::[(Empleado->Empleado)]->Empleado->Empleado
aplicarEnSerie transformaciones empleado = foldr ($) empleado (reverse transformaciones)

transformarEnSerie::[Empleado]->[(Empleado->Empleado)]->[Empleado]
transformarEnSerie empleados transformaciones = map (aplicarEnSerie transformaciones) empleados

puedePagarSueldos::Empresa->[(Empleado->Empleado)]->Bool
puedePagarSueldos empresa transformaciones = 
 (presupuestoEmpresa empresa) >= (sum.listaGananciasDeEmpleados.transformarEnSerie (empleadosEmpresa empresa)) transformaciones


--2) Ordenar una serie de subsidiarias en base a su Coeficiente de Huelgosidad (™, patente pendiente) que se calcula como:
--a. 1 punto por cada vez que tiene que reducir todos los salarios en 5000 para tener suficiente presupuesto.
--b. 10 puntos si no tiene ningún trabajador de rol “Carnero”.
ningunCarnero::Empresa->Bool
ningunCarnero empresa = all (\empleado->"Carnero" /= (rolEmpleado empleado)) (empleadosEmpresa empresa)

coeficienteHuelgosidad::Empresa->Int
coeficienteHuelgosidad empresa
 |not (puedePagarSueldos empresa []) = 
     1 + coeficienteHuelgosidad (empresa{empleadosEmpresa = transformarEnSerie (empleadosEmpresa empresa) [disminuirSalarioEn 5000]}) 
 |ningunCarnero empresa = 10
 |otherwise = 0

empresaConMayorHuelgosidad::[Empresa]->Empresa
empresaConMayorHuelgosidad empresas = foldl1 mayorHuelgosidadEntreDos empresas

mayorHuelgosidadEntreDos::Empresa->Empresa->Empresa
mayorHuelgosidadEntreDos empresa1 empresa2
 | coeficienteHuelgosidad empresa1 < coeficienteHuelgosidad empresa2 = empresa2
 | otherwise = empresa1

quitarEmpresa::[Empresa]->Empresa->[Empresa]
quitarEmpresa [] _ = []
quitarEmpresa (empresa:restoDeEmpresas) aQuitar
 | empresa == aQuitar = restoDeEmpresas --quitarEmpresa restoDeEmpresas aQuitar --Esto saca mas de una empresa si son iguales
 | otherwise = empresa:(quitarEmpresa restoDeEmpresas aQuitar)

ordenarSegunHuelgosidad::[Empresa]->[Empresa]
ordenarSegunHuelgosidad [] = []
ordenarSegunHuelgosidad empresas = empresaOrdenada : ordenarSegunHuelgosidad (quitarEmpresa empresas empresaOrdenada)
 where empresaOrdenada = empresaConMayorHuelgosidad empresas


--3) Si utilizamos una subsidiaria con infinitos empleados, ¿qué puntos funcionan? ¿qué puntos no?
{-
    Tanto el pto1 como el pto2 al sumarse una lista generada por cada empleado nunca se podria ejecutar y se trabaria.
    En el pto2 tambien se utiliza el all para detectar a los Carneros, por lo que tampoco se podra cumplir el all hasta recorrer toda la lista; en este 
caso funcionaría solo cuando el all da falso porque al hallar un caso que no cumple la condición frena, pero para lograr un verdadero nunca terminaría.
    Solo podrian funcionar en aquellos casos en los que baste la evaluacion diferida alcanzando la respuesta antes de recorrer toda la lista.
-}