import Text.Show.Functions

--De cada empleado se conoce su nombre, su rol y cuánto gana por mes. 
data Empleado = UnEmpleado{
    nombreEmpleado::String,
    rolEmpleado::String,
    gananciaEmpleado::Float
} deriving Show

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
cambioSegunSueldo::Empleado->Empleado
cambioSegunSueldo empleado
 | gananciaEmpleado empleado > 80000 = disminuirSalarioEn 10000 empleado
 | gananciaEmpleado empleado > 0 = empleado
 | otherwise = cambiarSalarioA 30000 empleado


--1) Saber si una propuesta es ilegal para una persona, que sucede cuando esa propuesta implica una disminución del salario.
propuestaIlegal::(Empleado->Empleado)->Empleado->Bool
propuestaIlegal propuesta empleado = gananciaEmpleado (propuesta empleado) < gananciaEmpleado empleado


--2) Saber si una propuesta es dudosa para un grupo de trabajadores, que sucede cuando para al menos un trabajador esa propuesta implica una disminución, 
--pero para al menos un otro trabajador, es un aumento (o el salario se mantiene igual).
propuestaDudosa::(Empleado->Empleado)->[Empleado]->Bool
propuestaDudosa propuesta empleados = any (propuestaIlegal propuesta) empleados


--3) Dada una lista de propuestas, saber cuánto ganaría ahora el trabajador si elige la que más le conviene.
mejorGanancia::Empleado->[(Empleado->Empleado)]->Float
mejorGanancia empleado propuestas = (maximum.(map gananciaEmpleado).(map (\propuesta->propuesta empleado))) propuestas


--4) nosAhorramosGuita: saber cuánta plata se ahorra la empresa si hace una propuesta única a muchos empleados al mismo tiempo y todos la aceptan.
nosAhorramosGuita::[Empleado]->(Empleado->Empleado)->Float
nosAhorramosGuita empleados propuesta = sum (map gananciaEmpleado empleados) - sum (map gananciaEmpleado (map propuesta empleados))


--5) Realizar ciertas transformaciones sobre una lista de empleados:
--a) conLosOjosCerrados: quedarse con un solo “Ingeniero Capo Master” y el resto de “Backend developer” que haya. 
--(Quedarse con el primer o el ultimo “Ingeniero Capo Master” queda a libre decisión).
--conLosOjosCerrados empleados = 


--b) reducciónViolenta: reducir en un número fijo todos los salarios de la empresa.
reduccionViolenta::[Empleado]->Float->[Empleado]
reduccionViolenta empleados cantidad = map (disminuirSalarioEn cantidad) empleados