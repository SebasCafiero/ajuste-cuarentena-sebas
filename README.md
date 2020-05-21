# Ajuste de cuarentena

Techín, fábrica de galletitas, está intentando acortar sus costos por todos lados, y para eso nos pide de raje un sistema para reajustar sus costos laborales. 

- De cada empleado se conoce su nombre, su rol y cuánto gana por mes. 
- A los empleados se les pueden hacer propuestas laborales, que consisten en un nuevo rol y una función a aplicar sobre el salario. Por ejemplo, algunas propuestas podrían ser:
    - Cambiarle el rol a “Ingeniero Capo Master” y subirle el salario en 10 pesos.	
    - Cambiarle el rol a “Backend developer” y hacer que su salario aumente  a  100000 pesos en el caso de que sea menor a dicha suma.

1. Saber si una propuesta es ilegal para una persona, que sucede cuando esa propuesta implica una disminución del salario
2. Saber si una propuesta es dudosa para un grupo de trabajadores, que sucede cuando para al menos un trabajador esa propuesta implica una disminución, pero para al menos un otro trabajador, es un aumento (o el salario se mantiene igual).
3. Dada una lista de propuestas, saber cuánto ganaría ahora el trabajador si elige la que más le conviene
4. **nosAhorramosGuita**: saber cuánta plata se ahorra la empresa si hace una propuesta única a muchos empleados al mismo tiempo y todos la aceptan.
5. Realizar ciertas transformaciones sobre una lista de empleados:
    - a. **conLosOjosCerrados**: quedarse con un solo “Ingeniero Capo Master” y el resto de “Backend developer” que haya. (Quedarse con el primer o el ultimo “Ingeniero Capo Master” queda a libre decisión)
    - b. **reducciónViolenta**: reducir en un número fijo todos los salarios de la empresa
    - c. **propuestaGeneral**: darle una lista de propuestas a todos los trabajadores de una empresa, donde cada uno elija la que más le conviene y la acepte. Podríamos por ejemplo darle a todos los empleados la elección entre cambiar su rol y ganar 10000 pesos más o cambiar su rol y ganar el doble que antes. La función debe retornar cómo quedan los empleados luego de aceptar las propuestas.
    - d. **soloLosQueCobranPoco**: quedarse sólo con los que ganan menos que el promedio. Por ejemplo, podríamos usarla diciendo: 
    soloLosQueCobranPoco [empleado1, empleado2]
    - e. Inventar una nueva transformación que en al menos una parte use una lambda con aplicación parcial.


## ¿No era sólo para la cuarentena?

Visto el éxito de las medidas, y con la cuarentena ya cerrada, Techín decide acelerar su proceso de cambio. Además, nos pide hacer medidas para cada una de sus empresas subsidiarias. De cada una, se conoce su lista de empleados y su presupuesto para salarios.

1. Dada una lista de transformaciones (propuestaGeneral, soloLosQueCobranPoco, etc), saber si luego de aplicar todos en serie ahora la subsidiaria puede pagar todos los salarios con su presupuesto	
2. Ordenar una serie de subsidiarias en base a su Coeficiente de Huelgosidad (™, patente pendiente) que se calcula como:
    - a. 1 punto por cada vez que tiene que reducir todos los salarios en 5000 para tener suficiente presupuesto
    - b. 10 puntos si no tiene ningún trabajador de rol “Carnero”
3. Si utilizamos una subsidiaria con infinitos empleados, ¿qué puntos funcionan? ¿qué puntos no?
