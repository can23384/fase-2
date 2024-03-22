import java.util.List;

/**
 * Interprete para Lisp en Java
 * 
 * @author AREA
 * @since febrero 2024
 */
public class Main {
    public static void main(String[] args) throws Exception {
        LispInterpreter lispInterpreter = LispInterpreter.getInterpreter(); // instancia del intérprete

        // Definición de expresiones predefinidas
        String[] inputs = {
            "(+ 1 2)",
            "(- 5 3)",
            "(cond ((> 3 2) \"Es mayor\") ((< 3 2) \"Es menor\"))",
            "(setq x 10)",
            "x",
            "(defun cuadrado (x) (* x x))",
            "(cuadrado 5)",
            "(AND T NIL)",
            "(OR NIL T)",
            "(defun suma (a b) (+ a b))",
            "(suma 3 4)"
        };

        for (String input : inputs) {
            try {
                List<?> argList = lispInterpreter.StringToLisp(input); // Conversión de texto a formato de lista de java
                
                // Como ocurre en Lisp, es posible ingresar varias listas a la vez y también datos sueltos. 
                // Por lo tanto para cada argumento se verifica que sea de tipo lista para poder ejecutar una función
                for (Object arg : argList) {
                    if (List.class.isAssignableFrom(arg.getClass())) {// verifica que el objeto es una implementación de lista
                        try {
                            Object output = lispInterpreter.eval((List<?>) arg); 
                            if (output != null) {
                                System.out.println(output); // Evalúa la función, opcionalmente se decidió imprimir el resultado
                            }
                        } catch (Exception e) {
                            System.err.println("* Exception: Error al ejecutar comando!  " + e.getMessage()); // Mensaje de error si la ejecución falla
                        }
                        
                    }
                }
            }
            catch (Exception e1) {
                System.err.println("* Exception: Error de sintáxis en la instrucción"); // Mensaje de error para comandos mal escritos
            }
        }
    }
}