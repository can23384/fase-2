package uvg.ed.gt;

import java.util.*;

/**
 * Clase principal que contiene el método main para ejecutar el intérprete de Lisp.
 */
public class Main {

    /**
     * Método principal que ejecuta el intérprete de Lisp con una serie de entradas predefinidas.
     *
     * @param args Los argumentos de la línea de comandos (no se utilizan en este programa).
     * @throws Exception Si ocurre algún error durante la ejecución del intérprete de Lisp.
     */
    public static void main(String[] args) throws Exception {
        // Se obtiene una instancia del intérprete de Lisp
        LispInterpreter lispInterpreter = LispInterpreter.getInterpreter();

        // Entradas de ejemplo para el intérprete de Lisp
        String[] inputs = {
                "(+ 1 2)",
                "(- 5 3)",
                "(cond ((> 3 2) \"Es mayor\") ((< 3 2) \"Es menor\"))",
                "(setq x 10)",
                "x",
                "(defun cuadrado (x) (* x x))",
                "(cuadrado 5)",
                "(defun suma (a b) (+ a b))",
                "(suma 3 4)"
        };

        // Se procesan las entradas
        for (String input : inputs) {
            try {
                // Se convierte la cadena de entrada en una lista de argumentos para el intérprete de Lisp
                List<?> argList = lispInterpreter.StringToLisp(input);

                // Se evalúa cada argumento
                for (Object arg : argList) {
                    if (List.class.isAssignableFrom(arg.getClass())) {
                        try {
                            // Se evalúa el argumento y se imprime el resultado
                            Object output = lispInterpreter.eval((List<?>) arg);
                            if (output != null) {
                                System.out.println(output);
                            }
                        } catch (Exception e) {
                            // Se maneja cualquier excepción que ocurra durante la evaluación
                            System.err.println("* Exception: Error al ejecutar comando!  " + e.getMessage());
                        }

                    }
                }
            }
            // Se maneja cualquier error de sintaxis en la entrada
            catch (Exception e1) {
                System.err.println("* Exception: Error de sintáxis en la instrucción");
            }
        }
    }
}

