import java.util.*;


public class Main {
    public static void main(String[] args) throws Exception {
        LispInterpreter lispInterpreter = LispInterpreter.getInterpreter(); 

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

        for (String input : inputs) {
            try {
                List<?> argList = lispInterpreter.StringToLisp(input); 

                for (Object arg : argList) {
                    if (List.class.isAssignableFrom(arg.getClass())) {
                        try {
                            Object output = lispInterpreter.eval((List<?>) arg); 
                            if (output != null) {
                                System.out.println(output);
                            }
                        } catch (Exception e) {
                            System.err.println("* Exception: Error al ejecutar comando!  " + e.getMessage()); 
                        }
                        
                    }
                }
            }
            catch (Exception e1) {
                System.err.println("* Exception: Error de sintáxis en la instrucción"); 
            }
        }
    }
}