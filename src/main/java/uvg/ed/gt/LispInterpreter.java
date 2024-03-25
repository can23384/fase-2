package uvg.ed.gt;

import java.util.*;

public class LispInterpreter {
    private static LispInterpreter interpreter;
    private Map<String, Object> environment;
    private Stack<Map<String, Object>> localEnvs = new Stack<>();
    private Map<String, Object> globalEnv = new HashMap<>();

    /**
     * Constructor privado para crear una instancia de LispInterpreter.
     * Se inicializa el entorno global con operadores y funciones básicas de Lisp.
     * Además, se definen y evalúan las funciones básicas (and, or, not) utilizando defun.
     */
    private LispInterpreter() {
        globalEnv.put("+", null);
        globalEnv.put("-", null);
        globalEnv.put("*", null);
        globalEnv.put("/", null);
        globalEnv.put("<", null);
        globalEnv.put(">", null);
        globalEnv.put("=", null);
        globalEnv.put("T", true);
        globalEnv.put("NIl", false);
        globalEnv.put("QUOTE", null);
        globalEnv.put("ATOM", null);
        globalEnv.put("LIST", null);
        globalEnv.put("EQUAL", null);
        globalEnv.put("COND", null);
        globalEnv.put("SETQ", null);
        globalEnv.put("DEFUN", null);
        localEnvs.push(globalEnv);

        String and = "defun and (h k) (cond ((= h nil) nil) ((= k nil) nil) (t t))";
        String or = "defun or (h k) (cond ((= h t) t) ((= k t) t) (t nil))";
        String not = "defun not (h) (cond (h nil) (t t))";
        try {
            eval(StringToLisp(or)); eval(StringToLisp(and)); eval(StringToLisp(not));
        } catch (Exception e) {
            System.err.println(e);
        }

    }

    public List<?> StringToLisp(String expression) throws Exception {
        List<Object> list = new ArrayList<>();
        int nlists = 0;
        String arg = "";
        boolean inList = false;
        boolean inArg = false;
        boolean inString = false;
        boolean inComentary = false;

        for (String c : expression.split("")) {
            if (c.isBlank()) {
                if (inArg && !inList && !inString) {
                    list.add(parseObject(arg));
                    inArg = false;
                    arg = "";

                } else if (inComentary && c.equals("\n")) {
                    inComentary = false;
                } else if (inArg) {
                    arg = arg.concat(c);
                } else {
                    continue;
                }
            } else {
                if (c.equals(";")) {
                    inComentary = true;
                    if (inArg) {
                        list.add(parseObject(arg));
                        arg = "";
                        inArg = false;
                    }
                    continue;
                }
                if (inComentary) {
                    continue;
                }
                if (c.equals("(") && !inString) {
                    if (inList) {
                        nlists += 1;
                        arg = arg.concat(c);
                    } else if (inArg) {
                        inArg = false; list.add(parseObject(arg)); arg = "";
                    }
                    inList = true;
                    inArg = true;
                    continue;
                }
                inArg = true;

                if (c.equals("\"") && !inList) {
                    inString = !inString;
                    arg = arg.concat(c);
                    if (!inString) {
                        list.add(arg); arg = ""; inArg = false;
                    }
                } else if (c.equals(")") && !inString) {
                    if (!inList) {
                        throw new Exception();
                    }
                    if (nlists > 0) {
                        nlists -= 1;
                        arg = arg.concat(c); continue;
                    }
                    inList = false; inArg = false;
                    list.add(StringToLisp(arg));
                    arg = "";

                } else {
                    arg = arg.concat(c);
                }
            }
        }
        if (inList || inString){
            throw new Exception();
        }
        if (inArg) {
            list.add(parseObject(arg));
        }
        return list;
    }

    public static LispInterpreter getInterpreter() {
        if (interpreter == null) {
            interpreter = new LispInterpreter();
        }
        return interpreter;
    }

    public void updateGlobalEnv() {
        globalEnv = localEnvs.peek();
    }

    public Stack<Map<String, Object>> getLocalEnvs() {
        return localEnvs;
    }

    public Object parseObject(String element) {
        Object toType = null;
        try {
            toType = Integer.parseInt(element);
        } catch (Exception e) {
            try {
                toType = Double.parseDouble(element);
            } catch (Exception e2) {
                if (element.toUpperCase().equals("T")) {
                    toType = true;
                } else if (element.toUpperCase().equals("NIL")){
                    toType = false;
                } else {
                    toType = (String) element;
                }
            }
        }
        return toType;
    }

    public Object getArg(Object arg)  throws Exception{
        if (List.class.isAssignableFrom(arg.getClass())) {
            return eval((List<?>) arg);
        } else if (arg.getClass() == String.class && globalEnv.containsKey(arg)) {
            return globalEnv.get(arg);
        }
        return arg;
    }

    public Object eval(List<?> list) throws Exception {
        String first = (String) list.get(0);
        int numberOfParams = list.size() - 1;
        globalEnv = localEnvs.peek();
        switch (first.toUpperCase()) {
            case "+":
                return suma(list.subList(1, list.size()));
            case "-":
                return substract(list.subList(1, list.size()));
            case "*":
                return multiplicar(list.subList(1, list.size()));
            case "/":
                return division(list.subList(1, list.size()));
            case ">":
                return moreThan(list.subList(1, list.size()));
            case "<":
                return lessThan(list.subList(1, list.size()));
            case "=":
                return equal(list.subList(1, list.size()));
            case "EQUAL":
                if (numberOfParams != 2) {
                    throw new Exception();
                }
                return equal(list.subList(1, list.size()));
            case "ATOM":
                if (numberOfParams != 1) {
                    throw new Exception();
                }
                return atom(list.subList(1, list.size()));
            case "LIST":
                return list(list.subList(1, list.size()));
            case "COND":
                return cond(list.subList(1, list.size()));
            case "QUOTE":
                if (numberOfParams != 1) {
                    throw new Exception();
                }
                return quote(list.subList(1, list.size()));
            case "SETQ":
                if (numberOfParams != 2) {
                    throw new Exception();
                }
                setq(list.subList(1, list.size()));
                return null;
            case "DEFUN":
                if (numberOfParams != 3) {
                    throw new Exception();
                }
                defun(list.subList(1, list.size()));
                return null;
            default:
                Object item = globalEnv.get(first);
                if (item == null) {
                    throw new Exception(first);
                }
                if (item.getClass() == LispFunction.class) {
                    if (numberOfParams == ((LispFunction) item).getParams().size()) {
                        Object result = ((LispFunction) item).apply(list.subList(1, list.size()));
                        return result;
                    } else {
                        throw new Exception(first);
                    }
                } else {
                    throw new Exception(first);
                }

        }

    }
    /**
     * Realiza la suma de los elementos en la lista de argumentos.
     * Si alguno de los argumentos es una lista, se evalúa primero.
     * @param args Lista de argumentos a sumar.
     * @return El resultado de la suma.
     * @throws Exception Si ocurre un error durante la suma.
     */
    public Object suma(List<?> args) throws Exception {
        Object result = 0; boolean stillInt = true;
        for (Object o : args) {
            Object tempResult;
            if (List.class.isAssignableFrom(o.getClass())) {
                tempResult = eval((List<?>) o );
            } else if (o.getClass() == String.class && globalEnv.containsKey(o)) {
                tempResult = globalEnv.get(o);
            } else {
                tempResult = o;
            }
            if (tempResult.getClass() == Integer.class) {
                if (stillInt) {
                    result = (Integer) result + (Integer) tempResult;
                } else {
                    result = (Double) result + (Integer) tempResult;
                }

            } else {
                if (stillInt) {
                    result = (Integer) result + (Double) tempResult;
                    stillInt = false;
                } else {
                    result = (Double) result + (Double) tempResult;
                }
            }


        }
        return result;
    }
    /**
     * Realiza la resta de los elementos en la lista de argumentos.
     * @param args Lista de argumentos.
     * @return El resultado de la resta.
     * @throws Exception Si no hay argumentos o si hay un error durante la operación.
     */
    public Object substract(List<?> args) throws Exception {
        if (args.isEmpty()) {
            throw new Exception("No hay argumentos");
        }

        Object result = getArg(args.get(0));
        if (! (result instanceof Number)) {
            throw new Exception("número: " + args.get(0));
        }

        for (int i = 1; i < args.size(); i++) {
            Object next = getArg(args.get(i));
            if (! (next instanceof Number)) {
                throw new Exception("número: " + args.get(i));
            }
            result = ((Number) result).doubleValue() - ((Number) next).doubleValue();
        }

        return result;
    }
    /**
     * Realiza la multiplicación de los elementos en la lista de argumentos.
     * @param args Lista de argumentos.
     * @return El resultado de la multiplicación.
     * @throws Exception Si no hay argumentos o si hay un error durante la operación.
     */
    public Object multiplicar(List<?> args) throws Exception {
        if (args.isEmpty()) {
            throw new Exception("No argumentos");
        }

        double result = 1.0;

        for (Object arg : args) {
            result *= ((Number) getArg(arg)).doubleValue();
        }

        return result;
    }
    /**
     * Realiza la división de los elementos en la lista de argumentos.
     * @param args Lista de argumentos.
     * @return El resultado de la división.
     * @throws Exception Si hay un número incorrecto de argumentos o si hay un error durante la operación.
     */
    public Object division(List<?> args) throws Exception {
        if (args.size() != 2) {
            throw new Exception("");
        }

        double numerator = ((Number) getArg(args.get(0))).doubleValue();
        double denominator = ((Number) getArg(args.get(1))).doubleValue();

        if (denominator == 0) {
            throw new Exception("");
        }

        return numerator / denominator;
    }
    /**
     * Comprueba si el primer argumento es mayor que el segundo.
     * @param args Lista de argumentos.
     * @return true si el primer argumento es mayor que el segundo, false en caso contrario.
     * @throws Exception Si el número de argumentos no es igual a dos o si ocurre un error durante la operación.
     */
    public boolean moreThan(List<?> args) throws Exception {
        if (args.size() != 2) {
            throw new Exception();
        }
        Object arg1 = getArg(args.get(0));
        Object arg2 = getArg(args.get(1));
        if (arg1 instanceof Number && arg2 instanceof Number) {
            return ((Number) arg1).doubleValue() > ((Number) arg2).doubleValue();
        } else {
            throw new Exception();
        }
    }
    /**
     * Comprueba si el primer argumento es menor que el segundo.
     * @param args Lista de argumentos.
     * @return true si el primer argumento es menor que el segundo, false en caso contrario.
     * @throws Exception Si el número de argumentos no es igual a dos o si ocurre un error durante la operación.
     */
    public boolean lessThan(List<?> args) throws Exception {
        if (args.size() != 2) {
            throw new Exception();
        }
        Object arg1 = getArg(args.get(0));
        Object arg2 = getArg(args.get(1));
        if (arg1 instanceof Number && arg2 instanceof Number) {
            return ((Number) arg1).doubleValue() < ((Number) arg2).doubleValue();
        } else {
            throw new Exception();
        }
    }
    /**
     * Comprueba si dos argumentos son iguales.
     * @param args Lista de argumentos.
     * @return true si los argumentos son iguales, false en caso contrario.
     * @throws Exception Si el número de argumentos no es igual a dos.
     */
    public boolean equal(List<?> args) throws Exception {
        if (args.size() != 2) {
            throw new Exception();
        }
        Object arg1 = getArg(args.get(0));
        Object arg2 = getArg(args.get(1));
        return arg1.equals(arg2);
    }

    private boolean isEqual(List<?> args) {
        if (args.size() < 2) {
            throw new RuntimeException("Se requieren dos argumentos");
        }
        Object first = evaluateNested(args.get(0));
        for (int i = 1; i < args.size(); i++) {
            if (!first.equals(evaluateNested(args.get(i)))) {
                return false;
            }
        }
        return true;
    }

    public Object eval(Object expression) {
        if (expression instanceof Integer || expression instanceof Double) {
            return expression;
        } else if (expression instanceof String) {
            String expStr = (String) expression;
        } else {
            throw new RuntimeException("funcion desconocida " );
        }

        throw new RuntimeException("expresion desconocida " + expression);
    }

    private Object evaluateNested(Object expression) {
        if (expression instanceof List) {
            return eval(expression);
        } else {
            return expression;
        }
    }
    /**
     * Comprueba si el primer argumento no es una lista.
     * @param args Lista de argumentos.
     * @return true si el argumento no es una lista, false en caso contrario.
     * @throws Exception Si el número de argumentos no es igual a uno.
     */
    public boolean atom(List<?> args) throws Exception {
        if (args.size() != 1) {
            throw new Exception();
        }
        Object arg = getArg(args.get(0));
        return !(arg instanceof List);

    }
    /**
     * Comprueba si el argumento es una lista.
     * @param args Lista de argumentos.
     * @return true si el argumento es una lista, false en caso contrario.
     * @throws Exception Si el número de argumentos no es igual a uno.
     */
    public boolean list(List<?> args) throws Exception {
        if (args.size() != 1) {
            throw new Exception();
        }
        Object arg = args.get(0);
        return arg instanceof List;
    }

    /**
     * Evalúa una lista de expresiones condicionales y devuelve el resultado de la primera condición verdadera.
     * @param args Lista de expresiones condicionales.
     * @return El resultado de la primera expresión verdadera, o null si ninguna expresión es verdadera.
     * @throws Exception Si las expresiones condicionales no están bien formadas.
     */
    public Object cond(List<?> args) throws Exception {
        for (Object arg : args) {
            if (!(arg instanceof List) || ((List<?>) arg).size() != 2) {
                throw new Exception("");
            }

            List<?> pair = (List<?>) arg;
            Object condition = pair.get(0);
            Object expression = pair.get(1);

            Object conditionResult;
            if (condition instanceof List) {
                conditionResult = eval((List<?>) condition);
            } else if (condition instanceof String && globalEnv.containsKey(condition)) {
                conditionResult = globalEnv.get(condition);
            } else {
                conditionResult = condition;
            }

            if (Boolean.TRUE.equals(conditionResult)) {
                if (expression instanceof List) {
                    return eval((List<?>) expression);
                } else if (expression instanceof String && globalEnv.containsKey(expression)) {
                    return globalEnv.get(expression);
                } else {
                    return expression;
                }
            }
        }

        return null;    }
    /**
     * Devuelve la expresión pasada como argumento sin evaluarla.
     * @param args Lista que contiene la expresión a retornar.
     * @return La expresión sin evaluar.
     * @throws Exception Si la lista de argumentos no contiene exactamente una expresión.
     */
    public Object quote(List<?> args) throws Exception {

        if (args.size() != 1) {
            throw new Exception("");
        }

        return args.get(0);
    }
    /**
     * Asigna un valor a una variable en el entorno global.
     * @param args Lista que contiene el nombre de la variable y el valor a asignar.
     * @throws Exception Si los argumentos no están correctamente formados o si ocurre un error durante la asignación.
     */
    public void setq(List<?> args) throws Exception {
        if (args.get(0).getClass() != String.class) {
            throw new Exception();
        }
        String name = (String) args.get(0);
        if (name.contains("\"")) {
            throw new Exception();
        }
        if (args.get(1).getClass() == String.class) {
            String asignment = (String) args.get(1);
            if (globalEnv.get(asignment) != null) {
                if (globalEnv.get(asignment).getClass() == LispFunction.class) {
                    throw new Exception();
                } else if (!asignment.contains("\"")) {
                    setq(List.of(name, globalEnv.get(asignment)));
                    return;
                }
            } else if (!asignment.contains("\"")) {
                throw new Exception();
            }
        }

        globalEnv.put(name, getArg(args.get(1)));


        return;
    }
    /**
     * Define una nueva función y la agrega al entorno global.
     * @param args Lista que contiene el nombre de la función, los parámetros y el cuerpo de la función.
     * @throws Exception Si los argumentos no están correctamente formados.
     */
    public void defun(List<?> args) throws Exception {
        String name = (String) args.get(0);
        if (name.contains("\"")) {
            throw new Exception();
        }
        List<?> params = (List<?>) args.get(1);
        List<?> body = (List<?>) args.get(2);
        globalEnv.put(name, new LispFunction(name, params, body));
        return;
    }
    /**
     * Representa una función en el lenguaje Lisp.
     */
    public class LispFunction {
        protected String name;
        protected List<?> params;
        protected List<?> body;
        /**
         * Aplica la función a una lista de argumentos.
         * @param argList Lista de argumentos para la función.
         * @return El resultado de aplicar la función a los argumentos.
         * @throws Exception Si ocurre un error durante la evaluación de la función.
         */
        public Object apply(List<?> argList) throws Exception {
            Stack<Map<String, Object>> localEnvs = LispInterpreter.getInterpreter().getLocalEnvs();
            localEnvs.push(new HashMap<>(localEnvs.peek()));

            for (int i = 0; i < params.size(); i++) {
                Object arg = argList.get(i);
                if (List.class.isAssignableFrom(arg.getClass())){
                    arg = LispInterpreter.getInterpreter().eval((List<?>)arg);
                } else if (arg.getClass() == String.class && localEnvs.peek().containsKey(arg)) {
                    arg = LispInterpreter.getInterpreter().getLocalEnvs().peek().get(arg);
                }

                localEnvs.peek().put((String) params.get(i), arg);
            }

            Object result = null;
            result = LispInterpreter.getInterpreter().eval(body);

            localEnvs.pop();
            LispInterpreter.getInterpreter().updateGlobalEnv();
            return result;
        }
        /**
         * Constructor de la clase LispFunction.
         * @param name El nombre de la función.
         * @param params Los parámetros de la función.
         * @param body El cuerpo de la función.
         */
        public LispFunction(String name, List<?> params, List<?> body) {
            this.name = name;
            this.params = params;
            this.body = body;
        }
        /**
         * Obtiene la lista de parámetros de la función.
         * @return La lista de parámetros.
         */
        public List<?> getParams() {
            return params;
        }

    }

}