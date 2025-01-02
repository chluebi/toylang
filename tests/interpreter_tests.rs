use toylang::runner;
use toylang::interpreter;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recursive_fib() {
        match runner::eval("programs/recursive_fib".to_string()) {
            Ok(interpreter::Value::Int(4181)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn factorial() {
        match runner::eval("programs/factorial".to_string()) {
            Ok(interpreter::Value::Int(3628800)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn collatz() {
        match runner::eval("programs/collatz".to_string()) {
            Ok(interpreter::Value::Int(1)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn square() {
        match runner::eval("programs/square".to_string()) {
            Ok(interpreter::Value::Bool(true)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn tuples() {
        match runner::eval("programs/tuples".to_string()) {
            Ok(interpreter::Value::Int(4950)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn lists() {
        match runner::eval("programs/lists".to_string()) {
            Ok(interpreter::Value::Int(9900)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn circular() {
        match runner::eval("programs/circular".to_string()) {
            Ok(interpreter::Value::Int(1)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn powerset() {
        // runner::eval returns a Result. We are only checking if it doesn't error.
        runner::eval("programs/powerset".to_string()).unwrap();
        assert!(true);
    }

    #[test]
    fn typecheck() {
        // runner::eval returns a Result. We are only checking if it doesn't error.
        runner::eval("programs/typecheck".to_string()).unwrap();
        assert!(true);
    }

    #[test]
    fn treesum() {
        match runner::eval("programs/treesum".to_string()) {
            Ok(interpreter::Value::Int(11)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn mmult() {
        match runner::eval("programs/mmult".to_string()) {
            Ok(interpreter::Value::Int(1884)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn forloop() {
        match runner::eval("programs/forloop".to_string()) {
            Ok(interpreter::Value::Int(2047)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn strings() {
        match runner::eval("programs/strings".to_string()) {
            Ok(interpreter::Value::String(s)) if s == "zyxd".to_string() => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn variadic() {
        match runner::eval("programs/variadic".to_string()) {
            Ok(interpreter::Value::Int(36)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn ret_in_loop() {
        match runner::eval("programs/ret_in_loop".to_string()) {
            Ok(interpreter::Value::Int(50)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn else_if() {
        match runner::eval("programs/else_if".to_string()) {
            Ok(interpreter::Value::Int(25)) => assert!(true),
            _ => assert!(false),
        }
    }
}