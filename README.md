# HW08

1. (8 points) Write a property-based test suite for your expression language.
   * So far, you should have an evaluator, a simplifier, and a parser for expressions. If you haven't implemented some of these features, skip them. In this assignment, you are expected to try your hand in property-based testing, so you can only test what you already have. 
   * You'll need to come up with a set of properties on your own. You can come up with pretty creative properties, if you think how different functions interact with each other. 
   * Check out the files in the [test/Test/](test/Test) folders: there're some additional examples. 

2. (8 points*) Implement an uber parser combinator for parsing expressions with binary infix, and unary prefix and postfix operators. Implement a parser for your expression language based on this combinator
   * This is an extra task, these 8 points are excluded from the total sum for homework assignments. By doing this task, you can compensate for some other skipped tasks. 
   * Come up with your way to specify the precedence, fixity, and associativity of operators. 
   * You can find hints for this task on Hoogle (parser-combinators package)
   * There is also a partial solution for the task in [Combinators.hs](src/Expr/Combinators.hs). Avoid it for an extra challenge. 
