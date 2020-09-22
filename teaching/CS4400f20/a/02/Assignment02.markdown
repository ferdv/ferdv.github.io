---
title: Assignment 2
subtitle: CS 4400 Programming Languages
css: /css/foghorn.css
documentclass: scrartcl
papersize: letter
urlcolor: cyan
header-includes:
   - \usepackage{charter}
   - \usepackage{fullpage}
   - \usepackage[scaled=0.85]{beramono}
---

**Due:** Thursday, September 24, 2020 at 9pm

**Submission:** 

1. Submit a single file, `Assignment02.hs` via <https://handins.ccs.neu.edu/courses/119>.

2. At the very top, the file should contain a preamble following this template.

    ```haskell
    {- |
    Module      :  Assignment02
    Description :  Assignment 2 submission for CS 4400.
    Copyright   :  (c) <your name>

    Maintainer  :  <your email>
    -}

    module Assignment02 where

    ... your code goes here ...
    ```

   The rest of the file will contain your solutions to the exercises below. 

3. Every top-level definition[^1] must include a purpose statement (for functions) and a type signature, followed by one or more defining equations.

4. Double-check that you have named everything correctly.

5. Make sure your file loads into GHCi or can be compiled by GHC without any errors.

6. If something is not clear, or you are struggling with some questions, talk to us: in office hours, after class, on Piazza, via email.

7. This assignment is to be completed and submitted individually.

[^1]: A *top-level definition* is one that is not nested inside another one.

**Purpose:** The aim of this assignment is to practice working with BNFs and abstract syntax trees, as well as writing higher-order functions with polymorphic types.

<!--**Grade:** To calculate your grade, we will take the following into account:

a) Does your code compile without errors?
b) Does it follow the above rules?
c) Are functions and constants named as specified? Do they have the correct types?
d) Does your code behave as specified? This will be determined by unit testing. 
e) How readable is your code?
-->


## Examples and Tests

Starting with this assignment, you are expected to write examples and tests for every data type and function you write. For check-expect-style unit tests, you can use the `SimpleTests` module available through the course website. Its use will be demonstrated in class. Alternatively, you can use one of Haskell's popular unit testing frameworks, `HUnit` or `HSpec`. We might demonstrate them later in the semester. For example variables, use the prefix `ex_`{.haskell}. For tests, the prefix `test_`{.haskell}.

# Questions

## BNF and Abstract Syntax

An s-expression is:

a) an atom
b) a list of s-expressions, enclosed in parentheses, separated by spaces

For now, an atom is either a number (integer), or a symbol. Textual examples of s-expressions are:

```scheme
(1 20 x 30 foo)
(+ 13 23)
20
(a b (c d))
x
(32 * (30 z) =)
(/ (- 10 2) 4)
```

1. In a `{- -}`{.haskell} comment, write down the BNF for s-expressions.
   
2. Design a datatype for s-expressions. Name it `SExpr`{.haskell}. Use `String`{.haskell} to represent symbols. Write at least 3 meaningful examples of `SExpr`{.haskell} values.

3. Write a function `showSExpr`{.haskell} which takes an s-expression and ~~prints~~ returns its string representation as above. Use single spaces between elements of an s-expression list. 

4. Recall the SAE language from class: 

    ```haskell
    data SAE = Number Integer            -- <SAE> ::= <Integer>
             | Add SAE SAE               --         | <SAE> + <SAE>
             | Sub SAE SAE               --         | <SAE> - <SAE>
             | Mul SAE SAE               --         | <SAE> * <SAE>
             | Div SAE SAE               --         | <SAE> / <SAE>
             deriving (Eq, Show)
    ```

    SAE expressions can be represented as s-expressions, using prefix notation (like Racket). Here are a few examples:

    ```
    32
    (+ 12 14)
    (- (/ 16 4) (- 5 4))
    ```

    Write two functions:

      (a) a function `fromSExpr` which converts an s-expression (with symbols restricted to `+`, `-`, `*`, and `/`) into an `SAE`{.haskell} expression
      (b) a function `toSExpr` which converts an `SAE`{.haskell} expression into its s-expression representation.
    

    For 4a, you can assume that only valid SAE s-expression in prefix form will be provided.

## Polymorphic higher-order functions

5. A polymorphic binary tree is defined as follows:

    ```haskell
    data BinaryTree a = Empty
                      | Node a (BinaryTree a) (BinaryTree a)
    ```

    Design a function `treeMap`{.haskell} which takes a function and maps it over the given binary tree. In other words, it applies the given function to each element in the tree, preserving its structure.

    Examples:

    ```haskell
    treeMap show (Node 1 (Node 42 Empty (Node 4 Empty Empty)) (Node 3 Empty Empty))
      =  Node "1" (Node "42" Empty (Node "4" Empty Empty)) (Node "3" Empty Empty)
    ```

    ```haskell
    treeMap head (Node [1] (Node [10, 2, -4] Empty Empty) (Node [-4, 12] Empty Empty)) 
      = Node 1 (Node 10 Empty Empty) (Node (-4) Empty Empty)
    ```

6. Design a higher-order function `iterateN`{.haskell}, which takes a function `f`{.haskell}, an `Integer`{.haskell} `n`{.haskell} and an initial value `init`{.haskell}  (of an arbitrary type) and applies `f`{.haskell} `n`-times, starting with `init`{.haskell}. Write the correct polymorphic signature.

    For example, if the following is defined:

      ```{.haskell}
      double :: Integer -> Integer
      double x = x + x
      ```

      the following should hold:

      ```haskell
      iterateN double 5 1 = 32

      iterateN double 0 42 = 42

      iterateN tail 3 ["Alewife", "Davis", "Porter", "Harvard", "Central"] =
        ["Harvard", "Central"]
      ```

<!-- Don't forget purpose statements and signatures! -->

<!--
6. Using the following `BinaryTree` definition, 

    ```haskell
    data BinaryTree = Empty
                    | Node Integer BinaryTree BinaryTree
    ```

implement a function `foldTree`{.haskell} with the following arguments: 

    - a function `combine`{.haskell}, which takes two integers and returns an integer,
    - a initial value `init`{.haskell}, and
    - a binary tree.

   The function should fold (combine) elements of the tree using the supplied function `f`{.haskell}. 

   Examples:

    ```haskell
    foldTree (*) 1 (Node 6 (Node 3 (Node 2) Empty) (Node 1 Empty Empty)) = 
    ```
    

6. Write a function `combineWith`{.haskell} which takes a function `f`{.haskell} and two lists, and combines the two lists element-wise.

   Examples:

     ```haskell
     combineWith (+) [1, 2, 3] [4, 5, 6]
     ```
-->

<!--
5. Write the function `iterateUntil` which takes a function `f`{.haskell}, a function `p`{.haskell} and a value `init`{.haskell} and, starting with `init`{.haskell}, keeps applying `f`{.haskell}, until `p`{.haskell} applied to the result returns `True`.{haskell}. For example, with the following definitions,

      ```{.haskell}
      double :: Integer -> Integer
      double x = x * x 

      greaterThan50 :: Integer -> Bool
      greaterThan50 x = x > 50

      isEmpty :: [a] -> Bool
      isEmpty [] = True
      isEmpty (_ : _) = False
      ```
      
      the following should hold

      ```haskell
      iterateUntil double greaterThan50 1 = 
      ```

      should return 
-->
