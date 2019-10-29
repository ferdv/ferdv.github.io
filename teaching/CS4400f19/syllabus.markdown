---
title: CS 4400/5400 Programming Languages
subtitle: Fall 2019
css: css/foghorn.css
---

## Time and Place

We meet on **Tuesdays, 6-9:15pm** in **[Hurtig Hall](https://www.northeastern.edu/campusmap/map/qad5.html) 129**. The first class is on Tue 9/10.

## People

| Role           | Name             | Email                     | Office hours      | Location  |
|----------------|------------------|---------------------------|-------------------|-----------|
| **Instructor** | Ferdinand Vesely | f.vesely@northeastern.edu | Wednesday 2-6pm   | [Nightingale](https://www.northeastern.edu/campusmap/map/qad5.html) 132A
| **TAs**        | Joshua Hailman   | jhailman@ccs              | Monday 5-7pm      | [WVH](https://www.northeastern.edu/campusmap/map/qad5.html) 102
|                | Vanja Srivastava | srivastava.v@husky        | *TBD*             | *TBD*     |


## Course Description

In CS 4400/5400, we will study concepts underlying programming language constructs, their design and their meaning. We will do this by specifying and implementing small example languages in the functional programming language Haskell, as well as looking at examples from real programming languages.

**Registrar's description for 4400:**
Introduces a systematic approach to understanding the behavior of programming languages. Covers interpreters; static and dynamic scope; environments; binding and assignment; functions and recursion; parameter-passing and method dispatch; objects, classes, inheritance, and polymorphism; type rules and type checking; and concurrency.


**Registrar's description for 5400:**
Studies the basic components of programming languages, specification of syntax and semantics, and description and implementation of programming language features. Discusses examples from a variety of languages.

## Prerequisities

The formal prerequisites of CS4400 are CS3000 (or CS4800) and CS3500. For CS5400 it is CS5010. Knowing at least basics of functional programming will make your life easier, but is not required as we will cover the necessary essentials. You might also find it helpful to brush up on material from CS2500.

## Topics

This is a rough and tentative list of topics that we will aim to cover this semester. It will be adapted into a schedule once the course gets started and as it progresses. I strongly recommend to check the online version of this syllabus frequently to get a better idea what are the next steps in this course.

1. Introduction to Programming Languages. Introducing Haskell
1. Abstract syntax. BNF. Basic interpreters. Big- vs. small-step
1. Names. Binding. Scope. 
1. Lambda calculus. Substitution. Reduction
1. Functions. Environments. Closures. De Bruijn indices
1. Recursion. Fixpoint combinators. The Y combinator
1. Functions as values. Higher-order functions
1. Strict vs. lazy evaluation. Call by name, call by value, call by need
1. Assignment. Stores. Loops
1. Wear formal today: "Pen and paper semantics". Operational. Denotational. Inductive definitions
1. Types & type systems. Type safety. Type inference
1. Data Types. Product. Sum. Pattern Matching
1. Monads. Monadic programming. Type classes
1. Classes and objects. Dynamic dispatch
1. Continuations. Exceptions. Coroutines
1. Concurrency. Process algebras. Bisimulation
1. Logic programming
1. Advanced type systems. Dependent types. Curry-Howard correspondence

## Schedule

| Date  | Topic                              | Materials                  |
|-------|------------------------------------|----------------------------|
| 09/10 | Overview & Quick Intro to Haskell  | [Slides](m/lec01/slides.pdf)\  \ [Lec01.hs](m/lec01/Lec01.hs) |
| 09/17 | Overview of Interpreters; Abstract Syntax; Arithmetic Expressions | [Lec02.hs](m/lec02/Lec02.hs) |
| 09/24 | Evaluating with Environments; More than One Value Type | [Slides](m/lec03/03-slides.pdf)\ \ [Lec03.hs](m/lec03/Lec03.hs)\ \ [Lec03a.hs](m/lec03/Lec03a.hs) |
| 10/01 | Intro to Lambda Calculus, Reduction, Reduction Strategies | [Notes](m/notes/notes.html#lambda-calculus)\ \ [Lec04.hs](m/lec04/Lec04.hs) |
| 10/08 | Programming in Pure Lambda Calculus | [Notes](m/notes/notes.html#programming-in-pure-lambda-calculus) |
| 10/15 | Reduction vs. Evaluation, Closures, Loops and Assignment | [Notes](m/notes/notes.html#reduction-vs.-evaluation) |
| 10/22 | Haskell Corner: Programming with Monads; QuickCheck | |

## Assignments

| Assignment            | Date Set | Date Due |                                  |
|-----------------------|----------|----------|----------------------------------|
| [Assignment 1: Haskell practice](cw/01/assignment01.html) |  09/23   |  09/27   | [PDF](m/cw/01/assignment01.pdf) |
| [Assignment 2: ABL](cw/02/Assignment2.html)| Week of 09/30 | 10/17 | [PDF](m/cw/02/Assignment2.pdf)\ \ [Pack](m/cw/02/Assignment2.zip)
| [Assignment 3: Pure Lambda Calculus](m/cw/03/Assignment3.pdf) | 10/20 | 11/01 | [Pack](m/cw/03/Assignment3.zip)
| Assignment 4 | Week of 11/04 | 11/20 |
| Assignment 5 | Week of 11/18 | 12/04 |

## Haskell

We use [Haskell](https://www.haskell.org/) as a language for implementing our example interpreters. Haskell is a programming language that is *statically typed*, *purely functional*, and *lazy*.[^haskellwords] I will assume that a majority of you does not have a previous experience with it. Thus, this course will contain an introduction to programming in Haskell and you can expect to pick up some techniques specific to functional programming along the way. Apart from being an implementation language for our interpreters, Haskell is an interesting language to study, so we will use it to exemplify some languages features.

The main implementation of Haskell is GHC: the Glasgow Haskell Compiler. It comes with a compiler, `ghc`, as well as an interactive REPL,  `ghci`. 

<!-- - Why functional languages? Brief history of Haskell. -->


**Installation.** There are several ways to install Haskell. Generally, the easiest way across major operating systems is to install the [Haskell Platform](https://www.haskell.org/platform/). Its website contains installation instructions for Windows, macOS, and various Linux distributions. 

**IDEs.** My personal preference is to use a combination of Vim (or another text editor) and running the compiler or REPL in a terminal. However, there are plugin-based alternatives, e.g.,
 
- [IntelliJ IDEA](https://www.jetbrains.com/idea/) with the [IntelliJ-Haskell plugin](https://plugins.jetbrains.com/plugin/8258-intellij-haskell), or
- [Atom](https://atom.io/) with the [ide-haskell plugin](https://atom.io/packages/ide-haskell) and other plugins (such as ide-haskell-repl).

The Atom option has currently some issues due to a deprecated dependency (ghc-mod) -- it will complain, but seems usable.
Of course, there is also a Haskell mode for Emacs users.

**Examples.** To give you a quick taste of what programming in Haskell looks like, here are a few basic versions of factorial:

```haskell
-- This is a one-line comment

{- This is
   a comment
   spread across
   multiple lines -}

-- Variant 1: pattern matching
-- type signatures are (mostly) optional in Haskell, but not in this course :)
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- Variant 2: conditional
fact' :: Int -> Int
fact' n = if n <= 1 then 1 else n * fact' (n - 1)

-- Variant 3: guards
fact'' :: Int -> Int
fact'' n | n <= 1    = 1
         | otherwise = n * fact'' (n - 1)
```

Here is a simple "Hello World":

```haskell
main :: IO () -- think of IO () as a type of computations with side-effects
main = do
  putStrLn "Hello World!"
```


And here is an interactive "Hello World":
```haskell
main :: IO ()
main = do -- do-blocks allow us to perform sequential computations
  putStrLn "What is your name?"
  name <- getLine
  let message = "Hello, "
  putStrLn (message ++ name)
```

<!-- Haskell vs. Java? -->



[^haskellwords]: Do not despair if these words currently mean nothing to you in this context. We will make sense of them during the course.

## Resources

- no required reading
- will provide lecture notes / annotated slides
- (potentially) useful books:
    - Haskell:
        - *Learn You a Haskell for Great Good!* by Miran Lipovaca (available [online](http://learnyouahaskell.com/chapters))
        - *Programming in Haskell* by Graham Hutton
    - *Essentials of Programming Languages* by Daniel P. Friedman and Mitchell Wand -- closest to what we will be doing, but with examples in Scheme
    - *Types and Programming Languages* by Benjamin C. Pierce -- oriented towards type systems
    - *Programming Languages: Application and Interpretation* by Shriram Krishnamurthi (available [online](http://cs.brown.edu/courses/cs173/2012/book/book.pdf))



## Forum

The primary forum for this class is on Piazza, which you can use to ask questions and exchange wisdom while completing assignments. I will also use Piazza to broadcast announcements to the class, so you will be expected to check it at least every few days.
Participate actively and use it as a first place to post questions related to assignments, programming, debugging, exams, etc. Please use the forum to post questions and answers that may be useful to
others. Bottom line: unless you have a private problem, post to Piazza before writing me/the TA an email.

Please register for Piazza by going to [http://piazza.com/northeastern/fall2019/cs44005400](http://piazza.com/northeastern/fall2019/cs44005400)
and registering as a student.


<!-- The class forum is on Piazza. Why Piazza? Because they have a nice web interface, as well as iPhone and Android apps. Piazza is the best place to ask questions about projects, programming, debugging issues, exams, etc. To keep things organized, please tag all posts with the appropriate hashtags, e.g. #lecture1, #project3, etc. I will also use Piazza to broadcast announcements to the class. Bottom line: unless you have a private problem, post to Piazza before writing me/the TA an email. -->

## Exams
There will be one midterm and one final. All exams will be closed book and closed notes, and
computers are not allowed nor is any access to the Internet via any device. The exams will cover
material from lectures, readings, and the projects. They will cover the material discussed during
the first and second halves of the class, respectively (i.e., they are not cumulative).

## Grades

Your final grade will be based on assignments, exams, and participation, with a rough split of 60%/35%/5%. The exact details are TBD.


## Late Policy
For the homework assignments, we will use flexible slip dates. Each student is given an automatic extension of 4 calendar days for the semester. You can use the extension on any assignment
during the semester in increments of a day[^lateday]. For instance, you can hand in one assignment 4 days late,
or one assignment 2 days late and two assignments 1 day late. This should let you schedule due dates around the due
dates for other courses. After you have used up your slip time, any assignment handed in late will be
marked off 20% per day. Assignments more than 2 days late (beyond the use of slip days) will not be
accepted. Extensions will not be granted.

[^lateday]: A “day” refers to 24 hours. Thus, a project turned in 28 hours late will count as two days late.

## Cheating Policy
It's ok to ask your peers about the concepts, algorithms, or approaches needed to do the assignments. We encourage you to do so; both giving and taking advice will help you to learn. However, what you turn in must be your own, or for projects, your group's own work. Looking at or copying code or homework solutions from other people or the Web is strictly prohibited. In particular, looking at other solutions (e.g., from other groups or students who previously took the course) is a direct violation. Projects must be entirely the work of the students turning them in, i.e. you and your group members. If you have any questions about using a particular resource, ask the course staff or post a question to the class forum.

All students are subject to the Northeastern University's [Academic Integrity Policy](http://www.northeastern.edu/osccr/academic-integrity-policy/). Per Khoury College policy, all cases of suspected plagiarism or other academic dishonesty must be referred to the Office of Student Conduct and Conflict Resolution (OSCCR). This may result is deferred suspension, suspension, or expulsion from the university.

## Accommodations for Students with Disabilities
If you have a disability-related need for reasonable academic accommodations in this course and have not yet met with a Disability Specialist, please visit www.northeastern.edu/drc and follow the outlined procedure to request services. If the Disability Resource Center has formally approved you for an academic accommodation in this class, please present the instructor with your "Professor Notification Letter" at your earliest convenience, so that we can address your specific needs as early as possible.

<!-- ## Title IX
Title IX makes it clear that violence and harassment based on sex and gender are Civil Rights offenses subject to the same kinds of accountability and the same kinds of support applied to offenses against other protected categories such as race, national origin, etc. If you or someone you know has been harassed or assaulted, you can find the appropriate resources [here]().
-->

