---
title: CS4400 Programming Languages
subtitle: Fall 2020
author: Ferdinand Vesely
css: css/foghorn.css
---

Parts of this syllabus are subject to change as the semester progresses.

## Time and Place

We meet on Tuesdays and Fridays, 9:50-11:30am in Mugar 201 and on Zoom. Zoom links for each lecture are available on Canvas. The first class is on Friday, 9/11.

## People

| Role           | Name               | Email                           | Office hours      | Location   |
|----------------|--------------------|---------------------------------|-------------------|------------|
| **Instructor** | Ferdinand Vesely   | f.vesely@northeastern           | Tue/Thu 3-4:30pm  | See Piazza |
| **TAs**        | Jack Gelinas       | gelinas.j@northeastern          | Wed, 1:45-4:30pm  | [Zoom](https://northeastern.zoom.us/j/97516065212) |
|                | Divya Venkatesh    | payyadivenkatesh.d@northeastern | Mon/Thu 12:30-2pm | [Zoom](https://northeastern.zoom.us/j/91801070558?pwd=NzRiVkVhVEI4MERXQmZsM1RXNlN4UT09) |

Note: We use waiting rooms in Zoom to deal with queues.

## Course Description

The overarching idea of (this version of) this course is to study constructs and fundamental concepts of programming languages, while getting familiar and exploring topics in functional programming using the language Haskell. We will be discussing concepts both formally and informally, as well as implement interpreters and other language-related tools in Haskell. These implementations will motivate exploring various approaches, idioms and concepts in functional programming.


**Registrar's description for 4400:**
Introduces a systematic approach to understanding the behavior of programming languages. Covers interpreters; static and dynamic scope; environments; binding and assignment; functions and recursion; parameter-passing and method dispatch; objects, classes, inheritance, and polymorphism; type rules and type checking; and concurrency.



## Prerequisities

The formal prerequisites of CS4400 are CS3000 (or CS4800) and CS3500. Knowing at least basics of functional programming will make your life easier, but is not required as we will cover the necessary essentials. You might also find it helpful to brush up on material from CS2500. Some basics of logic or discrete math might come in handy too.

## Topics

This is a rough and tentative list of topics that we will aim to cover this semester. It will be adapted into a schedule once the course gets started and as it progresses. I strongly recommend to check the online version of this syllabus frequently to get a better idea what are the next steps in this course. This list currently concentrates on aspects of programming languages, but topics covered will include useful functional programming concepts supported by Haskell.

1. Intro to Programming Languages. Haskell basics
1. Abstract syntax. BNF. Parsing
1. Basic interpreters. Arithmetic
1. Names and bindings. Substitution
1. Environments. Basic procedures and functions
1. Functions as values. Lambda calculus. Reduction
1. Recursion. Fixpoint combinators. The Y combinator
1. Strict vs. lazy evaluation. Call by name, call by value, call by need
1. Imperative programming. Assignment. Stores. Loops
<!-- 1. Wear formal today: "Pen and paper semantics". Operational. Denotational. Inductive definitions -->
1. Types & type systems. Simply-Typed Lambda Calculus
1. Polymorphism. Type inference
<!-- 1. Data Types. Product. Sum. Pattern Matching -->
<!-- 1. Monads. Monadic programming. Type classes -->
1. Classes and objects. Dynamic dispatch
1. Continuations. Exceptions. Coroutines
1. Non-determinism. Logic programming
1. Reasoning about programs
1. Concurrency. Process algebras. Bisimulation
1. Advanced type systems. Dependent types. Curry-Howard correspondence

## Schedule

| Date  | Topic                                   | Materials |
|-------|-----------------------------------------|-----------|
| 09/11 | Overview & Quick Intro to Haskell       | [Slides](l/01/slides.pdf) \ \ [Lec01.hs](l/01/Lec01.hs) \ \ [Wat (video)](https://archive.org/details/wat_destroyallsoftware) |
| 09/15 | Abstract Syntax & (Algebraic) Datatypes | [Notes](l/02/02.pdf) \ \ [Lec02.hs](l/02/Lec02.hs)|
| 09/18 | Intro to interpreters, evaluating SAE, polymorphic types | [Notes](l/03/03.pdf) \ \  [Lec03.hs](l/03/Lec03.hs)|
| 09/22 | JSON example & Haskell corner | [Lec04.hs](l/04/Lec04.hs) \ \ [Foo.hs](l/04/Foo.hs) |
| 09/25 | Bindings, substitution, scope & Maybe | [Lec05.hs](l/05/Lec05.hs) |
| 09/29 | Equational reasoning, maps & environments | [Notes](l/06/06.pdf) \ \ [Lec06.hs](l/06/Lec06.hs) |
| 10/02 | Procedural abstraction & function definitions | [Lec07b.hs](l/07/Lec07b.hs) \ \ [Maps.hs](l/07/Maps.hs) |
| 10/06 | Functions as values; Abstracting 'case ... of' | [Lec08a.hs](l/08/Lec08a.hs) |
| 10/09 | More abstracting and monads | [Notes](l/09/09.pdf)
| 10/13, 16, 20 | Lambda Calculus| [Notes](l/10/10.pdf)
| 10/23, 27 | Programming in Pure Lambda Calculus| [Notes](l/13/13.pdf)
| 10/30 | Lambda w/ Extensions; Recursive evaluators for Lambda | Extensions:&nbsp;[PDF](l/15/LambdaExtensions.pdf)&nbsp;[Source](l/15/LambdaExtensions.lhs)  Evaluators:&nbsp;[PDF](l/15/LambdaEvaluators.pdf)&nbsp;[Source](l/15/LambdaEvaluators.lhs) 
| 11/03 | Haskell corner: Functors \& Applicatives | |
| 11/06 | The Result monad; Primitive operations | [Ex1.hs](l/17/Ex1.hs) (messy) |
| 11/10 | Haskell's laziness = easy recursion; de Bruijn representation | |
| 11/13 | De Bruijn; Intro to Type Systems | [Ex1.hs](l/19/Ex1.hs) [Notes](l/19/19.pdf) |
| 11/17 | Specifying type systems; Basic type checking; STLC | [Ex1.hs](l/20/Ex1.hs) [Notes](l/20/20.pdf) |
| 11/20 | STLC; Parametric polymorphism | [Ex1.hs](l/21/Ex1.hs) [Notes](l/21/21.pdf) |
| 11/24 | Parametric polymorphism / System F; Type inference | [Ex1.hs](l/22/Ex1.hs) [Notes](l/22/22.pdf) |


## Assignments

You can expect homework assignments to be handed out roughly every week and there will be around 10 assignments in total this semester. Some of them will be individual work, for others you will be expected to work in pairs. They will be mostly programming assignments. Tentatively, assignments will be handed out on Fridays and be due on the following Thursday. Submission will be via 

| Assignment                             | Date Set | Date Due |                                  |
|----------------------------------------|----------|----------|----------------------------------|
| [Assignment 1](a/01/Assignment01.html) | 9/15     | 9/19     | [PDF](a/01/Assignment01.pdf)   |
| [Assignment 2](a/02/Assignment02.html) | 9/20     | 9/24     | [PDF](a/02/Assignment02.pdf) \ \ [SimpleTests.hs](a/02/SimpleTests.hs) |
| [Assignment 3](a/03/Assignment03.html) | 9/25     | 10/1     | [PDF](a/03/Assignment03.pdf) \ \ [Pack](a/03/Assignment03.zip) |
| [Assignment 4](a/04/Assignment04.html) | 10/2     | 10/8     | [PDF](a/04/Assignment04.pdf) \ \ [Pack](a/04/Assignment04.zip) |
| [Assignment 5](a/05/Assignment05.html) | 10/9     | 10/15    | [PDF](a/05/Assignment05.pdf) \ \ [Pack](a/05/Assignment05.zip) |
| [Assignment 6](a/06/Assignment06.html) | 10/18    | 10/24    | [PDF](a/06/Assignment06.pdf) \ \ [Pack](a/06/Assignment06.zip) |
| [Assignment 7](a/07/Assignment07.html) | 10/26    | 11/4     | [PDF](a/07/Assignment07.pdf) \ \ [Pack](a/07/Assignment07.zip) |
| [Assignment 8](a/08/Assignment08.html) | 11/13    | 11/20    | [PDF](a/08/Assignment08.pdf) \ \ [Pack](a/08/Assignment08.zip) |
| [Assignment 9](a/09/Assignment09.html) | 11/27    | 12/5     | [PDF](a/09/Assignment09.pdf) \ \ [Pack](a/09/Assignment09.zip) |
| [Assignment 10]{.deleted}     | [12/3]{.deleted}  | [12/11]{.deleted} | |

## Haskell

We use [Haskell](https://www.haskell.org/) both as a topic of study and for implementing our examples. Haskell is a programming language that is *statically typed*, *purely functional*, and *lazy*.[^haskellwords] I expect that a majority of you *does not* have previous experience with it. Thus, this course will contain an introduction to programming in Haskell and you can expect to pick up some techniques specific to functional programming along the way. Apart from being an implementation language for our interpreters, Haskell is an interesting language to study, so we will use it to exemplify some languages features.

The main implementation of Haskell is GHC: the Glasgow Haskell Compiler. It comes with a compiler, `ghc`, as well as an interactive REPL,  `ghci`. 

<!-- - Why functional languages? Brief history of Haskell. -->

**Haskell in a browser.** The quickest way to start using Haskell is to simply run it in your browser. There is a handful of online IDEs, for example <https://repl.it/languages/haskell>. Note, that when you click *Run*, it will throw an error at you about a variable `main` not being in scope. You can ignore that error for now -- it still loads your definitions. If you want to avoid that error, instead of clicking *Run*, type `:l main.hs` in the interactive area and press enter. Then you can reload the same file after you edit it by typing `:r`.

**Installation.** There are several ways to install Haskell. Generally, the easiest way across major operating systems is to install the [Haskell Platform](https://www.haskell.org/platform/). Its website contains installation instructions for Windows, macOS, and various Linux distributions. 

**IDEs.** My personal preference is to use a combination of Vim (currently mostly NeoVim) and running the compiler, REPL, or other tools in a terminal. However, there are plugin-based alternatives, e.g.,

* [Visual Studio Code](https://code.visualstudio.com/) -- based on my limited experience with it, VSCode has/had decent Haskell support and it seems fun to use, 
- [IntelliJ IDEA](https://www.jetbrains.com/idea/) with the [IntelliJ-Haskell plugin](https://plugins.jetbrains.com/plugin/8258-intellij-haskell), or
- [Atom](https://atom.io/) with the [ide-haskell plugin](https://atom.io/packages/ide-haskell) and other plugins (such as ide-haskell-repl).

Last time I checked, the Atom option had issues due to a deprecated dependency (ghc-mod) -- it complained, but seemed usable.

If you are an Emacs user, you're out of luck... Just kidding, there is, of course a [Haskell mode](https://github.com/haskell/haskell-mode/) and a whole lot of other goodies.

There's also [spacemacs](https://www.spacemacs.org/).

See also <https://wiki.haskell.org/IDEs>.

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

In general, there is no required reading for this course and I may provide lecture notes or annotated slides where applicable. However, there are other sources that we might use to replace or supplement those. I will draw your attention to them where appropriate.

### Books
Check Northeastern's library. A good portion of these books is available to us online. Some of these items have abbreviations -- I will use these when referring to supplementary reading.

- Haskell:
   - *Programming in Haskell* by Graham Hutton
   - *Get Programming with Haskell* by Will Kurt seems pretty good
   - [*Haskell Programming from first principles*](https://haskellbook.com/) by Christopher Allen and Julie Moronuki is new and seems interesting
   - *Learn You a Haskell for Great Good!* by Miran Lipovaca (available [online](http://learnyouahaskell.com/chapters)) -- I liked this one when I read it, but it is somewhat problematic, so watch out.

- Programming lanugages:
   - **[EOPL]** *Essentials of Programming Languages* by Daniel P. Friedman and Mitchell Wand (some editions with Christopher T. Haynes) -- closest to what we will be doing, but with examples written in Scheme
   - **[TAPL]** *Types and Programming Languages* by Benjamin C. Pierce -- THE textbook on types and type systems. It has a good chapter on Lambda Calculus and other chapters will be relevant when we look at type systems.
   - *Programming Languages: Application and Interpretation* by Shriram Krishnamurthi (available [online](http://cs.brown.edu/courses/cs173/2012/book/book.pdf))

### Other

I will keep extending this list.

- [Try Haskell!](https://tryhaskell.org) -- a nice online tutorial to get you started with Haskell
- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/) -- excellent comprehensive resource about various topics pertaining to Haskell and functional programming 
- [Hackage: The Haskell Package Repository](https://hackage.haskell.org/)
- [Eli's version of this course](https://pl.barzilay.org/) -- Eli Barzilay's excellent version of CS4400 and a source of inspiration for the topics in this version

## Forum

The primary forum for this class is on Piazza, which you can use to ask questions and exchange wisdom while completing assignments. I will also use Piazza to broadcast announcements to the class, so you will be expected to check it at least every few days.
Participate actively and use it as a first place to post questions related to assignments, programming, debugging, exams, etc. Please use the forum to post questions and answers that may be useful to
others. Bottom line: unless you have a private problem, post to Piazza before writing me/the TA an email.

Please register on Piazza by going to [https://piazza.com/northeastern/fall2020/cs4400](https://piazza.com/northeastern/fall2020/cs4400). You will need an access code which will be provided in class or via email.

Note: The Piazza forum is meant for discussion related to the topics of this course and to exchange knowledge. While I welcome debates about the course structure, the whys and why nots, or other general topics, I reserve the right to change the visibility of posts if I think they do not contribute to the learning aims of this course. You are most welcome to talk to me about any topic directly -- after class, in office hours, or in an ad-hoc/scheduled meeting.



<!-- The class forum is on Piazza. Why Piazza? Because they have a nice web interface, as well as iPhone and Android apps. Piazza is the best place to ask questions about projects, programming, debugging issues, exams, etc. To keep things organized, please tag all posts with the appropriate hashtags, e.g. #lecture1, #project3, etc. I will also use Piazza to broadcast announcements to the class. Bottom line: unless you have a private problem, post to Piazza before writing me/the TA an email. -->

<!-- ## Exams
There will be one midterm and one final. All exams will be closed book and closed notes, and
computers are not allowed nor is any access to the Internet via any device. The exams will cover
material from lectures, readings, and the projects. 
-->

<!-- They will cover the material discussed during
the first and second halves of the class, respectively (i.e., they are not cumulative). -->

## Grades

This is a tentative breakdown of how the final grade will be determined:

* 60% assignments 
* 35% other (in-class quizzes)
* 5% karma

The mapping of percentages to letter grades is as follows:

<table class="centered" cellspacing="0" cellpadding="0"><tbody><tr><td><p><span style="font-weight: bold">Cutoff</span></p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>93%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>90%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>86%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>83%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>80%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>76%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>73%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>70%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>66%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>63%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>60%</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>else</p></td></tr><tr><td><p><span style="font-weight: bold">Letter grade</span></p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>A</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>A-</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>B+</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>B</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>B-</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>C+</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>C</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>C-</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>D+</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>D</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>D-</p></td><td><p><span class="hspace">&nbsp;&nbsp;</span></p></td><td><p>F</p></td></tr></tbody></table>


## Late Policy

You are allowed to turn your work in after the deadline, with a penalty of 4% of your grade per hour. For example, submitting 10 minutes late reduces your grade by 4%, submitting 5 hours and 1 minute late reduces your grade by 24%. Submitting more than 25 hours late will result in a zero. This will be automatically determined by the Handins server. You can submit as many times as you wish. We thus recommend submitting even a partial solution well before the deadline. That way if something goes wrong, you'll get at least partial credit (and feedback).

<!--
We will allow  to turn in your work after the deadline, at a 4% per hour penalty starting immediately at the deadline (i.e., submitting 60-minutes-and-one-second late will be graded as two hours late). The handin server will use its own clock to determine what time it is, so it is a bad idea to try to sneak in a submission in those last few seconds. Submitting more than 25 hours late will result in a zero; however, we will accept homeworks up to two days late so that you can still receive feedback on your work.
-->

<!--For the homework assignments, we will use flexible slip dates. Each student is given an automatic extension of 4 calendar days for the semester. You can use the extension on any assignment
during the semester in increments of a day[^lateday]. For instance, you can hand in one assignment 4 days late,
or one assignment 2 days late and two assignments 1 day late. This should let you schedule due dates around the due
dates for other courses. After you have used up your slip time, any assignment handed in late will be
marked off 20% per day. Assignments more than 2 days late (beyond the use of slip days) will not be
accepted. Extensions will not be granted.

[^lateday]: A “day” refers to 24 hours. Thus, a project turned in 28 hours late will count as two days late.
-->

## Cheating Policy
It's ok to ask your peers about the concepts, algorithms, or approaches needed to do the assignments. We encourage you to do so; both giving and taking advice will help you to learn. However, what you turn in must be your own, or for projects, your group's own work. Looking at or copying code or homework solutions from other people or the Web is strictly prohibited. In particular, looking at other solutions (e.g., from other groups or students who previously took the course) is a direct violation. Projects must be entirely the work of the students turning them in, i.e. you and your group members. If you have any questions about using a particular resource, ask the course staff or post a question to the class forum.

All students are subject to the Northeastern University's [Academic Integrity Policy](http://www.northeastern.edu/osccr/academic-integrity-policy/). Per Khoury College policy, all cases of suspected plagiarism or other academic dishonesty must be referred to the Office of Student Conduct and Conflict Resolution (OSCCR). This may result is deferred suspension, suspension, or expulsion from the university.

## Accommodations for Students with Disabilities
If you have a disability-related need for reasonable academic accommodations in this course and have not yet met with a Disability Specialist, please visit www.northeastern.edu/drc and follow the outlined procedure to request services. If the Disability Resource Center has formally approved you for an academic accommodation in this class, please present the instructor with your "Professor Notification Letter" at your earliest convenience, so that we can address your specific needs as early as possible.

<!-- ## Title IX
Title IX makes it clear that violence and harassment based on sex and gender are Civil Rights offenses subject to the same kinds of accountability and the same kinds of support applied to offenses against other protected categories such as race, national origin, etc. If you or someone you know has been harassed or assaulted, you can find the appropriate resources [here]().
-->

