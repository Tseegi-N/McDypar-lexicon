# Expanding Conceptual Lexicon of McDypar
## This is a final team project for CSC 390: Natural Language Understanding

This project extends McDypar, a Lisp-based natural language parser grounded in Roger Schank’s Conceptual Dependency (CD) Theory. McDypar parses English sentences into structured representations of meaning using CD primitives, which model the intent and action behind language, rather than relying on surface-level syntax. Our work focuses on expanding McDypar’s capabilities by adding *new lexicons*, enabling *recursive clause* handling, and integrating with the BABEL system from the MARGIE project to *generate paraphrased* English output.

To compile and parse sentences, load McDypar and call parse function inside **clisp**.
```
(load 'mcdypar)
(parse '(Mary gave John a book))
```
If you'd like to generate paraphrases of your most recent parsing sentence, run the following.
```
(load 'prph)
(load 'surf)
(load 'mcdypar)
(load 'mcdypar_babel_converter)
(express (mcdypar-convert-cd-to-babel-style (get-cd)))
```

## What is McDypar?
McDypar (Micro Dyer's Parser) is a conceptual parser originally developed by Michael Dyer as an improvement over McELI. It processes sentences left to right, spawning demons, independent expectation-based processes, that bind concepts together as meaning unfolds. McDypar stores intermediate representations in a working memory (WM) and builds CD structures that represent the semantic essence of a sentence using primitives like *MTRANS*, *PTRANS*, and *INGEST*. These structures enable knowledge inference and reasoning.

## Added Lexicons
- Mary gave John a book.
- Fred told Mary that John eats lobster
- Fred told Mary a secret.
- John went into a restaurant.
- John gave Mary an aspirin.
- Sarah pushed the chair.
- John pushed the table to the wall from the window.
- Mary said John ate an apple
- John went home
- John bought a book
- The gun shot Mary 

## Discussion
Our contributions include:

- Extending McDypar’s lexicon with new verbs, subjects, prepositions, and templates inspired by examples from Dyer and Schank.

- Implementing recursive parsing to handle sentences with embedded clauses (e.g., “John told Mary that Fred eats lobster”).

- Exploring disambiguation demons to differentiate between sentence contexts, such as distinguishing sentence structure of "told that ___" versus "told a secret."
  
- Integrating McDypar with BABEL, the paraphrasing system from MARGIE, to translate CD structures back into natural English sentences.

This work highlights both the promise and limitations of symbolic parsers in modern NLP. While statistical models dominate current NLP pipelines, McDypar demonstrates that interpretable, rule-based systems still play a crucial role in commonsense reasoning, semantic clarity, and research into explainable AI. If you'd like to explore further, please refer to **csc390-Final.pdf** for detailed insight of our project.

## Acknowledgements
We would like to thank Professor Jamie Macbeth for his invaluable guidance throughout this project, including his insightful suggestions, prior code developed in collaboration with former students, and for providing key scholarly materials related to McDypar and Conceptual Dependency structures.

## Citations
Allen, J. F., Ferguson, G., & Stent, A. (2007). An architecture for more realistic conversational systems. In Intelligent User Interfaces.

Cullingford, R. E. (1978). Script Application: Computer Understanding of Newspaper Stories. Ph.D. Dissertation, Yale University.Schank, R. C. (2014). Conceptual information processing (Vol. 3). Elsevier.

DeJong, G. F. (1982). An Overview of the FRUMP System. In Strategies for Natural Language Processing (pp. 149–176). Lawrence Erlbaum.

Dyer, M. G. (1982). In-depth understanding: A computer model of integrated processing for narrative comprehension. Yale University.

Macbeth, Jamie C. "Enhancing learning with primitive-decomposed cognitive representations." In Proceedings of The First Annual International Workshop on Self-Supervised Learning (IWSSL-2020), Cambridge, MA. 2020.

McDonald, D., & Pustejovsky, J. (1985). Sparser: A better chart parser. In Proceedings of the 23rd Annual Meeting on ACL.

Schank, R. C. (1975). Conceptual Information Processing. North-Holland.

Schank, R. C., & Riesbeck, C. K. (2013). Inside computer understanding: Five programs plus miniatures. Psychology Press.
