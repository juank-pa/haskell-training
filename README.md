# Haskell Programming from first principles: Exercises solutions

**Note:** This is still a work in progress.

I was studying [Haskell](https://www.haskell.org/) for a month, and then began reading the Christopher Allen and Julie Moronuki's, [Haskell Programming from first principles](http://haskellbook.com/) book. At that point I started a repository with the answers for all Intermission and Chapter exercises. At first this served only one purpose: to self-document my progress throughout the book. But later, I thought it would be good idea to share what I've learned and to create an access point to let other developers learning Haskell compare their solutions, share ideas on better ones, and learn more about Haskell in the process.

To improve documentation I started adding not only the exercises, but the code samples found throughout the book. I updated my responses, to not be as plain as "1. a)" but to also answer using complete sentences, and in many cases explain the why behind it. I really wanted people to understand the exercises even if they don't have the book in front of them.

The first exercises solutions were very simple and straightforward. But after some chapters, you will notice there is open space for many different implementations for just a single question.

In some cases, I do not only implemented what the book requested, the way it requested it, but also played with other options, implemented helper functions which, themselves, add to the overall knowledge, and peek a bit on other more advanced topics. Sometimes I make personal comments on whether an exercise has an open-ended response or not, from my personal point of view, and give different implementations depending on different exercise interpretations.

To prevent overwhelming beginners, I prefered not to import internal or external libraries excessively. I give explanatory comments everytime I use a library or a concept that, by that point in the book, you're are not supposed to have seen it yet. From time to time, I let you peek into more advance stuff just to keep you interested in what's coming up next, but don't worry if you don't understand the solution at that point.

I want to emphasize I'm a beginner too and in now way these exercises express a definitive solution, or even best practices at all. You are welcome to give ideas to improve the repo and exercises contained in it. You can fork the repo and practice on your own, open issues on this repo or create a pull request.

## Solution Conventions

There are some personal conventions I used while creating solutions:

* Exercises are grouped in folders by chapters.
* Each chapter contains a `Content` folder with the sample code used in that chapter. Sometimes function names overlap, and some other times the book creates new modules explicitly. In those cases, I create individual modules for samples. Otherwise I keep every other sample code in a `Common.hs` file inside the `Content` folder.
* Exercises are placed in an `Exercises` folder. Intermission exercises placed inside the `Intermission.hs` file and final chapter exercises placed in the `Exercises.hs` file.
* If the exercise requires a separate module, it is placed in their own file inside the `Exercises` folder, and being referenced from the corresponding `Intermission,hs` or `Exercises.hs` file.
* Every `.hs` has a qualified module name reflecting its file path.
* I use [hlint](https://hackage.haskell.org/package/hlint) to aid me in writing better code. But sometimes, at least at initial stages, samples use code that could be improved. I use code like this:`{-# ANN module "HLint: ignore <custom message>" #-}` to remove hlint warnings, and to express improvements for the following code more clearly.
* I like to use the `:set -Wall` option in `GHCi` when loading each file, and thus, to prevent warnings I added the necessary compiler flags on top of the required flags to remove those warnings, e.g `{-# OPTIONS_GHC <flag> #-}`.
