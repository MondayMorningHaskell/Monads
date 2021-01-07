# Monads (and other Functional Structures)

This repository is meant to accompany the [Monads Series](https://www.mmhaskell.com/monads) on the Monday Morning Haskell blog. Each article in the series has one of two corresponding modules in this repository. Any module with "Complete" in its name is meant mostly as a reference. You can read the code along with the article and see the finished product. The other modules (for example [Functors](https://github.com/MondayMorningHaskell/Monads/blob/master/src/Functors.hs) and [MonadsBasic](https://github.com/MondayMorningHaskell/Monads/blob/master/src/MonadsBasic.hs)) also follow the same code examples, but some parts are left `undefined` with a "TODO" marker, so you can write the code in for yourself!

## Repository Instructions

Start by building the code:

```
stack build
```

Then, you can open up the exercise in one window and bring up GHCI in another to follow along. There's some overlap in variable names, so you should start your GHCI session by clearing the module list:

```
>> stack ghci
>> :l
```

Then you can load the module you're working with, and try out the different expressions!

```
>> :l ReaderWriter
>> acc1 "Hello"
(2, "lloHH")
>> acc1'
(undefined)
```

The `acc1'` function is undefined. So you can then modify it and use `:r` to reload the module!

```
>> :r
>> acc1' "Hello"
("lloHH", 2)
```

## Full Module List

1. [Part 1](https://mmhaskell.com/monads/functors): [Functors](https://github.com/MondayMorningHaskell/Monads/blob/master/src/Functors.hs), [FunctorsComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/FunctorsComplete.hs)
2. [Part 2](https://mmhaskell.com/monads/applicatives): [ApplicativesComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/ApplicativesComplete.hs) (All coding can be done in GHCI)
3. [Part 3](https://mmhaskell.com/monads/tutorial): [MonadsBasic](https://github.com/MondayMorningHaskell/Monads/blob/master/src/MonadsBasic.hs), [MonadsBasicComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/MonadsBasicComplete.hs)
4. [Part 4](https://mmhaskell.com/monads/reader-writer): [ReaderWriter](https://github.com/MondayMorningHaskell/Monads/blob/master/src/ReaderWriter.hs), [ReaderWriterComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/ReaderWriterComplete.hs)
5. [Part 5](https://mmhaskell.com/monads/state): [State](https://github.com/MondayMorningHaskell/Monads/blob/master/src/State.hs), [StateComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/StateComplete.hs)
6. [Part 6](https://mmhaskell.com/monads/transformers): [Transformers](https://github.com/MondayMorningHaskell/Monads/blob/master/src/Transformers.hs), [TransformersComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/TransformersComplete.hs)
7. [Part 7](https://mmhaskell.com/monads/laws): [LawsComplete](https://github.com/MondayMorningHaskell/Monads/blob/master/src/LawsComplete.hs) (One small coding addition you can make)
