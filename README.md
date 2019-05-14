# dilacar
Machine translation system from Ottoman Turkish to Modern Turkish.

Initially the final project for COS401 Introduction to Machine Translation with Srinivas Bangalore at Princeton University. The report for the project can be found [here](http://www.cs.princeton.edu/~ckorkut/papers/ottoman.pdf).

Named after Turkish-Armenian linguist Agop Dilâçar (born Martayan), who is known for his work on the Turkish language and who was given the last name Dilâçar (literally meaning "language opener") by Mustafa Kemal Atatürk.

## Installation

I'm currently having a problem building the executable on my machine, but running the REPL works:

```
stack ghci
```

And then you can run the program as such:

```
λ> :set +s
λ> run "ياپراقلر باخچه\8204لردن دوشمش."
yapraklar
bahçelerden
düşmüş, duşmuş, dövüşmüş, döşmüş
.
(2.61 secs, 15,223,379,696 bytes)
```

## Accuracy

This is still work in progress. Many, many of the suffixes in Turkish language are missing from the program, but the mechanism to add them is there. Turkish has a very high number of suffixes, especially derivative ones. With more time put into the project, its accuracy should go up.
