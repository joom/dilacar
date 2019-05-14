# lokum
Machine translation system from Ottoman Turkish to Modern Turkish.

Initially the final project for COS401 Introduction to Machine Translation with Srivinas Bangalore at Princeton University.

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
