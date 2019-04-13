# Tufte-Style-Slopegraphs-in-D3-

The purpose of this project is to use D3.js replicate the first two slopegraphs found here:

https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk

The stacked slopegraph for survival rates, which I call the Gill Sans style, was pretty easy to do, but the code is probably not great because it's the first thing I've ever written in javascript.

The graph of GDPs (Bembo style) was harder, because I had to figure out a way to place the points as closely as I could to where they should be, while still preserving the slopes and avoiding crashes in the labels.  My solution was to set up a constrained optimization problem.  I plotted the points at their correct locations (according to the spacing I was using), adjusted the tie so that the minimum distance between any two points was 1, then multiplied everything by 18 so that the new minimum was 18.  This gave me a feasible region I could use to fit the points by minimizing the sum of squared distances between their placements and their ideal placements.  The R code does all the work and the points are then plotted in D3.js.  The graph differs a little from Tufte's, but the slopes are correct. 

### GDP rates slopegraphs:

[First version](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteBemboStyle.html)

[With a larger slope for better spacing between the points](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteBembo36ptDrop.html)


### Survival rates slopegraph:

[Survival Rates](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteGillSansStyle.html)
