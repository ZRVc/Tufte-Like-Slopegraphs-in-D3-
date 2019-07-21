# Tufte-Like-Slopegraphs-in-D3-

The purpose of this project was to use D3.js replicate the first two slopegraphs found here:

https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk

The stacked slopegraph for survival rates was pretty easy to do, but the graph of GDP's was harder, because I had to find a way to place the points as closely as I could to where they should be, while still preserving the slopes and avoiding crashes in the labels. My solution was to set up a constrained optimization problem. I plotted the points at their correct locations (according to the spacing I was using), adjusted the tie so that the minimum distance between any two points was 1, then multiplied everything by 16 so that the new minimum was 16. This gave me a feasible region I could use to fit the points by minimizing the sum of squared distances between their placements and their ideal placements. The R code did all the work and the points were then plotted in D3.js. The graph differed a little from Tufte's, but the slopes were correct (within a tolerance).

I couldn't figure out why Tufte drew this slopegraph the way he did. Then, I got ahold of the printed version. The graph in the book is different from the one on the web. In the book, it looks like he plotted the lines where they should be and adjusted the labels so they wouldn't crash. The endpoints of some of the lines aren't centered on the text, but the slopes look good to me. For the web version, he seems to have centered the endpoints of the lines on the adjusted text, producing inaccurate slopes.

The version from the book next to the unadjusted points:
![alt text](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/TufteBookVersion.jpg)

The current version of the R code is [ROI5.R](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/ROI5.R).  It is nearly to the point where others can easily use it.  Currently, it breaks ties with a separate optimization procedure, designed to give a feasible region that satisfies strict criteria, in order to allow for a variety of constraints later on.  It uses linear constraints to preserve the order of the tied points, but, if the user specifies, one or both columns can be given quadratic constraints so that the optimizer can choose the order.

My code is capable of drawing:

[A) A replica of the original book version](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteCorrect.html)

[B) A replica of the version on Tufte's webpage, with the incorrect slopes](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteIncorrect.html)

[C) A version of B) with correct slopes, which is what the code was originally developed to do](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteNewCorrect.html)

[D) A version of C) using quadratic constraints; notice the different ordering of Canada and Belgium in the first column](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteQuadratic124.html)


### Tufte's Web Version vs My Version C Side-by-Side:

![alt text](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/TuftesVsMineSideBySide.png "Side-by-side")

### Tufte's Web Version vs My Version C Overlay:

![alt text](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/TuftesVsMineOverlay.png "Overlay")

### Survival rates slopegraph:

[Survival Rates](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteGillSansStyle.html)
