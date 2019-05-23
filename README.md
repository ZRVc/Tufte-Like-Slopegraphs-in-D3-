# Tufte-Style-Slopegraphs-in-D3-

The purpose of this project was to use D3.js replicate the first two slopegraphs found here:

https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk

The stacked slopegraph for survival rates was pretty easy to do, but the graph of GDP's was harder, because I had to figure out a way to place the points as closely as I could to where they should be, while still preserving the slopes and avoiding crashes in the labels.  My solution was to set up a constrained optimization problem.  I plotted the points at their correct locations (according to the spacing I was using), adjusted the tie so that the minimum distance between any two points was 1, then multiplied everything by 18 so that the new minimum was 18.  This gave me a feasible region I could use to fit the points by minimizing the sum of squared distances between their placements and their ideal placements.  The R code did all the work and the points were then plotted in D3.js.  The graph differed a little from Tufte's, but the slopes were correct. 

I was convinced that Tufte's graph had incorrect slopes.  Then, I looked at the printed version and it became clear what had happened.  The version in the book is different from the one on the web.  In the book, it looks like he plotted the lines where they should be and adjusted the labels so that they wouldn't crash.  The endpoints of some of the lines aren't centered on the text, but the slopes appear to be correct.  For the web version, he seems to have centered the endpoints of the lines on the corrected text, with the result being inaccurate slopes.

[The version from the book](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteBookVersion.jpg)

My code is capable of drawing:

[A) A replica of the original book version](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteCorrect.html)

[B) A replica of the version on Tufte's webpage, with the incorrect slopes](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteIncorrect.html)

[C) A version of B) with correct slopes, which is what the code was originally developed to do](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteNewCorrect.html)

### Tufte's vs Mine side-by-side:

![alt text](https://github.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/blob/master/TuftesVsMineSideBySide.png "Side-by-side")

### Tufte's vs Mine Overlay:

![alt text](https://github.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/blob/master/TuftesVsMineOverlay.png "Overlay")

### Survival rates slopegraph:

[Survival Rates](https://zrvc.github.io/Tufte-Style-Slopegraphs-in-D3-/TufteGillSansStyle.html)
