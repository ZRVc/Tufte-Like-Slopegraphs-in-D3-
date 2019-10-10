# Tufte-Like-Slopegraphs-in-D3-

The purpose of this project was to use D3.js replicate two of Edward Tufte's slopegraphs found here:

https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk

The [stacked slopegraph for survival rates](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteGillSansStyle.html) was pretty easy to do, but the graph of government receipts was harder. I developed it into a constrained optimization problem, where I minimise the sum of squared distances between the points and their ideal positions, while imposing a minimum distance between the labels and ensuring that the slopes are all drawn on the same scale. Here's a comparison of my attempt with Tufte's:

### Tufte's Web Version (Left) vs My Version C (Purple)

<img align="middle" src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteVsMineSBS.png" width="890px">



This overlay demonstrates the extent to which my lines disagree with Tufte's. The fact that some are so close and some are so far off means that some of his slopes are incorrect.

<img align="middle" src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteVsMineOverlay.png">

I couldn't figure out why Tufte drew this slopegraph the way he did. Then, I got ahold of the printed version. <i>The graph in the book is different from the one on the web</i>. In the book, it looks like he plotted the lines where they should be and adjusted the labels so that they wouldn't crash. The endpoints of some of the lines aren't centered on the text, but the slopes look good to me. For the web version, he seems to have centered the endpoints of the lines on the adjusted text, producing inaccurate slopes.

### Compare the Lines from the Book Version to Those of the Unadjusted Points
<img src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteBookSmall.jpg" width="487"><img src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteCrashSmall.png" width="324">

### My Code
The graphs are drawn in D3.js. The points that are plotted come from an R script that uses the ROI package to set up and solve the optimization problem.

The current version of the R code is [ROI5.R](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/ROI5.R).  It is nearly to the point where others can easily use it.  Currently, it breaks ties with a separate optimization procedure, designed to give a feasible region that satisfies strict criteria, in order to allow for a variety of constraints later on.  It uses linear constraints to preserve the order of the tied points, but, if the user specifies, one or both columns can be given quadratic constraints so that the optimizer can choose the order.

My code is capable of drawing:

[A) A replica of the original book version](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteCorrect.html)

[B) A replica of the version on Tufte's webpage, with the incorrect slopes](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteIncorrect.html)

[C) A version of B) with correct slopes, which is what the code was originally developed to do](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteNewCorrect.html)

[D) A version of C) using quadratic constraints; notice the different ordering of Canada and Belgium in the first column](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteQuadratic124.html)

The font I'm using came from here:
https://edwardtufte.github.io/tufte-css/
