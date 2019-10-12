# Tufte-Like-Slopegraphs-in-D3-

The purpose of this project was to use D3.js to replicate two of Edward Tufte's slopegraphs found here:

https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk

The [stacked slopegraph for survival rates](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteGillSansStyle.html) was pretty easy to draw, but the graph of government receipts was harder. To plot the points without the labels crashing, I used constrained optimization. I minimized the sum of squared distances between the points and their ideal positions, while imposing a minimum distance between the labels and ensuring that the lines were all drawn on the same scale.

Here's a comparison of my attempt with Tufte's:

### Tufte's Web Version (Left) vs My Version C (Purple)

<img align="middle" src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteVsMineSBS.png" width="890">



<br>This overlay demonstrates the extent to which my lines disagree with Tufte's. The fact that some are so close and some are so far off means that some of his slopes are incorrect.

<img align="middle" src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteVsMineOverlay.png">

<br>I couldn't figure out why Tufte drew this slopegraph the way he did. Then, I got ahold of the printed version. <i>The graph in his book is different from the one on his webpage</i>.

### Compare the Lines from the Book Version to Those of the Unadjusted Points

<img src="https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/images/TufteBookVsCrash.jpg">

In the book, it looks like he plotted the lines without adjustment and moved the labels so that they wouldn't crash. Some of the lines' endpoints aren't centered on the text, but the slopes look good to me. For the web version, he seems to have centered the endpoints of the lines on the adjusted text⁠—producing inaccurate slopes.

### My Code
An [R script](https://github.com/ZRVc/Tufte-Like-Slopegraphs-in-D3-/blob/master/ROI5.R) sets up and solves an optimization problem to find the points, which are then plotted in D3.js. This code is capable of drawing:

[A) A replica of the original book version](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteCorrect.html)

[B) A replica of the version on Tufte's webpage, with the incorrect slopes](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteIncorrect.html)

[C) A version of B) with correct slopes, which is what the code was originally developed to do](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteNewCorrect.html)

[D) A version of C) using quadratic constraints; notice the different ordering of Canada and Belgium in the first column](https://zrvc.github.io/Tufte-Like-Slopegraphs-in-D3-/TufteQuadratic124.html)
