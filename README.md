See https://sites.google.com/view/the-limekilns-cognition-study/start-at-the-beginning for a walkthrough

This tutorial will guide you through a basic analysis of fictional data. The tutorial is indended to be used as a worked example of how to start thinking about analysing data in R. It is in no way intended to be authoritative. Much more comprehensive guides are available on the internet and in published text books. My hope is that this is a useful primer, and will be specifically useful for those researching cognitive impairment and the use of neuropsychological tests. 
This tutorial will help you to
explore participant characteristics, with some common problems in the data 
perform basic descriptive statistics
measure statistical associations between key variables
complete a logistic regression
ROC curve analysis
You can work through this tutorial in a stats package of your choice, but the worked examples here are all in R. You can download R and RStudio for free (available for all platforms). Below is what RStudio looks like.
I leart R by making sense of some code which was written for a similar purpose, then trying to adapt that code to my own ends. This tutorial is designed very much with this mode of learning. Sample code in R supplied at the bottom of each page. This isn't a stats course, rather this tutorial assumes you have a working knowledge of stats somewhere at the back of your mind, but you just need to be shown how to do things in practice (and in R)

#© copyright of Lachlan Fotheringham
#This file is part of The Limekilns Cognition Study- an R tutorial
#The Limekilns Cognition Study- an R tutorial is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3
#The Limekilns Cognition Study- an R tutorial is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
See <https://www.gnu.org/licenses/> for more info
