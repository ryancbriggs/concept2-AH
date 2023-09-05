# Rowing and Apple Health dashboard
This is a simple set of R scripts that takes in Apple Health data and Concept2 rowing data and outputs a pretty 1 page pdf made in Quarto.

It is somewhat specific to me and I am posting it because some of the munging or formatting code might be useful to others.

The Apple Health data comes from an Apple Watch (manually exported from the Apple Health app). It comes out as an XML file. The rowing data comes from the Concept2 webpage, where each rowing season is a unique csv file. These data files are expected to be in a folder called "Data".

A makefile holds it all together and depends on the scripst outputting logs in kind of hacky way. The indiviudal scripts have some constants in them that are specific to me but easy to change. If anyone actually cares, I can clean it up.
