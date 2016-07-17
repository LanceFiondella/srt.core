Software Reliability Tool ([SRT](http://sasdlc.org))
--------------------------------

A web oriented software reliabilty testing suite written in R. Made to parse data sets and predict failure models based on common statistical approaches using the shiny framework.

Made by Lance Fiondella at the University of Massachusetts Dartmouth

Allen P. Nikora at Jet propulsion laboratory/ California Institute of technology


Dependancies
-------
Using `source("install_script.R")` located in this repository will resolve the dependancies for SRT
 

To Run
-------

```R
library(shiny)  
runApp("/Path/To/Dir/SRT/")
```
This will launch the app on localhost on a random port and attempt to open a browser.

Deploy using dockers on Linux
-----------------------------

- Install docker-engine (follow instructions on https://docs.docker.com/engine/installation/)
- Pull the image using </br>
```$ docker pull arhik/srt```
- deploy by portforwarding desired port on your system to the default 3838 port using </br>
```$ docker run -d -p xxxx:3838 arhik/srt```

