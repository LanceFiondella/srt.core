Software Reliability Tool ([SRT](http://sasdlc.org))
--------------------------------

A web oriented software reliabilty testing suite written in R. Made to parse data sets and predict failure models based on common statistical approaches using the shiny framework.

Lance Fiondella, University of Massachusetts Dartmouth

Allen Nikora, Jet propulsion laboratory/ California Institute of technology


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

Deploy using docker image on Linux
-----------------------------
- SRT is available as docker image and can be pulled from here (https://hub.docker.com/r/arhik/srt/)
- Install docker-engine (follow instructions on https://docs.docker.com/engine/installation/)
- Pull the image using </br>
```$ docker pull arhik/srt```
- deploy by portforwarding desired port on your system to the default 3838 port using </br>
```$ docker run -d -p xxxx:3838 arhik/srt```


Contribution:
-------------
The aim of this tool is to allow third party contributions both in terms of architecture and models.

### Model Contributors Guide
--------------------------
- Link to contributors guide ()

Cons:
- [ ] PDF export of tables is not supported yet.
- [ ] Only NHPP Models are included.

TODO:

- [ ] Python port of this is in progress.

Caution
-------
- Under Heavy development use with caution.

Versioning:
----------
- v1.0 is released.

Acknowledgement
--------------
This research was supported by (i) the Naval Air Systems Command (NAVAIR) through the Systems Engineering Research Center (SERC), a Department of Defense (DoD) University Affiliated Research Center (UARC) under Research Task 139: Software Reliability Modeling and (ii) the National Science Foundation under Grant Number (1526128).



Copyright and License
----------------------
Code release under [MIT LICENSE](https://github.com/LanceFiondella/srt.core/blob/master/LICENSE.md). 
