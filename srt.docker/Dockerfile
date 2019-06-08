FROM ubuntu:15.04

MAINTAINER Karthik (arhik) "arhik23@gmail.com"
# Install dependencies

# RUN apt-get update -y

# Install R

RUN echo 'deb http://cran.rstudio.com/bin/linux/ubuntu vivid/' > /etc/apt/sources.list.d/r.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 && \
    apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y r-base wget git libxml2-dev curl nginx gdebi-co$
RUN su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
RUN echo 'options("repos"="http://cran.us.r-project.org")' > /.Rprofile
RUN wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.1.759-amd64.deb -O shiny-$
EXPOSE 3838
RUN mkdir -p /srv/SRT/srt.core
COPY srt.core /srv/SRT/srt.core
RUN rm /etc/shiny-server/shiny-server.conf
COPY srt.core/shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN Rscript /srv/SRT/srt.core/install_script.R
CMD shiny-server
