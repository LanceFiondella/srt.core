OCK_PATH=~/srt-app
sudo docker build -t arhik/srt ${DOCK_PATH}
sudo docker run -d -p 1237:3838 arhik/srt
sudo docker build -t rdavis/srt ${DOCK_PATH}
sudo docker run -d -p 1234:3838 rdavis/srt
sudo docker build -t dgarcia1/srt ${DOCK_PATH}
sudo docker run -d -p 1235:3838 dgarcia1/srt 
sudo docker build -t vnagaraju/srt ${DOCK_PATH}
sudo docker run -d -p 1236:3838 vnagaraju/srt
sudo docker build -t rmuri/srt ${DOCK_PATH}
sudo docker run -d -p 1238:3838 rmuri/srt

