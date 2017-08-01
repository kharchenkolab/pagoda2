# Docker for R with Pagoda2
Use a version of R-3.4.1 with Pagoda2 and all dependencies preinstalled.

## Obtaining docker:
#### Windows
https://docs.docker.com/docker-for-windows/install/
#### Mac
https://docs.docker.com/docker-for-mac/install/
#### Ubuntu
https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/

#### OR install using online script
curl -sSL https://get.docker.com/ | sh


## Downloading the docker image:
Obtain the file `docker-p2.tar.gz` from http://pklab.med.harvard.edu/nikolas/pagoda2/docker/docker-p2.tar.gz

Importing the tar into the docker system with `docker load`
Creates a container called docker-p2:latest in your docker. 
> docker load -i docker-p2.tar.gz 

Check available docker images with:
> docker images

# QuickStart with resources and options set:

> docker run -ti --rm --cpus 4 -m 12g -v /$HOME/data:/pagoda2 docker-p2 

## How to run this docker container 
Open interactive R shell of the container:
> docker run -ti docker-p2  

This docker container opens by default in the path /pagoda2 

Open a bash shell in the filesystem of the container.  
> docker run -ti docker-p2 /usr/bin/bash  

Normally if you run a container without options it will start and stop immediately, if you want keep it running you can use the command, docker run -td container_id this will use the option -t that will allocate a pseudo-TTY session and -d that will detach automatically the container (run container in background and print container ID).

If you want a transient container, docker run --rm will remove the container after it stops.

#### Detach from running docker:
Leave a docker container running in the background: CTRL-p CTRL-q

#### Show running containers and get id 
> docker ps

#### Run the docker in the background:
> docker run -tdi docker-p2
returns CONTAINER ID

#### Connect to a running container:
> docker attach [CONTAINER ID]

#### Stop detached docker container
by exiting R or bash while attached or with
> docker stop [CONTAINER ID]

## Resource management and File-handling
### Mounting Folders or Volumes into the docker
Use run with the -v flag:  
Syntax: docker run -v /HOSTPATH:/GUESTPATH    
> docker run -v /home/data:/pagoda2/data docker-p2

### Resource Management:
Further flags for `docker run`:


#### CPU
- --cpus: number of cores to make accesible to the container  

- -c: Share of the CPUs allowed to be used by the container [0-1024] 

  - --c 512 would imply using a maximum of 50% of each core

- --cpuset: specify exactly which CPUs to use. 
    - --cpuset=0,1


#### MEM
- -m: Allocate memory. Currently no independent management of Swap. 
  - size of swap = size of memory
  - Understands eg. 128m, 6g
  - Should be set!

#### Port forwarding
- --p 5000:22 - forward port 22 of guest to host 5000



https://docs.docker.com/engine/reference/run/#specify-custom-cgroups 

https://goldmann.pl/blog/2014/09/11/resource-management-in-docker/#_cpu



### Docker cheat-sheet and useful information
https://github.com/wsargent/docker-cheat-sheet  

https://www.calazan.com/docker-cleanup-commands/



