#!/bin/bash

#CONTAINER_NAME="test_e2e_instance"
#IMAGE_NAME="nibio/e2e-instance"

CONTAINER_NAME="segmentanytree_run"
IMAGE_NAME="donaldmaen/segment-any-tree:latest"


if [ $(docker container ls -a -q -f name=$CONTAINER_NAME) ]; then
    docker container rm -f $CONTAINER_NAME
	fi


# Check if the image exists
# if [ $(docker image ls -q -f reference=$IMAGE_NAME) ]; then
#     echo "Removing existing image $IMAGE_NAME"
#     docker image rm $IMAGE_NAME
# else
#     echo "Image $IMAGE_NAME does not exist."
# fi

# ./build.sh
#docker build -t $IMAGE_NAME .

echo "Running the container"
# docker run -it --gpus all --name $CONTAINER_NAME $IMAGE_NAME > e2e-instance.log 2>&1



###The Right One for FLAME Lab
docker run -it --rm --gpus 1 \
    --name $CONTAINER_NAME \
    --mount type=bind,source=/mnt/d/FlameLab/TLS/Lubrecht/ThinFinal,target=/home/nibio/mutable-outside-world/bucket_in_folder \
    --mount type=bind,source=/mnt/d/FlameLab/TLS/Lubrecht/segmented,target=/home/nibio/mutable-outside-world/bucket_out_folder \
    --mount type=bind,source=$(pwd)/nibio_inference,target=/home/nibio/mutable-outside-world/nibio_inference \
       donaldmaen/segment-any-tree:latest bash

#docker run -it --rm --gpus 1 \
#    --name $CONTAINER_NAME \
#    --mount type=bind,source=/mnt/d/ALK/FARO_Processing/Step_2_Normalized,target=/home/nibio/mutable-outside-world/bucket_in_folder \
#    --mount type=bind,source=/mnt/d/ALK/FARO_Processing/Step_3_Segmented,target=/home/nibio/mutable-outside-world/bucket_out_folder \
#    --mount type=bind,source=$(pwd)/nibio_inference,target=/home/nibio/mutable-outside-world/nibio_inference \
#       donaldmaen/segment-any-tree:latest bash


#maciekwielgosz/segment-any-tree:lates
#    --mount type=bind,source=/home/nibio/mutable-outside-world/code/PanopticSegForLargeScalePointCloud_maciej/bucket_in_folder,target=/home/nibio/mutable-outside-world/bucket_in_folder \
 #   --mount type=bind,source=/home/nibio/mutable-outside-world/code/PanopticSegForLargeScalePointCloud_maciej/bucket_out_folder,target=/home/nibio/mutable-outside-world/bucket_out_folder \
   #$IMAGE_NAME bash 
