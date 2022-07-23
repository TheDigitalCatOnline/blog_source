#!/bin/bash

src_dir="drawio"
dst_dir="pelican/content/images"

src_format="drawio"
dst_format="jpg"

if [[ ! -d ${src_dir} ]]; then echo "Directory ${src_dir} missing. Please run this from the root of the project"; exit 1; fi

for src_file in $(find ${src_dir} -type f -iname "*.${src_format}"); do
    border=50

    dst_file=$(echo ${src_file} | sed -r -e s,"^${src_dir}","${dst_dir}", -e s,"\.${src_format}$",".${dst_format}",)

    if [[ ${dst_file} -nt ${src_file} ]]; then continue; fi

    dirs=""
    dir=$(dirname ${src_file})
    while [[ ${dir} != "." ]]; do
	dirs="${dir} ${dirs}"
	dir=$(dirname ${dir})
    done

    for dir in ${dirs}; do
	custom_script="${dir}/custom.sh"
	if [[ -f ${custom_script} ]]; then
 	    source ${custom_script}
	fi
    done

    mkdir -p $(dirname ${dst_file})

    echo "Processing ${src_file}"
    drawio -x -b ${border} -f ${dst_format} -o ${dst_file} ${src_file} 2>/dev/null
    mogrify -shave 3x3 ${dst_file}
done


# for course in $(find ${src_dir} -mindepth 1 -maxdepth 1 -type d -printf "%P\n"); do

#     for image in $(find ${src_dir}/${course} -type f -iname "*.${src_format}" -printf "%P\n"); do
# 	border=50

# 	src_image=${src_dir}/${course}/${image}
# 	custom_script=$(dirname ${src_image})/custom.sh
# 	if [[ -f ${custom_script} ]]
# 	then
# 	    # echo "Sourcing ${custom_script}"
# 	    source ${custom_script}
# 	fi

# 	dst_image=$(echo "{dst_dir}/${course}/${image}" | sed -r -e s,"\.${src_format}$",".${dst_format}",)

# 	mkdir -p $(dirname ${dst_image})
	
# 	if [[ ${src_image} -nt ${dst_image} ]]
# 	then
# 	    echo "Processing ${src_image}"
# 	    drawio -x -b ${border} -f ${dst_format} -o ${dst_image} ${src_image} 2>/dev/null
# 	    mogrify -shave 3x3 ${dst_image}
# 	fi
#     done
# done
