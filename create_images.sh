#!/bin/bash

src_dir="images"
dst_dir="pelican/content/images"

if [[ ! -d ${src_dir} ]]; then echo "Directory ${src_dir} missing. Please run this from the root of the project"; exit 1; fi

for src_file in $(find ${src_dir} -type f); do
    border=50

    if [[ ! -z $(basename ${src_file} | grep -E "\.drawio$") ]]; then
	dst_file=$(echo ${src_file} | sed -r -e s,"^${src_dir}","${dst_dir}", -e s,"\.drawio$",".jpg",)
    else
	dst_file=$(echo ${src_file} | sed -r -e s,"^${src_dir}","${dst_dir}",)
    fi

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

    echo "Processing ${src_file} --> ${dst_file}"
    if [[ ! -z $(basename ${src_file} | grep -E "\.drawio$") ]]; then
	drawio -x -b ${border} -f jpg -o ${dst_file} ${src_file} 2>/dev/null
	mogrify -shave 3x3 ${dst_file}
    else
	cp ${src_file} ${dst_file}
    fi
done
