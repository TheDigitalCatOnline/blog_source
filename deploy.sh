#!/bin/bash

AWS_PROFILE="tdc"
OUTPUT_DIR="pelican/output"
S3_BUCKET="www.thedigitalcatonline.com"
CLOUDFRONT_DISTRIBUTION="E1VK6I0BH0G8RZ"

cd pelican
make publish
cd ..

AWS_PROFILE=${AWS_PROFILE} s3cmd sync ${OUTPUT_DIR}/ s3://${S3_BUCKET} --acl-public --delete-removed --no-mime-magic
awsv2 --profile ${AWS_PROFILE} cloudfront create-invalidation --distribution-id ${CLOUDFRONT_DISTRIBUTION} --paths "/*"
