#!/bin/bash

AWS_PROFILE="tdc"
OUTPUT_DIR="pelican/output"
DEPLOY_DIR="deploy"
S3_BUCKET="www.thedigitalcatonline.com"
CLOUDFRONT_DISTRIBUTION="E1VK6I0BH0G8RZ"

cd pelican
make publish
cd ..

rm -fR ${DEPLOY_DIR}/*
cp -R ${OUTPUT_DIR}/* ${DEPLOY_DIR}

AWS_PROFILE=${AWS_PROFILE} s3cmd sync ${DEPLOY_DIR}/ s3://${S3_BUCKET} --acl-public --delete-removed --no-mime-magic
aws --profile ${AWS_PROFILE} cloudfront create-invalidation --distribution-id ${CLOUDFRONT_DISTRIBUTION} --paths "/*"
