#!/bin/bash

version=$(bumpversion --dry-run major | grep new_version | sed -r s,"^.*=",,)

git flow release start ${version}
bumpversion major
git flow release finish -m "Updated to new version ${version}" -p

