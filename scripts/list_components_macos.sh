#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

URL=$1
FILENAME=$2

curl --output webimage.dmg --url "$URL" --retry 5 --retry-delay 5
hdiutil attach webimage.dmg -quiet
sudo /Volumes/"$(basename "$URL" .dmg)"/bootstrapper.app/Contents/MacOS/bootstrapper --list-components | sudo tee "$FILENAME"
installer_exit_code=$?
hdiutil detach /Volumes/"$(basename "$URL" .dmg)" -quiet
exit $installer_exit_code
