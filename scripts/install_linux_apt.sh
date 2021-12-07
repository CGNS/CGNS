#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

COMPONENTS=$(echo "$1" | sed "s/,/ /g")
#shellcheck disable=SC2086
sudo apt-get install -y $COMPONENTS
sudo apt-get clean
