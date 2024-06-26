#!/bin/bash

# SPDX-FileCopyrightText: 2022 Intel Corporation
#
# SPDX-License-Identifier: MIT

COMPONENTS=$(echo "$1" | sed "s/,/ /g")
apt-get install -y "$COMPONENTS"
