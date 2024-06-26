#!/bin/bash

# SPDX-FileCopyrightText: 2022 Intel Corporation
#
# SPDX-License-Identifier: MIT

COMPONENTS=$(echo "$1" | sed "s/,/ /g")
#shellcheck disable=SC2086
sudo dnf -y install $COMPONENTS
