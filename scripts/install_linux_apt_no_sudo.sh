#!/bin/bash

# SPDX-FileCopyrightText: 2022 Intel Corporation
#
# SPDX-License-Identifier: MIT

COMPONENTS="${1//,/ }"
apt-get install -y "$COMPONENTS"
