#!/bin/bash

# SPDX-FileCopyrightText: 2022 Intel Corporation
#
# SPDX-License-Identifier: MIT

COMPONENTS="${1//,/ }"
#shellcheck disable=SC2086
dnf -y install $COMPONENTS
