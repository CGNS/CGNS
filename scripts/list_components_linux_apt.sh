#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

FILENAME=$1

sudo apt-cache search ^intel- | sudo tee "$FILENAME"
