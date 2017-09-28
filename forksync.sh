#!/bin/sh
git remote add upstream https://github.com/CGNS/CGNS.git
git fetch upstream
git merge upstream/develop

