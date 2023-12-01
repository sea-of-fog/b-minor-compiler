#!/bin/bash
git ls-files | grep '\.ml' | xargs wc -l
