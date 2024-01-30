#!/bin/bash
git ls-files | grep '\.ml\|.bm' | xargs wc -l
