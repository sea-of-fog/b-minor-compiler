#!/bin/bash
git ls-files | grep '\.ml\|\.bm\|\.sh' | xargs wc -l
