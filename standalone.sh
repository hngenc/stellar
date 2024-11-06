#!/bin/bash

for file in $(find . -name '*.chipyard'); do
    name=$(basename $file .chipyard)
    name="$(dirname $file)/$name"
    cp $name $file
done

