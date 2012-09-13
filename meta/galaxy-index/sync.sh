#!/bin/sh

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/galaxy-index/$i/root/ $i/root/
done

rsync -a --progress -h --delete --exclude root --exclude compiled ../../ plt-etc:local/galaxy/
