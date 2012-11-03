#!/bin/sh

rsync -a --progress -h --delete --exclude root --exclude compiled ../../ plt-etc:local/galaxy/

for i in official planet-compat ; do
    rsync -a --progress -h --delete plt-etc:local/galaxy/meta/planet2-index/$i/root/ $i/root/
done

