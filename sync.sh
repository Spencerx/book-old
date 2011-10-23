#!/bin/sh

if [ ! -x site ] ; then
  echo "'site' binary is not found, could not generate site!"
  exit 1
fi

./site clean
./site build

rsync -avz --delete _site/* nlpwp@nlpwp.org:site/
