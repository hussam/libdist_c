#!/bin/bash

if [ -f rel/num_dev_rels ]
then
   N=`cat rel/num_dev_rels`
   echo "Generating $N dev rels"
else
   N=6
   echo "No rel/num_dev_rels file found. Assuming a default number of $N dev rels"
   echo $N > rel/num_dev_rels
fi

for i in $(seq 1 $N)
do
   echo "Making dev rel #$i"
   cp -r dev/gen_dev dev/dev$i
   sed -i '' -e "s/NODE/node$i/" dev/dev$i/etc/vm.args
done
rm -r dev/gen_dev
