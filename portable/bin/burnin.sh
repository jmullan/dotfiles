#!/bin/bash
DRIVE=/dev/sde
sudo s
echo sudo badblocks -b 4096 -vwsp 4 "${DRIVE}"
echo sudo dd if=/dev/zero of="${DRIVE}" iflag=nocache oflag=direct bs=4096
