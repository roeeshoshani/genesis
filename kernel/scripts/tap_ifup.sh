#!/bin/sh
ip l set up dev "$1"
ip a add 6.6.6.8/24 dev "$1"
