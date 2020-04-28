#!/bin/bash

rm  Samoyeet/*
../bnfc --functor -m -d samoyeet.cf
rm Samoyeet/Test.hs