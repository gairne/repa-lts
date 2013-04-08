#!/bin/bash

runhaskell Setup.hs clean && runhaskell Setup.hs configure --user && runhaskell Setup.hs build