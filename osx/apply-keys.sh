#!/bin/bash
defaults write 'com.google.Chrome' NSUserKeyEquivalents ' {

        Downloads = "\001";
        "Quit Google Chrome" = "@~q";
    
}'
defaults write 'org.mozilla.aurora' NSUserKeyEquivalents ' {

        Inspector = "@~^v";
    
}'
defaults write 'Apple Global Domain' NSUserKeyEquivalents ' {

        Minimize = "@^$.";
    
}'
defaults write 'com.sublimetext.2' NSUserKeyEquivalents ' {

        "Quit Sublime Text 2" = "@~q";
    
}'
killall cfprefsd
