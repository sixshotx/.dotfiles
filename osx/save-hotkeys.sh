#!/bin/sh
# save-hotkeys.sh

# To map a menubar command to nothing, set it equal to \1 in apply-keys.sh

DESTFILE=${PWD}/apply-keys.sh
echo '#!/bin/bash' > $DESTFILE

defaults find NSUserKeyEquivalents | sed -e "s/Found [0-9]* keys in domain '\\([^']*\\)':/defaults write '\\1' NSUserKeyEquivalents '/" -e "s/    NSUserKeyEquivalents =     {//"  -e "s/};//" -e "s/}/}'/" >> $DESTFILE
echo killall cfprefsd >> $DESTFILE
chmod a+x $DESTFILE
