# Patch to fix Opus Magnum
```diff
diff -Naur ~/.local/share/Steam/steamapps/common/Opus\ Magnum/Lightning.old ~/.local/share/Steam/steamapps/common/Opus\ Magnum/Lightning
--- Lightning.old	2018-10-03 09:44:32.388761529 +0200
+++ Lightning	2018-10-03 09:31:56.335354893 +0200
@@ -29,7 +29,7 @@
 else
 	# Restore the original library search path that was replaced by the Steam runtime.
 	# This should make it possible to launch a PDF reader or other installed programs.
- export LD_LIBRARY_PATH=$STEAM_LD_LIBRARY_PATH
- export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$STEAM_LD_LIBRARY_PATH
 
 	if [ "$ARCH" == "x86_64" ]; then
 		./Lightning.bin.x86_64 $@
```
