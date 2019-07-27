# myphoto

This implemments parts of my image handling workflow in a composable way. For example the following command
```
./myphoto.sh unraw --wb1 \
             untiff --rm \
             align \
             show wait \
             stack \
             untiff \
             copy ../results \
             show \
             -- IMG1 IMG2 IMG3 ...
             
```
- converts raw files to tiff, and uses the first raw as a graycard image
- converts the tiff files to png and removes the tiff files
- aligns the png files
- show all images in the current pipeline in a image viewer while waiting for user confirmation whether to continue
- stacks the aligned images
- converts the stacked result from tiff to png, but keeping the tiff this time
- copys the final png to the folder `../results`
- shows the result

## Modules:
### UnRAW
Takes raw files and converts them to 48bit tiff.
### UnTiff
Takes tiff files and converts them to lossless compressed png files.
### Align
Takes images (tiff, png or jpg) and aligns them.
### Stack
Takes (aligned) images and does focus stacking on them.
### Show
Shows the result in an image viewer.
### Minor modules:
#### Copy
#### Wait
## Not yet ported modules:
### Galery
Create a simple static HTML galery of images.
### Init
Initially create the folder structure and optionally import files
## Not yet implemented modules:
#### Link
