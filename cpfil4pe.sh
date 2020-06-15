#!/bin/sh

while true; do
echo "Please input the name of element of each atom types in your .fdf file"
read na_el_pe
echo "Please choose that it is the Carbon potential (C) or Hydrogen potential (H) you want to change from"
while true; do
read pe
case $pe in
[C]*)
break 1
;;
[H]*)
break 1
;;
*)
echo "Please choose C or H for changing the potential file"
esac
done
cp $pe.psf $na_el_pe.psf # later please add check function that the file exists or not before copy
echo
ls -l $na_el_pe.psf
echo
echo "Do you have other atom types of element in your .fdf file needs to be created for potential files (Y or N)"
while true; do
read choose3
case $choose3 in
[Yy]*)
break 1
;;
[Nn]*)
break 2
;;
*)
echo "Please choose you have other elements or not (Y or N)"
esac
done
done
