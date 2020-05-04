#!/bin/bash
TARGZFILE=$1
PDFFILE=$2
TMP_DIR=$(mktemp -d)
QPDF=$(which qpdf)
QPDF_OPTIONS="--compress-streams=y --object-streams=generate"
GS_OPTIONS="-dPDFSETTINGS=/ebook -dCompatibilityLevel=1.5 -dAutoRotatePages=/None -dPrinted=false -dNOPLATFONTS -dSAFER -dEmbedAllFonts=true"
if [ ! -x "$QPDF" ]; then
    echo "$0: qpdf not found, cannot compact vignettes"
    exit 1
fi
cd ${TMP_DIR}
tar -axf "$TARGZFILE"
if [ ! -r "$PDFFILE" ]; then
    echo "$0: cannot find $PDFFILE inside $TARGZFILE"
    exit 1
fi
echo Before: $(du -h "$PDFFILE")
gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite $GS_OTPIONS -sOutputFile="$PDFFILE-tmp" "$PDFFILE"
echo After gs: $(du -h "$PDFFILE-tmp")
mv "$PDFFILE-tmp" "$PDFFILE"
if [ $? -ne 0 ]; then
    echo "$0: cannot overwrite $PDFFILE"
    exit 1
fi
$QPDF $QPDF_OPTIONS "$PDFFILE" "$PDFFILE-tmp"
echo After qpdf: $(du -h "$PDFFILE-tmp")
mv "$PDFFILE-tmp" "$PDFFILE"
if [ $? -ne 0 ]; then
    echo "$0: cannot overwrite $PDFFILE"
    exit 1
fi
tar -acf "$TARGZFILE" *
if [ $? -ne 0 ]; then
    tar -atvf "$TARGZFILE"
    echo "$0: something failed !"
    exit 1
fi
echo "$0: Success !"
