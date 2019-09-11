#!/bin/bash
TARGZFILE=$1
PDFFILE=$2
TMP_DIR=$(mktemp -d)
QPDF=$(which qpdf)
QPDF_OPTIONS="--compress-streams=y --object-streams=generate"
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
$QPDF $QPDF_OPTIONS "$PDFFILE" "$PDFFILE-tmp"
mv "$PDFFILE-tmp" "$PDFFILE"
if [ $? -ne 0 ]; then
    echo "$0: cannot overwrite $PDFFILE"
    exit 1
fi
ps2pdf -dPDFSETTINGS=/ebook -dAutoRotatePages=/None -dPrinted=false "$PDFFILE" "$PDFFILE-tmp"
mv "$PDFFILE-tmp" "$PDFFILE"
if [ $? -ne 0 ]; then
    echo "$0: cannot overwrite $PDFFILE"
    exit 1
fi
tar -acf "$TARGZFILE" *
tar -atvf "$TARGZFILE"
if [ $? -ne 0 ]; then
    echo "$0: something failed !"
    exit 1
fi
echo "$0: Success !"
