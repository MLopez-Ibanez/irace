#!/bin/bash
set -u
set -o pipefail
TARGZFILE=$1
PDFFILE=$2
TMP_DIR=$(mktemp -d)
QPDF=$(which qpdf)
QPDF_OPTIONS="--compress-streams=y --object-streams=generate"
GS_OPTIONS="-dPDFSETTINGS=/ebook -dCompatibilityLevel=1.5 -dAutoRotatePages=/None -dPrinted=false -dNOPLATFONTS -dSAFER -dEmbedAllFonts=true -dCompressFonts=true -dSubsetFonts=true"
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
BEFORE_SIZE=$(stat -c%s "$PDFFILE")
gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite $GS_OPTIONS -sOutputFile="$PDFFILE-tmp" "$PDFFILE"
echo After gs: $(du -h "$PDFFILE-tmp")
AFTER_SIZE=$(stat -c%s "$PDFFILE-tmp")
if (( BEFORE_SIZE > AFTER_SIZE)); then
    mv "$PDFFILE-tmp" "$PDFFILE"
    if [ $? -ne 0 ]; then
        echo "$0: cannot overwrite $PDFFILE"
        exit 1
    fi
    echo "$0: Success !"
fi
$QPDF $QPDF_OPTIONS "$PDFFILE" "$PDFFILE-tmp"
echo After qpdf: $(du -h "$PDFFILE-tmp")
BEFORE_SIZE=$(stat -c%s "$PDFFILE")
AFTER_SIZE=$(stat -c%s "$PDFFILE-tmp")
if (( BEFORE_SIZE > AFTER_SIZE)); then
    mv "$PDFFILE-tmp" "$PDFFILE"
    if [ $? -ne 0 ]; then
        echo "$0: cannot overwrite $PDFFILE"
        exit 1
    fi
    echo "$0: Success !"
fi
R --slave -e "tools::compactPDF('$PDFFILE', gs_quality = 'ebook')"
echo After compactPDF: $(du -h "$PDFFILE")
tar -acf "$TARGZFILE" *
if [ $? -ne 0 ]; then
    tar -atvf "$TARGZFILE"
    echo "$0: something failed !"
    exit 1
fi
echo "$0: DONE"
