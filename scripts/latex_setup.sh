#!/bin/sh
tlmgr option repository ctan
tlmgr update --self
tlmgr option -- autobackup 0
tlmgr option -- docfiles 0
tlmgr option -- srcfiles 0
#tlmgr option -- require-verification 0
#tlmgr option -- verify-downloads 0
#tlmgr option -- verify-repo none
tlmgr install \
      oberdiek \
      epstopdf-pkg \
      epstopdf \
      grfext \
      gettitlestring \
      environ \
      trimspaces \
      etoolbox \
      upquote \
      babel-english \
      pgf \
      xcolor \
      algorithms \
      relsize \
      tocbibind \
      appendix \
      tocloft \
      enumitem \
      listings \
      tcolorbox \
      collection-fontsrecommended \
      framed \
      inconsolata \
      microtype

