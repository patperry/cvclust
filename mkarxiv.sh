#!/usr/bin/env bash

ARXIV=arXiv

TITLE=mhlm

FILES=( \
    mhlm.bbl \
    mhlm_supp.pdf \
    macros.sty \
    statsoc.cls \
    figure/ml-10m-misclass.pdf \
    figure/ml-10m-coef-pairs.png \
    figure/sim-logistic.pdf \
    figure/sim-linear.pdf \
    figure/time-order.pdf
)

DISTDIR=${ARXIV}/${TITLE}


rm -rf "${ARXIV}"

mkdir -p "${DISTDIR}/figure"

for FILE in "${FILES[@]}"
do
    cp "${FILE}" "${DISTDIR}/${FILE}"
done

awk '
    $0 != "\\end{document}" {
        print $0
    }

    END {
        print "\\includepdf[pages={1-11}]{mhlm_supp}"
        print "\\end{document}"
    }
' mhlm.tex > "${DISTDIR}/mhlm.tex"


pushd ${ARXIV} > /dev/null
rm -f ${TITLE}.tar.gz
tar czvf "${TITLE}.tar.gz" "${TITLE}" > /dev/null
popd > /dev/null
