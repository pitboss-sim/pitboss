{ pkgs }:
pkgs.writeShellScriptBin "coverage" ''
  time cabal test \
    --builddir=dist-coverage \
    --enable-coverage \
    --enable-executable-dynamic \
    --disable-library-vanilla \
    --ghc-options="-fhpc -dynamic" \
    --exclude= \

  rm -rf .hpc coverage
  mkdir -p .hpc coverage

  for mixdir in $(find dist-coverage -type d -name "pitboss-0.1.0.0-inplace" -path "*/mix/*"); do
    echo "$mixdir"
    cp -r "$mixdir" .hpc/
  done

  for mixdir in $(find dist-coverage -type d -name "pitboss-*-inplace*" -path "*/mix/*"); do
    cp -r "$mixdir" .hpc/ 2>/dev/null || true
  done

  find dist-coverage -name "*.mix" -exec cp {} .hpc/ \;
  find dist-coverage -name "*.tix" -exec cp {} coverage/pitboss-test.tix \;

  if [ -f "coverage/pitboss-test.tix" ]; then
    echo ""
    echo ""

    EXCLUDES="--exclude=Spec.*"

    # Generate HTML for humans
    hpc markup coverage/pitboss-test.tix \
      --hpcdir=.hpc \
      --destdir=coverage/html \
      $EXCLUDES
    echo "HTML: coverage/html/hpc_index.html"

    hpc report coverage/pitboss-test.tix \
      --hpcdir=.hpc > coverage/overall.txt \
      $EXCLUDES
    echo "Overall: coverage/overall.txt"

    hpc report coverage/pitboss-test.tix \
      --per-module \
      --hpcdir=.hpc > coverage/per-module.txt \
      $EXCLUDES

    echo "Per-module: coverage/per-module.txt"
  else
    echo "Error: .tix file not found in coverage/"
  fi
''
