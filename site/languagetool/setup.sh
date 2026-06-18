#!/bin/bash
# Download and install LanguageTool for Emacs integration
# Run this on new machines to set up LanguageTool

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check for existing installation
if ls LanguageTool-*/languagetool-server.jar 2>/dev/null; then
    echo "LanguageTool already installed:"
    ls -d LanguageTool-*/
    exit 0
fi

echo "Downloading LanguageTool..."
curl -LO https://languagetool.org/download/LanguageTool-stable.zip

echo "Extracting..."
unzip -q LanguageTool-stable.zip
rm LanguageTool-stable.zip

echo "Done. Installed:"
ls -d LanguageTool-*/

cat << 'NGRAM_INFO'

================================================================================
OPTIONAL: N-gram language model (~15GB)

N-grams enable context-aware confusion-pair detection (their/there, its/it's).
LanguageTool works without them, just with fewer style checks.

To install:
  1. Download en.zip from https://languagetool.org/download/ngram-data/
     (look for "ngram data" section - ~13GB compressed)
  2. Extract so that ngrams/en/ exists:
       mkdir -p ngrams && unzip en.zip -d ngrams/
  3. Restart Emacs - flymake-languagetool will detect and use them automatically

The emacs config only passes --languageModel if ngrams/en/ exists.
================================================================================
NGRAM_INFO
