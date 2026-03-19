#!/bin/bash

# Generate PDF from Competitive Programming Guide
# This script handles special characters properly

echo "Generating PDF..."

# First, try with basic pdflatex settings
pandoc COMPETITIVE_PROGRAMMING_GUIDE.md \
  -o COMPETITIVE_PROGRAMMING_GUIDE.pdf \
  --toc \
  --toc-depth=3 \
  --number-sections \
  -V geometry:margin=0.75in \
  -V fontsize=9pt \
  -V linkcolor=blue \
  -V colorlinks=true \
  -V urlcolor=blue \
  --highlight-style=tango \
  --pdf-engine=pdflatex \
  -V 'mainfont:DejaVu Sans' \
  2>&1

if [ $? -eq 0 ]; then
  echo "✓ PDF generated successfully: COMPETITIVE_PROGRAMMING_GUIDE.pdf"
  ls -lh COMPETITIVE_PROGRAMMING_GUIDE.pdf
else
  echo "✗ PDF generation failed. Trying alternative method..."

  # Alternative: Create HTML first, then print
  echo "Generating HTML version instead..."
  pandoc COMPETITIVE_PROGRAMMING_GUIDE.md \
    -o COMPETITIVE_PROGRAMMING_GUIDE.html \
    --toc \
    --toc-depth=3 \
    --number-sections \
    --standalone \
    --css=print-style.css \
    --highlight-style=tango

  if [ $? -eq 0 ]; then
    echo "✓ HTML generated: COMPETITIVE_PROGRAMMING_GUIDE.html"
    echo "  You can:"
    echo "  1. Open in browser and Print to PDF (Ctrl+P)"
    echo "  2. Install wkhtmltopdf: sudo apt-get install wkhtmltopdf"
    echo "     Then run: wkhtmltopdf COMPETITIVE_PROGRAMMING_GUIDE.html COMPETITIVE_PROGRAMMING_GUIDE.pdf"
  fi
fi
