#!/bin/bash

# Generate all printable formats for Competitive Programming Guide
# Run this script to regenerate everything after making changes

echo "🚀 Generating all formats..."
echo ""

# Generate HTML versions (always works)
echo "📄 Generating HTML files..."
pandoc COMPETITIVE_PROGRAMMING_GUIDE.md \
  -o COMPETITIVE_PROGRAMMING_GUIDE.html \
  --toc --toc-depth=3 --number-sections \
  --standalone --css=print-style.css \
  --highlight-style=tango \
  --metadata title="Competitive Programming Guide" \
  && echo "  ✓ COMPETITIVE_PROGRAMMING_GUIDE.html"

pandoc CHEAT_SHEET.md \
  -o CHEAT_SHEET.html \
  --standalone --css=print-style.css \
  --highlight-style=tango \
  --metadata title="Quick Reference Cheat Sheet" \
  && echo "  ✓ CHEAT_SHEET.html"

echo ""

# Try to generate PDFs if tools are available
if command -v wkhtmltopdf &> /dev/null; then
  echo "📄 Generating PDFs with wkhtmltopdf..."
  wkhtmltopdf -q COMPETITIVE_PROGRAMMING_GUIDE.html COMPETITIVE_PROGRAMMING_GUIDE.pdf 2>/dev/null \
    && echo "  ✓ COMPETITIVE_PROGRAMMING_GUIDE.pdf"
  wkhtmltopdf -q CHEAT_SHEET.html CHEAT_SHEET.pdf 2>/dev/null \
    && echo "  ✓ CHEAT_SHEET.pdf"
elif command -v xelatex &> /dev/null; then
  echo "📄 Generating PDFs with XeLaTeX..."
  pandoc COMPETITIVE_PROGRAMMING_GUIDE.md \
    -o COMPETITIVE_PROGRAMMING_GUIDE.pdf \
    --pdf-engine=xelatex \
    --toc --toc-depth=3 --number-sections \
    -V geometry:margin=0.75in -V fontsize=9pt \
    2>/dev/null && echo "  ✓ COMPETITIVE_PROGRAMMING_GUIDE.pdf"

  pandoc CHEAT_SHEET.md \
    -o CHEAT_SHEET.pdf \
    --pdf-engine=xelatex \
    -V geometry:margin=0.75in -V fontsize=10pt \
    2>/dev/null && echo "  ✓ CHEAT_SHEET.pdf"
else
  echo "⚠️  PDF generation skipped (install wkhtmltopdf or texlive-xetex)"
  echo "   HTML files can be printed to PDF from your browser (Ctrl+P)"
fi

echo ""
echo "✨ Done! Generated files:"
ls -lh COMPETITIVE_PROGRAMMING_GUIDE.html CHEAT_SHEET.html 2>/dev/null | awk '{print "   " $9 " (" $5 ")"}'
ls -lh COMPETITIVE_PROGRAMMING_GUIDE.pdf CHEAT_SHEET.pdf 2>/dev/null | awk '{print "   " $9 " (" $5 ")"}'

echo ""
echo "📖 To print: Open HTML files in browser and press Ctrl+P"
echo "📖 See PRINT_INSTRUCTIONS.md for detailed instructions"
