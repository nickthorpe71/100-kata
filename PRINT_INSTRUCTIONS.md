# Print Instructions for Competitive Programming Guide

You now have **multiple printable formats** of your competitive programming guide!

---

## 📄 Files Created

### 1. **Full Guide**
- `COMPETITIVE_PROGRAMMING_GUIDE.md` - Original markdown (40+ pages)
- `COMPETITIVE_PROGRAMMING_GUIDE.html` - Print-ready HTML version

### 2. **Quick Reference**
- `CHEAT_SHEET.md` - Condensed 4-page reference
- `CHEAT_SHEET.html` - Print-ready cheat sheet

### 3. **Supporting Files**
- `print-style.css` - Print-optimized styling
- `generate_pdf.sh` - PDF generation script

---

## 🖨️ How to Print

### Option 1: Print from Browser (Easiest)

**For Full Guide:**
1. Open `COMPETITIVE_PROGRAMMING_GUIDE.html` in your browser
2. Press `Ctrl+P` (or `Cmd+P` on Mac)
3. Settings:
   - **Destination:** Save as PDF
   - **Layout:** Portrait
   - **Margins:** Default
   - **Scale:** 100%
   - ✅ **Background graphics:** ON (for code highlighting)
4. Save as PDF

**For Cheat Sheet:**
1. Open `CHEAT_SHEET.html` in your browser
2. Press `Ctrl+P`
3. Same settings as above
4. Should be ~4 pages

### Option 2: Using wkhtmltopdf (Better Quality)

Install the tool:
```bash
sudo apt-get install wkhtmltopdf
```

Generate PDFs:
```bash
# Full guide
wkhtmltopdf COMPETITIVE_PROGRAMMING_GUIDE.html COMPETITIVE_PROGRAMMING_GUIDE.pdf

# Cheat sheet
wkhtmltopdf CHEAT_SHEET.html CHEAT_SHEET.pdf
```

### Option 3: Using the Script (Requires XeLaTeX)

Install XeLaTeX:
```bash
sudo apt-get install texlive-xetex
```

Run the script:
```bash
./generate_pdf.sh
```

---

## 📋 What to Print

### For Interviews:
**Print the Cheat Sheet** (4 pages)
- Quick pattern recognition
- Code templates
- STL reference
- Perfect for desk reference during practice

### For Deep Study:
**Print the Full Guide** (40+ pages)
- Complete explanations
- All 15 patterns with examples
- Interview strategy
- Communication framework

### Pro Tip:
Print cheat sheet **double-sided** on 2 sheets → perfect quick reference card!

---

## 🎨 Customization

Edit `print-style.css` to change:
- Font size (currently 9pt for full guide)
- Margins (currently 0.75in)
- Code highlighting colors
- Page break behavior

Example - make font bigger:
```css
body {
  font-size: 11pt;  /* Change from 9pt */
}
```

---

## 📱 Mobile Version

The HTML files work great on mobile too! Open in browser for:
- Searchable reference
- Clickable table of contents
- Syntax-highlighted code

---

## ✅ Print Checklist

Before printing:

- [ ] Open HTML file in browser first to preview
- [ ] Check page count (Full: ~40, Cheat: ~4)
- [ ] Verify code blocks fit without cutting off
- [ ] Enable background graphics for code highlighting
- [ ] Use "Save as PDF" first to check output
- [ ] Consider double-sided printing to save paper

---

## 💡 Usage Tips

**Full Guide:**
- Print and put in a binder
- Tab major sections for quick lookup
- Highlight patterns you struggle with
- Add your own notes in margins

**Cheat Sheet:**
- Laminate for durability
- Keep at your desk during practice
- Reference during mock interviews
- Print multiple copies (one for home, one for office)

---

## 🔧 Troubleshooting

**Code blocks cut off?**
- Reduce font size in CSS
- Change to landscape orientation
- Use wkhtmltopdf with `--enable-smart-shrinking`

**Page count too high?**
- Use cheat sheet instead
- Print specific sections only
- Reduce margins in CSS

**Fonts look weird?**
- Make sure DejaVu fonts are installed
- Or change to Arial/Times in CSS

**Links don't work in PDF?**
- That's normal for browser PDFs
- Use wkhtmltopdf for clickable links

---

Happy printing! 🎉
