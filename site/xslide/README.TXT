This package contains an Emacs major mode for editing XSL stylesheets.
The current revision is 0.2.2, dated 4 August 2003.

The xslide home page is http://www.menteith.com/xslide/

xslide source code is also available at
http://www.sourceforge.net/projects/xslide/.

The xslide mailing list is xslide-list@lists.sourceforge.net.  See
https://lists.sourceforge.net/lists/listinfo/xslide-list/.

* MANIFEST

File                    Contents

README.TXT              The file you are reading
ChangeLog               Log of changes since xslide 0.2
Makefile                Makefile for byte-compiling 
NEWS                    Summary of what's new or changed about xslide
TODO                    List of sample things to do next
dot_emacs               Some things for your .emacs file
xslide.el               Base file for the XSL major mode
xslide-abbrev.el        Abbreviations for XSL mode
xslide-data.el          Data about XSL elements and formatting objects
xslide-font.el          Font lock keywords for XSL stylesheets
xslide-initial.xsl      Default initial stylesheet inserted in empty buffers
xslide-process.el       Procedures for processing XSL stylesheets

Note that make-regexp.el, which was used by earlier version of xslide,
is not required by xslide 0.2 or later.

Note that "xslide" is pronounced as one word, similar to "slide".  It
is not spelled out as "x-s-l-i-d-e".


* FEATURES

 - XSL customization group for setting some variables

 - Initial stylesheet inserted into empty XSL buffers;

 - "Template" menu for jumping to template rules, named templates, key
   declarations, and attribute-set declarations in the buffer;

 - `xsl-process' function that runs an XSL processor and collects the
   output;

 - Predefined command line templates and error regexps for Java and
   Windows executable versions of both XT and Saxon;

 - Font lock highlighting so that the important information stands
   out;

 - `xsl-complete' function for inserting element and attribute names;

 - `xsl-insert-tag' function for inserting matching start- and
    end-tags;

 - Automatic completion of end-tags;

 - Automatic indenting of elements with user-definable indentation
   step;

 - `xsl-if-to-choose' converts <xsl:if> to <xsl:choose> with
   nested <xsl:when>; and

 - Comprehensive abbreviations table to further ease typing.

xsl-mode was tested using Emacs 21.2.1 and XEmacs 21.4.


* BUG REPORTS/ENCHANCEMENTS

I would be glad to accept bug reports and, especially, enhancements.
Use `xsl-submit-bug-report' for bug reports.

Bug reports and enhancement requests can also be logged on the
SourceForge site.


* INSTALLATION

1. Unzip the distribution.

   This should extract the files listed in the manifest above.

   The files have DOS-style line breaks.  You may need to use "unzip
   -a" or "gunzip -a" to convert the line ends in the files to your
   local convention.

2. Copy the .el files to your emacs site-lisp directory (e.g.,
   /usr/local/lib/emacs/site-lisp) or some other directory that is on
   your Emacs load path.

3. Byte-compile the .el files, if you want.

   If you byte-compiled a previous version of the files, you must
   byte-compile the new files (or remove the old .elc files) so that
   autoload loads the correct versions.

   If there are any byte compiler warning, ignore them.  They probably
   can't be avoided with code that is run on multiple Emacs versions
   [1].

   There are two easy ways to byte-compile the .el files:

   a. Byte-compile from within Emacs:

      Load xslide.el (M-x load-file RET xslide.el) then byte
      compile the .el files using C-0 M-x byte-recompile-directory and
      supplying the path name of the directory containing the .el
      files.

   or

   b. Byte-compile using the provided Makefile:

      First, review the values of EMACS and SHELL in the Makefile.
      Second, run 'make' without arguments to byte-compile the files.      

4. Add the elisp code in dot_emacs to your .emacs file.  Uncomment any
   portions that you want to also use.

5. Restart Emacs (or, if it's easier for you, evaluate the code that
   you just added to your .emacs file).

6. When you visit a file with an extension of .xsl or .fo, xsl-mode
   will automatically be loaded.

7. To find out more about xsl-mode, type C-h m from within an XSL
   file.

8. To customize xsl-mode font-lock colours, etc., browse the `Xsl'
   Customize group after loading xsl-mode.


Notes:
[1] Originally noted by the VM mail reader by Kyle Jones.

-----
$Id: README.TXT,v 1.7 2003/08/04 21:24:09 tonygraham Exp $
