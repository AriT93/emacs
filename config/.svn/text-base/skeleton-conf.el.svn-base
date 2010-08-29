(message "loading skeleton-conf")
(setq abbrev-mode 1)
(require 'csharp-mode)
(require 'skeleton)
(require 'sgml-mode)
(define-skeleton html
  "html-skeleton"
  nil
"<html>"\n
"<body>"\n
\n
"</body>"\n
"</html>"
'(indent-region (point-min) (point-max))
'(goto-char (point-max))
'(forward-line -2)
'(indent-according-to-mode))


(define-skeleton xhtml-trans-skeleton
      "Inserts a skeletal XHTML file with the DOCTYPE declaration
    for the XHTML 1.0 Transitional DTD"
      nil
      "<?xml version=\"1.0\""
      (if buffer-file-coding-system
          (concat " encoding=\""
                  (setq v1
                        (symbol-name
                         (coding-system-get buffer-file-coding-system
                                        'mime-charset))) "\""))
      "?>\n"
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
      "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
      "<head>\n"
      (if buffer-file-coding-system
            (concat
             "<meta http-equiv=\"Content-type\" content=\"text/html; charset="
             v1 "\" />\n"))
      "<meta name=\"Author\" content=\""
      (skeleton-read "Enter author: ") "\" />\n"
      "<title>" (skeleton-read "Enter title: ") "</title>\n"
      "</head>\n"
      "<body>\n"
      "\n"
      "</body>\n"
      "</html>"
      '(indent-region (point-min) (point-max))
      '(goto-char (point-max))
      '(forward-line -2)
      '(indent-according-to-mode))

(define-abbrev csharp-mode-abbrev-table "tcf"
  "" 'c-sharp-try-catch-finally)
(define-abbrev csharp-mode-abbrev-table "tc"
  "" 'c-sharp-try-catch)
(define-abbrev csharp-mode-abbrev-table "nc"
  "" 'arit93/csharp-class)
(define-abbrev csharp-mode-abbrev-table "ncaspx"
  "" 'arit93/asp-csharp-class)
(define-abbrev csharp-mode-abbrev-table "mthd"
  "" 'arit93/csharp-method)
(define-abbrev csharp-mode-abbrev-table "mfunc"
  "" 'arit93/csharp-main)
(define-abbrev html-mode-abbrev-table "naspx"
  "" 'arit93/asp-skeleton)
(define-abbrev csharp-mode-abbrev-table "tb"
  "" 'arit93/csharp-asp-textbox)
(define-abbrev csharp-mode-abbrev-table "lbl"
  "" 'arit93/csharp-asp-label)
(define-abbrev html-mode-abbrev-table "atb"
  "" 'arit93/asp-textbox)
(define-abbrev html-mode-abbrev-table "albl"
  "" 'arit93/asp-label)
(define-abbrev csharp-mode-abbrev-table "cbtn"
  "" 'arit93/csharp-asp-button)
(define-abbrev html-mode-abbrev-table "abtn"
  "" 'arit93/asp-button)
(define-abbrev csharp-mode-abbrev-table "hndlr"
  "" 'arit93/csharp-add-handler)
(define-abbrev csharp-mode-abbrev-table "hndel"
  "" 'arit93/csharp-add-handler-method)
(define-abbrev html-mode-abbrev-table "addl"
  "" 'arit93/asp-dropdownlist)
(define-abbrev csharp-mode-abbrev-table "cddl"
  "" 'arit93/csharp-asp-dropdownlist)
(define-abbrev html-mode-abbrev-table "addli"
  "" 'arit93/asp-dropdownlistitem)
(define-abbrev html-mode-abbrev-table "ttip"
  "" 'arit93/asp-tooltip)
(define-abbrev csharp-mode-abbrev-table "cprop"
  "" 'arit93/csharp-property)

(define-skeleton arit93/csharp-add-handler
  "instert an event handler"
  nil
  "this."(skeleton-read "Object: ")"."(skeleton-read "Method: ")
  " += new System.EventHandler(this."(skeleton-read "Delegate: ")");"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-add-handler-method
  "insert the stub for a handler delegate"
  nil
  "private void "(skeleton-read "Delegate Name ")
  "(object sender, System.EventArgs e)"\n
  "{"\n
  \n
  "}"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))


(define-skeleton arit93/csharp-asp-textbox
  "insert a declaration for a textbox"
  nil
  "protected System.Web.UI.WebControls.TextBox "
  (skeleton-read "Name: ")";"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-asp-label
  "insert a declaration for a label"
  nil
  "protected System.Web.UI.WebControls.Label "
  (skeleton-read "Name: ")";"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-asp-dropdownlist
  "insert a declaration for a dropdownlist"
  nil
  "protected System.Web.UI.WebControls.DropDownList "
  (skeleton-read "Name: ")";"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/asp-dropdownlist
  "insert a declaration for a DropDownList"
  nil
  "<asp:DropDownList id=\""
  (skeleton-read "Name: ")"\" runat=\"server\">"\n
  "</asp:DropDownList>"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/asp-dropdownlistitem
  "insert a declaration for a DropDownListitem"
  nil
  "<asp:ListItem Value=\""
  (skeleton-read "Value: ")"\">"
  (skeleton-read "Text: ")
  "</asp:ListItem>"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))


(define-skeleton arit93/asp-textbox
  "insert a declaration for a textbox"
  nil
  "<asp:textbox id=\""
  (skeleton-read "Name: ")"\" runat=\"server\">"
  "</asp:textbox>"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-asp-button
  "insert a declaration for a button"
  nil
  "protected System.Web.UI.WebControls.Button "
  (skeleton-read "Name: ")";"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/asp-button
  "insert a button"
  nil
  "<asp:button id=\""
  (skeleton-read "Name: ") "\" runat=\"server\" Text=\""
  (skeleton-read "Text: ") "\"></asp:button>"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))


(define-skeleton arit93/asp-label
  "insert a declaration for a label"
  nil
  "<asp:label id=\""
  (skeleton-read "Name: ")"\" runat=\"server\" AssociatedControlID=\""
  (skeleton-read "Associated Control: ") "\">"
  (skeleton-read "Text: ") "</asp:label>"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-main
  "insert a Main method"
  nil
  "[STAThread]"\n
  "static void Main(string[] args)"\n
  "{"\n
  _ \n
  "}"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-property
  "insert a new property"
  nil
  >"public "
  (skeleton-read "Type: ")" "
  (skeleton-read "Name: ")"{get{} set{}}"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-method
  "insert a new method"
  nil
  > (if (y-or-n-p "Public Method? ")
	"public "
	"private ")
  (skeleton-read "Return type: ") " " (skeleton-read "Method Name: ") "( " _ ")"\n
  "{"\n
  \n
  "}"\n
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton c-sharp-try-catch-finally
  "Insert a try catch block"
  nil
  > "try"\n
  "{"\n
  _ \n
  "}"\n
  "catch(Exception e)"\n
  "{"\n
  "}"\n
  "finally"\n
  "{"\n
  "}"\n
  '(save-excursion)
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/csharp-class
  "Inserts the basic csharp contents"
  nil
  "using System;"\n
  "using System.IO;"\n
  "using System.Data;"\n
  \n
  "namespace " (skeleton-read "Enter Namespace: ") "{"\n
  "/// <summary>\n"
  "/// Description Goes Here\n"
  "/// </summary>\n"
  "public class " (skeleton-read "Enter Class Name: ")\n
  "{"\n
  _ \n
  "}"\n
  "}"\n
'(save-excursion)
'(indent-region (point-min) (point-max))
'(goto-char (point-max))
'(indent-according-to-mode))

(define-skeleton arit93/asp-csharp-class
  "Inserts the basic csharp contents"
  nil
  "using System;\n"
"using System.Collections;\n"
"using System.ComponentModel;\n"
"using System.Data;\n"
"using System.Drawing;\n"
"using System.Web;\n"
"using System.Web.SessionState;\n"
"using System.Web.UI;\n"
"using System.Web.UI.WebControls;\n"
"using System.Web.UI.HtmlControls;\n"
  \n
  "public class " (skeleton-read "Enter Class Name: ") " : System.Web.UI.Page\n"
  "{"\n
  "	private void Page_Load(object sender, System.EventArgs e)\n"
  "{\n"
  "// Put user code to initialize the page here\n"
  "}\n"
  "\n"
  "#region Web Form Designer generated code\n"
  "override protected void OnInit(EventArgs e)\n"
  "{\n"
  "  //\n"
  "  // CODEGEN: This call is required by the ASP.NET Web Form Designer.\n"
  "  //\n"
  "  InitializeComponent();\n"
  "  base.OnInit(e);\n"
  "  }\n"
  "  \n"
  "  /// <summary>\n"
  "  /// Required method for Designer support - do not modify\n"
  "  /// the contents of this method with the code editor.\n"
  "  /// </summary>\n"
  "  private void InitializeComponent()\n"
  "  {    \n"
  "  this.Load += new System.EventHandler(this.Page_Load);\n"
  "  }\n"
  "  #endregion\n"
    _ \n
  "}"\n
  '(save-excursion)
  '(indent-region (point-min) (point-max))
  '(goto-char (point-max))
  '(indent-according-to-mode))

(define-skeleton c-sharp-try-catch
  "Insert a try catch block"
  nil
  > "try"\n
  "{"\n
  _ \n
  "}"\n
  "catch(Exception e)"\n
  "{"\n
  "}"\n
  '(save-excursion)
  '(indent-region (point-min) (point-max))
  '(indent-according-to-mode))

(define-skeleton arit93/asp-skeleton
      "Inserts a skeletal XHTML file with the DOCTYPE declaration
    for the XHTML 1.0 Transitional DTD"
      nil
	  "<%@ Page Language=\"C#\" AutoEventWireup=\"true\" CodeFile=\""
	  (concat ""(setq clname (skeleton-read "Class Name: ") )".aspx.cs")
	  (concat "\" Inherits=\"" clname "\"%>\n")
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
      "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
      "<head runat=\"server\">\n"
      (if buffer-file-coding-system
		  (concat
		   "<meta http-equiv=\"Content-type\" content=\"text/html; charset="
		                     (setq v1
                        (symbol-name
                         (coding-system-get buffer-file-coding-system
                                        'mime-charset))) "\" />\n"))
	  "<meta content=\"Emacs 22.0.50.1\" name=\"GENERATOR\">\n"
	  "<meta content=\"C#\" name=\"CODE_LANGUAGE\">\n"
	  "<meta content=\"JavaScript\" name=\"vs_defaultClientScript\">\n"
	  "<meta content=\"http://schemas.microsoft.com/intellisense/ie5\" name=\"vs_targetSchema\">\n"
      "<title>" clname "</title>\n"
      "</head>\n"
      "<body>\n"
      "<form id=\"form1\" runat=\"server\">"
	  "\n"
	  "</form>\n"
      "</body>\n"
      "</html>"
      '(indent-region (point-min) (point-max))
      '(goto-char (point-max))
      '(forward-line -2)
      '(indent-according-to-mode))

(define-skeleton arit93/asp-tooltip
  "Inserts a boxover tooltip using the Title attribute.  This requires boxover.js"
  nil
  "Title=\"header=["
  (skeleton-read "Title: ")
  "] body=["
  (skeleton-read "Body: ")
  "] fade=[on]\""
)


(provide 'skeleton-conf)