/*
    XalanStyleFrame.java

    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: August 1, 2002

    Copyright (C) 2002 Tony Addyman

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.
   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
   
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
 */

package xslt.debugger.xalan;

import org.apache.xpath.VariableStack;
import org.apache.xalan.templates.ElemVariable;
import org.apache.xalan.templates.ElemParam;
import org.apache.xalan.templates.ElemTemplateElement;
import org.apache.xalan.templates.ElemTemplate;
import org.apache.xalan.templates.StylesheetRoot;
import org.apache.xalan.templates.Constants;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Vector;
import xslt.debugger.Manager;
import xslt.debugger.StyleFrame;
import xslt.debugger.Variable;

public class XalanStyleFrame extends StyleFrame
{
  VariableStack varStack;
  ElemTemplateElement element;
  StylesheetRoot stylesheetRoot;
  XalanTraceListener traceListener;

  public XalanStyleFrame(StylesheetRoot stylesheetRoot,
			 VariableStack varStack,
                         ElemTemplateElement element,
                         String name,
                         String filename,
                         int line,
                         int column,
                         Manager manager,
			 XalanTraceListener traceListener)

  {
    super(name, filename, line, column, manager);
    this.varStack = varStack;
    this.element = element;
    this.stylesheetRoot = stylesheetRoot;
    isTemplate = element instanceof ElemTemplate;
    this.traceListener = traceListener;
  }

  /**
   * Get the local variables of this style frame.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getLocalVariables()
  {
    ElemVariable variable;
    String name;
    Variable xvar;
    if (localVariables == null) {
      localVariables = new ArrayList();
      Vector variables = traceListener.getLocalVariables();
      for (Enumeration e = variables.elements(); e.hasMoreElements();) {
	  variable = (ElemVariable)e.nextElement();
	  name = variable.getName().getLocalName();
	  xvar = new XalanVariable(name, varStack, variable, this);
	  localVariables.add(xvar);
      }
    }
    return localVariables;
  }

  /**
   * Get the value of element.
   * @return value of element.
   */
  public ElemTemplateElement getElement()
  {
    return element;
  }

}
