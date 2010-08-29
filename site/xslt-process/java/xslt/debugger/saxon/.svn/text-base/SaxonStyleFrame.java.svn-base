/*
    SaxonStyleFrame.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March 20, 2001

    Copyright (C) 2001 Ovidiu Predescu
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

package xslt.debugger.saxon;

import com.icl.saxon.Context;
import com.icl.saxon.style.StyleElement;
import com.icl.saxon.style.XSLTemplate;
import java.util.ArrayList;
import java.util.Enumeration;
import xslt.debugger.Manager;
import xslt.debugger.StyleFrame;
import xslt.debugger.Variable;
import com.icl.saxon.om.Name;

public class SaxonStyleFrame extends StyleFrame
{
  Context context;
  StyleElement element;
  int frameId = -1;
  ArrayList globalVariables = null;
  
  public SaxonStyleFrame(Context context,
                         StyleElement element,
                         String name,
                         String filename,
                         int line,
                         int column,
                         Manager manager)
  {
    super(name, filename, line, column, manager);
    this.context = context;
    this.element = element;
    isTemplate = element instanceof XSLTemplate;
//     System.out.println("StyleFrame: context = " + context);
  }

  /**
   * Get the local variables of this style frame.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getLocalVariables()
  {
    if (localVariables == null)
      getVariableNames();

    return localVariables;
  }

  /**
   * Get the global variables of the stylesheet. The Saxon API
   * requires a StyleElement object to be able to get the global
   * variables, even though they are kept at the stylesheet level.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getGlobalVariables()
  {
    getVariableNames();
    return globalVariables;
  }

  /**
   * Get the value of element.
   * @return value of element.
   */
  public StyleElement getElement()
  {
    return element;
  }

  /**
   * <code>getVariableNames</code> collects the variable names from
   * Saxon and places them in the internal data structures for later
   * reuse.
   */
  protected void getVariableNames()
  {
    // First initialize the frameId
    if (frameId == -1)
      frameId = context.getBindery().getFrameId();

    localVariables = new ArrayList();
    globalVariables = new ArrayList();
    
    // Now get the local variables. Saxon's getVariableNames() method
    // returns an array of two Enumerations, the first one for global
    // variables, the second one for local variables.
    Enumeration[] e = element.getVariableNames();
    
    while (e[0].hasMoreElements()) {
      String name = extractName((String)e[0].nextElement());
      Variable variable
        = new SaxonVariable(name, context, element, frameId, this);
      globalVariables.add(variable);
    }

    while (e[1].hasMoreElements()) {
      String name = extractName((String)e[1].nextElement());
      Variable variable
        = new SaxonVariable(name, context, element, frameId, this);
      localVariables.add(variable);
    }
  }

  /**
   * Extract the name from the full name (which looks like URI^name).
   *
   * @param fullName a <code>String</code> value
   * @return a <code>String</code> value
   */
  protected String extractName(String fullName)
  {
    int index = fullName.lastIndexOf("^");

    if (index == -1)
      return fullName;
    else
      return fullName.substring(index + 1, fullName.length());
  }
}
