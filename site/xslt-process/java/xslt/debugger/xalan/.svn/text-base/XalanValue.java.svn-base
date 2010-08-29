/*
    XalanValue.java

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

import org.apache.xpath.XPathContext;
import org.apache.xpath.VariableStack;
import org.apache.xpath.objects.XObject;
import org.apache.xpath.objects.XNodeSet;
import org.apache.xalan.templates.ElemVariable;
import javax.xml.transform.TransformerException;
import xslt.debugger.Manager;
import xslt.debugger.StyleFrame;
import xslt.debugger.Type;
import xslt.debugger.Variable;

public class XalanValue extends xslt.debugger.Value
{
  XalanVariable variable;
  VariableStack varStack;

  final int MAXIMUM_VALUE_SIZE = 100;

  public XalanValue(XalanVariable variable,
                    VariableStack varStack)
  {
    super();
    this.variable = variable;
    this.varStack = varStack;

    try {
      if (value == null) {
	XObject xvalue = null;
	ElemVariable elemVar = variable.getElemVariable();
	int index = elemVar.getIndex();
	int frame = varStack.getStackFrame();
	if (elemVar.getIsTopLevel()) {
	  xvalue = varStack.elementAt(index);
	} else {
	  xvalue = varStack.getLocalVariable(index, frame);
	}
        if (xvalue != null) {
	  int xalanType = xvalue.getType();
	  switch (xalanType) {
	  case XObject.CLASS_BOOLEAN:
	  case XObject.CLASS_NUMBER:
	  case XObject.CLASS_STRING:
	  case XObject.CLASS_UNKNOWN:
	    value = xvalue.toString();
	    break;
	  case XObject.CLASS_NODESET:
	    value = ((XNodeSet)xvalue).xstr().toString();
	    break;
	  case XObject.CLASS_UNRESOLVEDVARIABLE:
	      value = "";
	      break;
	  default:
	    value = "";
	    break;
	  }
	  // nodesets can generate very large string values
	  if (value.length() > MAXIMUM_VALUE_SIZE) {
	    value = value.substring(0, MAXIMUM_VALUE_SIZE) + "...";
	  }
	  type = new XalanType(xalanType);
	}
      }
    }
    catch (TransformerException e) {
      System.out.println("Got exception while trying to get value: " + e);
    }
  }

  public String getValue()
  {
    return value;
  }
}
