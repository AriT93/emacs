/*
    SaxonValue.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March 22, 2001

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


import com.icl.saxon.Binding;
import com.icl.saxon.Context;
import com.icl.saxon.expr.Value;
import com.icl.saxon.expr.XPathException;
import com.icl.saxon.om.NamePool;
import com.icl.saxon.style.StyleElement;
import xslt.debugger.Manager;
import xslt.debugger.StyleFrame;
import xslt.debugger.Type;
import xslt.debugger.Variable;
import com.icl.saxon.Controller;
import com.icl.saxon.Bindery;
import com.icl.saxon.expr.StaticContext;

public class SaxonValue extends xslt.debugger.Value
{
  SaxonVariable variable;
  Context context;
  StyleElement element;
  int frameId;
  final int MAXIMUM_VALUE_SIZE = 100;

  public SaxonValue(SaxonVariable variable,
                    Context context,
                    StyleElement element,
                    int frameId)
  {
    super();
    this.variable = variable;
    this.context = context;
    this.element = element;
    this.frameId = frameId;

    try {
      if (value == null) {
        Controller controller = context.getController();
        StaticContext staticContext = context.getStaticContext();
        Bindery bindery = context.getBindery();

        if (staticContext == null) {
          value = "";
          type = new Type(Type.ANY);
          return;
        }
        
        int fprint = staticContext.makeNameCode(variable.getName(), false);
//         System.out.println("fingerprint for name " + variable.getName()
//                            + " is " + fprint);
        
        Binding b = element.getVariableBinding(fprint);
//         System.out.println("binding is " + b);
        
        if (b != null) {
          Value v = bindery.getValue(b, frameId);
//           System.out.println("saxon value: " + v.asString());
          
          if (v != null) {
            value = v.asString();
	    // nodesets can generate very large string values
	    if (value.length() > MAXIMUM_VALUE_SIZE) {
	      value = value.substring(0, MAXIMUM_VALUE_SIZE) + "...";
	    }
            type = new SaxonType(v.getDataType());
          }
        }
      }
    }
    catch (XPathException e) {
      System.out.println("Got exception while trying to get value: " + e);
    }
  }
  
  public String getValue()
  {
    return value;
  }
}
