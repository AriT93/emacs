/*
    SaxonVariable.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 20, 2001

    Copyright (C) 2001 Ovidiu Predescu

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
import xslt.debugger.Variable;

public class SaxonVariable extends Variable
{
  public SaxonVariable(String name,
                       Context context,
                       StyleElement element,
                       int frameId,
                       StyleFrame styleFrame)
  {
    super(name, styleFrame);
    this.value = new SaxonValue(this, context, element, frameId);
  }
}
