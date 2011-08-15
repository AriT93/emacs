/*
    XSLTDebugger.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: April 10, 2001

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

package xslt.debugger.xalan;

import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.Templates;
import javax.xml.transform.Source;
import java.util.TooManyListenersException;
import java.util.Stack;
import java.util.ArrayList;
import java.util.Vector;
import java.lang.Runnable;
import java.io.FileOutputStream;
import java.io.File;
import java.io.OutputStreamWriter;

import xslt.debugger.Manager;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.TrAXErrorListener;
import xslt.debugger.SAXParserErrorHandler;
import xslt.debugger.Variable;

import org.apache.xalan.processor.TransformerFactoryImpl;
import org.apache.xalan.transformer.TransformerImpl;
import org.apache.xalan.transformer.XalanProperties;
import org.apache.xalan.trace.TraceManager;
import org.apache.xalan.trace.TraceListener;
import org.apache.xalan.trace.PrintTraceListener;
import org.apache.xalan.templates.StylesheetRoot;
import org.apache.xalan.templates.ElemVariable;
import org.apache.xalan.templates.Constants;
import org.apache.xpath.XPathContext;
import org.apache.xpath.VariableStack;

public class XSLTDebugger extends AbstractXSLTDebugger
{
  TransformerFactory tFactory = null;
  StylesheetRoot stylesheetRoot = null;
  TransformerImpl transformerImpl = null;
  ArrayList globalVariables = null;
  
  public XSLTDebugger() {}

  public TransformerFactory getTransformerFactory(boolean forDebug)
  {
    if (tFactory == null)
      tFactory = new org.apache.xalan.processor.TransformerFactoryImpl();
    return tFactory;
  }

  public void prepareTransformerForDebugging(Transformer transformer,
                                             boolean forDebug)
  {
    if (forDebug) {
      // Register a TraceListener with a TraceManager associated
      // with the TransformerImpl.
      TransformerFactoryImpl tfi = (TransformerFactoryImpl)tFactory;
      tfi.setAttribute(XalanProperties.SOURCE_LOCATION, Boolean.TRUE);
      XalanTraceListener traceListener = new XalanTraceListener(this);
      transformerImpl = (TransformerImpl)transformer;
      stylesheetRoot = transformerImpl.getStylesheet();
      TraceManager trMgr = transformerImpl.getTraceManager();
      try {
        trMgr.addTraceListener(traceListener);
      }
      catch (TooManyListenersException e) {
        manager.getObserver().caughtException(e);
      }
      // Use a Writer to ensure output is produced incrementally for the debugger
      // Should the encoding be a user-selectable option in emacs?
      try {
	result = new StreamResult(new OutputStreamWriter(manager.getOutputStream(), "UTF8"));
      } catch (Exception e) { }
    }
  }

  public SAXParserErrorHandler getSAXParserErrorHandler(Manager manager)
  {
    return new XalanSAXParserErrorHandler(manager);
  }
  

  /**
   * Get the global variables of the stylesheet.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getGlobalVariables()
  {
    getVariableNames();
    return globalVariables;
  }

  /**
   * <code>getVariableNames</code> collects the variable names from
   * Xalan and places them in the internal data structures for later
   * reuse.
   */
  protected void getVariableNames()
  {
    globalVariables = new ArrayList();
    Vector vars = stylesheetRoot.getVariablesAndParamsComposed();
    int i = vars.size();
    while (--i >= 0) {
      ElemVariable variable = (ElemVariable)vars.elementAt(i);
      int token = variable.getXSLToken();
      if(token == Constants.ELEMNAME_PARAMVARIABLE
	 || token == Constants.ELEMNAME_VARIABLE) {
	Variable xvar = new XalanVariable(variable.getName().getLocalName(),
					  getVarStack(), variable, null);
	if (variable.getIsTopLevel()) {
	  globalVariables.add(xvar);
	}
      }
    }
  }

  public StylesheetRoot getStylesheet()
  {
    return stylesheetRoot;
  }

  public VariableStack getVarStack()
  {
    return transformerImpl.getXPathContext().getVarStack();
  }

}
