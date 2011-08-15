/*
    XSLTDebugger.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March 6, 2001

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

import com.icl.saxon.Controller;
import com.icl.saxon.ExtendedInputSource;
import com.icl.saxon.FeatureKeys;
import com.icl.saxon.ParameterSet;
import com.icl.saxon.StyleSheet;
import com.icl.saxon.output.Emitter;
import com.icl.saxon.output.MessageEmitter;
import com.icl.saxon.style.StyleElement;
import com.icl.saxon.trace.TraceListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.lang.Runnable;
import java.util.ArrayList;
import java.util.Stack;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Manager;
import xslt.debugger.Observer;
import xslt.debugger.cmdline.EmacsObserver;
import xslt.debugger.SAXParserErrorHandler;

public class XSLTDebugger extends AbstractXSLTDebugger
{
  TransformerFactory tFactory = null;
  
  public XSLTDebugger() {}

  public TransformerFactory getTransformerFactory(boolean forDebug)
  {
    if (tFactory == null)
      tFactory = new com.icl.saxon.TransformerFactoryImpl();

    return tFactory;
  }
  
  /**
   * Prepares the transformer for debugging.
   *
   * If the observer is an EmacsObserver, changes the Saxon
   * <code>Emitter</code>, used to output <code>xsl:message</code>, to
   * <code>EmacsEmitter</code>.
   */
  public void prepareTransformerForDebugging(Transformer transformer,
                                             boolean forDebug)
  {
    Observer observer = manager.getObserver();
    if (observer instanceof EmacsObserver) {
      MessageEmitter emitter = new MessageEmitter();
      emitter.setWriter(new PrintWriter(manager.getMessageStream()));
      emitter.setOutputStream(manager.getMessageStream());
      ((Controller)transformer).setMessageEmitter(emitter);
    }

    if (forDebug) {
      Controller controller = (Controller)transformer;
      TraceListener traceListener = new SaxonTraceListener(this);
      controller.setTraceListener(traceListener);
      controller.setLineNumbering(true);
      // Use a Writer to ensure output is produced incrementally for the debugger
      // Should the encoding be a user-selectable option in emacs?
      try {
	result = new StreamResult(new OutputStreamWriter(manager.getOutputStream(), "UTF8"));
      } catch (Exception e) { }
    }
  }

  public SAXParserErrorHandler getSAXParserErrorHandler(Manager manager)
  {
    return new SaxonSAXParserErrorHandler(manager);
  }

  /**
   * Return the list of global variables.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getGlobalVariables()
  {
    // With Saxon we obtain the list of global variables by asking a
    // SaxonStyleFrame object for it. We need to find a StyleElement
    // in the style frames stack to be able to ask for global
    // variables.
    Stack styleFrames = manager.getStyleFrames();
    if (styleFrames == null || styleFrames.size() == 0)
      return null;
    
    SaxonStyleFrame styleFrame = (SaxonStyleFrame)styleFrames.get(0);
    if (styleFrame.getElement() instanceof StyleElement)
      return styleFrame.getGlobalVariables();
    else
      return null;
  }
}
