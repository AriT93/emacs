/*
    XalanTraceListener.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: April 26, 2001

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

import java.io.PrintWriter;
import java.util.Stack;
import java.util.Vector;
//import org.apache.xalan.templates.ElemTemplateElement;
//import org.apache.xalan.templates.ElemVariable;
import org.apache.xalan.trace.GenerateEvent;
import org.apache.xalan.trace.SelectionEvent;
import org.apache.xalan.trace.EndSelectionEvent;
import org.apache.xalan.trace.TraceListenerEx2;
import org.apache.xalan.trace.TracerEvent;
import org.apache.xml.dtm.ref.DTMNodeProxy;
import org.w3c.dom.Node;
import org.apache.xpath.XPath;
import org.apache.xpath.objects.XNodeSet;
import javax.xml.transform.SourceLocator;
import xslt.debugger.Manager;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.SourceFrame;
import xslt.debugger.StyleFrame;

public class XalanTraceListener implements TraceListenerEx2
{
  public final static int SOURCE = 0;
  public final static int STYLE = 1;

  Manager manager;
  XSLTDebugger debugger;
  String currentFilename = null;
  int currentLine = -1;
  int currentColumn = -1;
  int visitCount = 1;
  SourceFrame sourceFrameToStop = null;
  StyleFrame styleFrameToStop = null;
  String previousFilename = null; // work around Xalan bug?
  Vector localVariables = null;
  Stack varSize = null;

  public XalanTraceListener(XSLTDebugger debugger) {
    this.debugger = debugger;
    manager = debugger.getManager();
    currentFilename = debugger.getStylesheetId();
    localVariables = new Vector();
    varSize = new Stack();
  }

  public void generated(GenerateEvent ev) {
    // ignored
  }

  public synchronized void debuggerStopped(int elementType,
					   String filename,
					   int line,
					   int column,
					   String message) {
    if (filename.equals(currentFilename) && (line == currentLine)) {
      visitCount++;
    } else {
      visitCount = 1;
    }

    manager.debuggerStopped(filename, line, column, visitCount, message);

    currentFilename = filename;
    currentLine = line;
    currentColumn = column;
    switch (debugger.getAction()) {
    case AbstractXSLTDebugger.DO_NEXT:
      if (message.startsWith("leaving")) {
        // We were invoked when exiting from a frame. We want to
        // continue and enter in the next element or stop again when
        // we exit from the next element.
        debugger.setAction(AbstractXSLTDebugger.DO_DEFERRED_STOP);
      }
      break;

    case AbstractXSLTDebugger.DO_FINISH:
      // Find the xsl:template instruction we are in currently, and
      // setup styleFrameToStop at this one.
      styleFrameToStop = null;
      Stack styleFrames = manager.getStyleFrames();
      for (int i = styleFrames.size() - 1; i >= 0; i--) {
        StyleFrame frame = (StyleFrame)styleFrames.get(i);
        if (frame.isTemplate()) {
          styleFrameToStop = frame;
          break;
        }
      }
      break;

    default:
        if (elementType == SOURCE)
          sourceFrameToStop = manager.peekSourceFrame();
        else if (elementType == STYLE)
          styleFrameToStop = manager.peekStyleFrame();
        break;
    }
  }

  public void selected(SelectionEvent ev)
  {
    String name = null;
    debugger.checkRequestToStop();
    String filename = "";
    int line = -1;
    int column = -1;
    Node sourceNode = ev.m_sourceNode;
    name = sourceNode.getNodeName();
    if (name == null) name = "";

    int nodeHandler = ((DTMNodeProxy)sourceNode).getDTMNodeNumber();
    SourceLocator locator = ((DTMNodeProxy)sourceNode).getDTM()
        .getSourceLocatorFor(nodeHandler);
    if (locator != null) {
      filename = locator.getSystemId();
      if (filename == null) filename = "";
      line = locator.getLineNumber();
      column = locator.getColumnNumber();
    }

    String elementName = ev.m_styleNode.getNodeName();
    SourceFrame frame =
      new SourceFrame(name, filename, line, column, manager);
    manager.pushSourceFrame(frame);

    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(SOURCE, filename, line, column, "entering: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(SOURCE, filename, line, column,
                          "entering: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
        debuggerStopped(SOURCE, filename, line, column, "entering: " + name);
        break;
        
      default:
        break;
      }
    }
    if (!elementName.equals("for-each") && !elementName.equals("apply-templates"))
      manager.popSourceFrame();
  }

  public void selectEnd(EndSelectionEvent ev)
    throws javax.xml.transform.TransformerException
  {
    SourceFrame frame = null;
    String name = null;
    name = ev.m_sourceNode.getNodeName();
    if (name == null) name = "";

    String elementName = ev.m_styleNode.getNodeName();
    boolean showFrame = elementName.equals("for-each") ||
      elementName.equals("apply-templates");
    if (showFrame) {
      frame = manager.peekSourceFrame();
      ((SourceFrame)frame).setIsExiting();
    }

    debugger.checkRequestToStop();

    String filename = "";
    int line = -1;
    int column = -1;
    Node sourceNode = ev.m_sourceNode;
    int nodeHandler = ((DTMNodeProxy)sourceNode).getDTMNodeNumber();
    SourceLocator locator = ((DTMNodeProxy)sourceNode).getDTM()
        .getSourceLocatorFor(nodeHandler);
    if (locator != null) {
      filename = locator.getSystemId();
      if (filename == null) filename = "";  // defensive programming
      line = locator.getLineNumber();
      column = locator.getColumnNumber();
    }

    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(SOURCE, filename, line, column, "leaving: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
	  debuggerStopped(SOURCE, filename, line, column, "leaving: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_NEXT:
        if (sourceFrameToStop != null && sourceFrameToStop == frame) {
          // We reached the end of the element after which we have to
          // stop. Set the action to STEP so that we stop right after this
          // node and continue
	  debuggerStopped(SOURCE, filename, line, column, "leaving: " + name);
	  sourceFrameToStop = null;
        }
        break;

      case AbstractXSLTDebugger.DO_FINISH:
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
	debuggerStopped(SOURCE, filename, line, column, "leaving: " + name);
        break;
        
      }
    }
    if (showFrame) manager.popSourceFrame();
  }

  public void trace(TracerEvent ev)
  {
    debugger.checkRequestToStop();

    String name = ev.m_styleNode.getNodeName();
    String filename = ev.m_styleNode.getSystemId();
    if (filename == null) {
      filename = currentFilename;
    } else {
      previousFilename = filename;
    }
    int line = ev.m_styleNode.getLineNumber();
    int column = ev.m_styleNode.getColumnNumber();

    StyleFrame styleFrame
      = new XalanStyleFrame(debugger.getStylesheet(), debugger.getVarStack(),
			    ev.m_styleNode,
			    name, filename, line, column, manager, this);
    manager.pushStyleFrame(styleFrame);
    varSize.push(new Integer(localVariables.size()));
    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(STYLE, filename, line, column, "entering: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(STYLE, filename, line, column, "entering: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
        debuggerStopped(STYLE, filename, line, column, "entering: " + name);
        break;
        
      default:
        break;
      }
    }
  }

  public void traceEnd(TracerEvent ev)
  {
    StyleFrame frame = manager.peekStyleFrame();

    String name = ev.m_styleNode.getNodeName();
    // Apparent Xalan bug - no traceEnd events from choose
    // Pop the choose styleframe automatically
    if (!name.equals("choose") && frame.getName().equals("choose")) {
      manager.popStyleFrame();
      frame = manager.peekStyleFrame();
    }
    ((StyleFrame)frame).setIsExiting();
    String filename = ev.m_styleNode.getSystemId();
    if (filename == null) {
      filename = previousFilename;
    } else {
      previousFilename = filename;
    }
    int line = ev.m_styleNode.getLineNumber();
    int column = ev.m_styleNode.getColumnNumber();
    debugger.checkRequestToStop();
    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(STYLE, filename, line, column, "leaving: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
	  debuggerStopped(STYLE, filename, line, column, "leaving: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_NEXT:
        if (styleFrameToStop != null && styleFrameToStop == frame) {
          // We reached the end of the element after which we have to
          // stop. Set the action to STEP so that we stop right after this
          // node and continue
	  debuggerStopped(STYLE, filename, line, column, "leaving: " + name);
	  styleFrameToStop = null;
        }
        break;

      case AbstractXSLTDebugger.DO_FINISH:
        if (styleFrameToStop != null && styleFrameToStop == frame)
	  debuggerStopped(STYLE, filename, line, column, "leaving: " + name);
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
	debuggerStopped(STYLE, filename, line, column, "leaving: " + name);
        break;
        
      }
    }
    manager.popStyleFrame();
    localVariables.setSize(((Integer)varSize.pop()).intValue());
    if (name.equals("param") || name.equals("variable")) {
	localVariables.addElement(ev.m_styleNode);
    }
  }

    public Vector getLocalVariables() {
	return localVariables;
    }

}
