/*
    SaxonTraceListener.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March  6, 2001

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
import com.icl.saxon.NodeHandler;
import com.icl.saxon.om.NodeInfo;
import com.icl.saxon.style.StyleElement;
import com.icl.saxon.style.XSLApplyTemplates;
import com.icl.saxon.style.XSLAttribute;
import com.icl.saxon.style.XSLCallTemplate;
import com.icl.saxon.trace.TraceListener;
import java.util.Stack;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Manager;
import xslt.debugger.Observer;
import xslt.debugger.SourceFrame;
import xslt.debugger.StyleFrame;
import java.util.ArrayList;

public class SaxonTraceListener implements TraceListener
{
  public final static int SOURCE = 0;
  public final static int STYLE = 1;
  public final static int TOP_LEVEL = 2;

  XSLTDebugger debugger;
  Manager manager;
  String currentFilename = null;
  int currentLine = -1;
  int currentColumn = -1;
  int visitCount = 1;
  SourceFrame sourceFrameToStop = null;
  StyleFrame styleFrameToStop = null;

  public SaxonTraceListener (XSLTDebugger debugger)
  {
    this.debugger = debugger;
    manager = debugger.getManager();
  }

  public synchronized void debuggerStopped(NodeInfo element,
                                           boolean leaving,
                                           int elementType,
                                           String message)
  {
    String name = element.getDisplayName();
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = -1;

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
      if (leaving) {
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

  /**
   * Called upon entering a node.
   */
  public void enterNode(NodeInfo element,
                        Context context,
                        int elementType)
  {
    debugger.checkRequestToStop();

    String name = element.getDisplayName();
    if (name.equals("") && elementType == SOURCE) name = "#document"; // like Xalan
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = -1;

    if (elementType == SOURCE) {
      SourceFrame frame = new SourceFrame(name, filename, line, column,
                                          manager);
      manager.pushSourceFrame(frame);
    }
    else if (elementType == STYLE) {
      StyleFrame styleFrame
        = new SaxonStyleFrame(context, (StyleElement)element,
                              name, filename, line, column, manager);
      manager.pushStyleFrame(styleFrame);
    }

    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(element, false, elementType, "entering: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(element, false, elementType,
                          "entering: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
        debuggerStopped(element, false, elementType, "entering: " + name);
        break;
        
      default:
        break;
      }
    }
  }

  /**
   * Called after a node of the source tree got processed
   */
  public void leaveNode(NodeInfo element,
                        Context context,
                        int elementType)
  {
    Object frame = null;

    if (elementType == SOURCE) {
      frame = manager.peekSourceFrame();
      ((SourceFrame)frame).setIsExiting();
    }
    else if (elementType == STYLE) {
      frame = manager.peekStyleFrame();
      ((StyleFrame)frame).setIsExiting();
    }
    
    debugger.checkRequestToStop();

    if (element.getNodeType()!= NodeInfo.ELEMENT )
	return;

    String name = element.getDisplayName();
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = -1;

    if (manager.isBreakpoint(filename, line)) {
      debuggerStopped(element, true, elementType, "leaving: " + name);
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(element, true, elementType, "leaving: " + name);
        }
        break;

      case AbstractXSLTDebugger.DO_NEXT:
        if ((elementType == SOURCE
             && sourceFrameToStop != null && sourceFrameToStop == frame)
            || (elementType == STYLE
                && styleFrameToStop != null && styleFrameToStop == frame)) {
          // We reached the end of the element after which we have to
          // stop. Set the action to STEP so that we stop right after this
          // node and continue
          debuggerStopped(element, true, elementType, "leaving: " + name);
          if (elementType == SOURCE)
            sourceFrameToStop = null;
          else if (elementType == STYLE)
            styleFrameToStop = null;
        }
        break;

      case AbstractXSLTDebugger.DO_FINISH:
        if (elementType == STYLE
            && styleFrameToStop != null && styleFrameToStop == frame)
          debuggerStopped(element, true, elementType, "leaving: " + name);
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
        debuggerStopped(element, true, elementType, "leaving: " + name);
        break;
        
      }
    }

    if (elementType == SOURCE)
      manager.popSourceFrame();
    else if (elementType == STYLE)
      manager.popStyleFrame();
  }

  /**
   * Called at the start of processing
   */
  public void open()
  {
    // Reset the state variables
    currentFilename = null;
    currentLine = -1;
    currentColumn = -1;
    sourceFrameToStop = null;
    styleFrameToStop = null;
  }

  /**
   * Called at end
   */
  public void close() {}

  /**
   * Called for all top level elements
   */
  public void toplevel(NodeInfo element)
  {
    enterNode(element, null, TOP_LEVEL);
  }

  /**
   * Called when a node of the source tree gets processed
   */
  public void enterSource(NodeHandler handler, Context context)
  {
    NodeInfo element = (NodeInfo)context.getContextNode();
    enterNode(element, context, SOURCE);
  }

  /**
   * Called after a node of the source tree got processed
   */
  public void leaveSource(NodeHandler handler, Context context)
  {
    NodeInfo element = (NodeInfo)context.getContextNode();
    leaveNode(element, context, SOURCE);
  }

  /**
   * Called when an element of the stylesheet gets processed
   */
  public void enter(NodeInfo element, Context context)
  {
    if (!(element instanceof StyleElement))
      return;

    enterNode(element, context, STYLE);
  }

  /**
   * Called after an element of the stylesheet got processed
   */
  public void leave(NodeInfo element, Context context)
  {
    leaveNode(element, context, STYLE);
  }

  String getModeName(Context context)
  {
    int nameCode = context.getMode().getNameCode();

    if (nameCode == -1)
      return "*default*";
    else
      return context.getController().getNamePool().getDisplayName(nameCode);
  }
}
