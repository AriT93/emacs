/*
    Manager.java

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

package xslt.debugger;

import java.util.Stack;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Collection;
import java.util.ArrayList;
import java.net.URL;
import java.net.MalformedURLException;
import java.lang.IndexOutOfBoundsException;
import java.lang.ClassNotFoundException;
import java.io.OutputStream;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.File;

import org.xml.sax.InputSource;
import org.apache.fop.messaging.MessageHandler;
import org.apache.fop.apps.FOPException;
import org.apache.fop.apps.Driver;

import org.apache.avalon.framework.logger.Logger;
import org.apache.avalon.framework.logger.ConsoleLogger;

import xslt.debugger.FOPLogger;
/**
 * <code>Manager</code> manages the breakpoints, and local and global
 * variables. It contains an {@link AbstractXSLTDebugger} instance,
 * which is this thread's interface with the real worker thread where
 * the XSLT processor runs.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 * @see Observer
 * @see AbstractXSLTDebugger
 */
public class Manager
{
  public final static boolean runningWindows
    = System.getProperties().getProperty("os.name")
      .toLowerCase().indexOf("windows") != -1;

  /**
   * <code>styleFrames</code> maintains the stack of frames in the
   * XSLT sheet traversed during the XSLT processing. These are
   * basically the xsl:templates traversed by the processor. The
   * element at index 0 is considered to be the top of the template,
   * or the global scope.
   */
  Stack styleFrames;

  /**
   * <code>sourceFrames</code> keeps the stack of frames in the source
   * XML document.
   */
  Stack sourceFrames;

  /**
   * <code>breakpoints</code> keeps an association between filename
   * and line number, and the corresponding <code>Breakpoint</code>
   * object.
   */
  HashMap breakpoints = new HashMap();

  /**
   * <code>observer</code> is the instance to be notified when the
   * internal state of the XSLT "program" has been modified.
   */
  Observer observer = null;

  /**
   * Mapping between processor name to an instance of
   * AbstractXSLTDebugger.
   */
  HashMap processors = new HashMap();

  /**
   * <code>debugger</code> is an actual instance of AbstractXSLTDebugger
   * wrapper for the real XSLT processor.
   *
   */
  AbstractXSLTDebugger debugger = null;

  /**
   * <code>outStream</code> specifies where the results of the XSLT
   * processing should be sent. By default this is
   * <code>System.out</code>.
   */
  protected OutputStream outStream = System.out;
  protected boolean closeOnFinish = false;

  /**
   * <code>messageStream</code> is the stream where messages generated
   * with xsl:message are sent.
   */
  protected OutputStream messageStream = System.out;

  /**
   * <code>fopLogLevel</code> specifies the level of logging to be reported
   * when FOP is run.
   */
  protected int fopLogLevel = FOPLogger.LEVEL_INFO;

  /**
   * <code>forDebug</code> indicates whether the XSLT processor should
   * be setup for debugging or not.
   */
  boolean forDebug = false;

  /**
   * The following variables are used to maintain info on what is the
   * information communicated to the GUI client (e.g. Emacs), the last
   * time the debugger stopped. This is used to minimize the
   * communication between the XSLT debugger and the GUI front-end, by
   * sending only the data that changed since last time.
   */
  Stack lastStyleFrames = null;
  Stack lastSourceFrames = null;
  ArrayList lastGlobalVariables = null;
  ArrayList lastLocalVariables = null;

  /**
   * The XML file name to process.
   */
  String xmlFilename = null;

  /**
   * The XSLT stylesheet to use to process the XML document.
   */
  String xslFilename = null;
  
  /**
   * The XSLT stylesheet parameters.
   */
  Hashtable transformParameters  = null;
  
  /**
   * Creates a new <code>Manager</code> instance.
   *
   */
  public Manager() {}

  /**
   * Creates a new  XSLT processor to be used for executing the
   * XSLT transformation. <code>processorName</code> is the name of the
   * XSLT processor.
   *
   * @param processorName - <code>String</code>: the name of the XSLT
   * processor
   * @see AbstractXSLTDebugger for more information on what is the
   * class structure of the debugger
   */
  public void setXSLTProcessorType(String processorName)
  {
    try {
      processorName = processorName.toLowerCase();
      debugger = (AbstractXSLTDebugger)processors.get(processorName);
      if (debugger == null) {
        Class debuggerClass = Class.forName("xslt.debugger."
                                            + processorName
                                            + ".XSLTDebugger");
        debugger = (AbstractXSLTDebugger)debuggerClass.newInstance();
        processors.put(processorName, debugger);
      }
    }
    catch (ClassNotFoundException e) {
      System.out.println("Cannot find the " + processorName
                         + " XSLT processor for debugging!\n" + e);
      observer.caughtException(e);
      System.exit(1);
    }
    catch (Exception e) {
      System.out.println("Cannot create instance of the " + processorName
                         + " XSLT processor!\n" + e);
      observer.caughtException(e);
      System.exit(1);
    }
      
    debugger.setManager(this);
    debugger.setProcessorName(processorName);
  }

  /**
   * <code>isBreakpoint</code> determines whether a breakpoint is set
   * at a particular location or not.
   *
   * @param filename the <code>String</code> file name
   * @param line the <code>int</code> line number
   * @return true if there's a breakpoint set at this location, false
   * otherwise
   */
  public boolean isBreakpoint(String filename, int line)
  {
    Breakpoint bkp = getBreakpointAt(filename, line);

    if (bkp != null)
      return bkp.isEnabled();
    else
      return false;
  }

  /**
   * Return a <code>Breakpoint</code> instance if there's a breakpoint
   * in the file name, at the line number indicated as arguments, null
   * otherwise.
   *
   * @param filename a <code>String</code> value, representing the
   * file name
   * @param line an <code>int</code> value, representing the line
   * number
   * @return a <code>Breakpoint</code> value or null if no breakpoint
   * is setup at the indicated location
   */
  public Breakpoint getBreakpointAt(String filename, int line)
  {
    if (runningWindows)
      filename = filename.toLowerCase();
    String key = filename + ":" + line;
    return (Breakpoint)breakpoints.get(key);
  }
  
  /**
   * Set breakpoint at <code>line</code> in
   * <code>filename</code>. If a breakpoint is already set at that
   * location, the request is ignored.
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void setBreakpoint (String filename, int line)
  {
    if (runningWindows)
      filename = filename.toLowerCase();
    String key = filename + ":" + line;
    Breakpoint breakpoint = new Breakpoint(filename, line, this);
    breakpoints.put(key, breakpoint);
  }

  /**
   * Remove breakpoint at location. The breakpoint is removed. Doesn't
   * do anything if there's no breakpoint setup at the indicated
   * location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void removeBreakpoint (String filename, int line)
  {
    if (runningWindows)
      filename = filename.toLowerCase();
    String key = filename + ":" + line;
    breakpoints.remove(key);
  }

  /**
   * Enable the breakpoint set up at
   * <code>filename</code>:<code>line</code>. Ignores the request if
   * no breakpoint exists at this location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void enableBreakpoint(String filename, int line)
  {
    Breakpoint breakpoint = getBreakpointAt(filename, line);
    if (breakpoint != null)
      breakpoint.setEnabled(true);
  }

  /**
   * Disable the breakpoint set up at
   * <code>filename</code>:<code>line</code>. Ignores the request if
   * no breakpoint exists at this location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void disableBreakpoint(String filename, int line)
  {
    Breakpoint breakpoint = getBreakpointAt(filename, line);
    if (breakpoint != null)
      breakpoint.setEnabled(false);
  }

  /**
   * Return the breakpoints setup in this debugger instance.
   * @return a <code>HashMap</code> with the breakpoints
   */
  public Collection getBreakpoints()
  {
    return breakpoints.values();
  }

  /**
   * Pushes a new source frame on the source frames stack.
   *
   * @param frame a <code>SourceFrame</code> value
   */
  public void pushSourceFrame(SourceFrame frame)
  {
    sourceFrames.push(frame);
  }

  /**
   * <code>peekSourceFrame</code> returns the topmost source frame on
   * the source frames stack.
   *
   * @return a <code>SourceFrame</code> value
   */
  public SourceFrame peekSourceFrame()
  {
    return sourceFrames.size() > 0 ? (SourceFrame)sourceFrames.peek() : null;
  }

  /**
   * <code>popSourceFrame</code> pops the topmost source frame from
   * the source frames stack.
   */
  public void popSourceFrame()
  {
    sourceFrames.pop();
  }

  /**
   * <code>newStyleFrame</code> pushes a StyleFrame instance on the
   * style frames stack and associates it with the corresponding
   * SourceFrame object.
   *
   * @param styleFrame a <code>StyleFrame</code> value
   */
  public void pushStyleFrame(StyleFrame styleFrame)
  {
    SourceFrame sourceFrame = peekSourceFrame();

    styleFrames.push(styleFrame);
    styleFrame.setSourceFrame(sourceFrame);
    if (sourceFrame != null)
      sourceFrame.setStyleFrame(styleFrame);
  }

  /**
   * <code>peekStyleFrame</code> returns the topmost StyleFrame
   * instance on the style frames stack.
   *
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame peekStyleFrame()
  {
    return styleFrames.size() > 0 ? (StyleFrame)styleFrames.peek() : null;
  }

  /**
   * Return the style frame specified by index in the style frames
   * stack.
   *
   * @param index an <code>int</code> value
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame peekStyleFrame(int index)
  {
    if (index == -1)
      index = styleFrames.size() - 1;
    
    if (index >= 0 && index < styleFrames.size())
      return (StyleFrame)styleFrames.get(index);
    return null;
  }

  /**
   * <code>popStyleFrame</code> pops the topmost object from the style
   * frames stack.
   */
  public void popStyleFrame()
  {
    styleFrames.pop();
  }

  /**
   * Returns the source frames stack.
   *
   * @return a <code>Stack</code> value
   */
  public Stack getSourceFrames()
  {
    return sourceFrames;
  }
  
  /**
   * Return the XSLT stack frames.
   * @return a <code>Stack</code> value
   */
  public Stack getStyleFrames()
  {
    return styleFrames;
  }

  /**
   * Set the XSLT transform parameters. If there are no such
   * parameters the parameter is null.
   * @param transformParameters a <code>Hashtable</code> value
   */
    public void setTransformParameters(Hashtable transformParameters)
  {
    this.transformParameters = transformParameters;
  }

  /**
   * <code>startXSLTProcessing</code> start a new XSLT processing. It
   * starts a new thread under the command line tool that will execute
   * the actual processing. This will run either in debug or non-debug
   * mode, depending on the value of <code>forDebug</code>.
   *
   * @param xmlFilename a <code>String</code> value
   * @param forDebug a <code>boolean</code> value
   * @exception InterruptedException if an error occurs
   */
  public synchronized void startXSLTProcessing(boolean forDebug)
    throws InterruptedException
  {
    lastStyleFrames = null;
    lastSourceFrames = null;
    lastGlobalVariables = null;
    lastLocalVariables = null;

    debugger.setXMLFilename(xmlFilename);
    debugger.setXSLTStylesheet(xslFilename);
    debugger.setTransformParameters(transformParameters);
    this.forDebug = forDebug;
    if (!debugger.isStarted()) {
      Thread worker = new Thread(debugger);
      worker.start();
    }
  }

  public void startDebugger()
    throws InterruptedException
  {
    sourceFrames = new Stack();
    styleFrames = new Stack();
    startXSLTProcessing(true);
  }

  public void startProcessor()
    throws InterruptedException
  {
    startXSLTProcessing(false);
  }

  public void convertToPDF(String outputFilename)
  {
    try {
      InputSource xmlSource = new InputSource(new URL(xmlFilename).toString());
      File file = new File(outputFilename);
      boolean created = file.createNewFile();
      file.deleteOnExit();
      FileOutputStream fileStream = new FileOutputStream(file);

      Driver driver = new Driver(xmlSource, fileStream);
      Logger logger = new FOPLogger(observer, getFopLogLevel());
      MessageHandler.setScreenLogger(logger);
      driver.setLogger(logger);
      MessageHandler.setOutputMethod(MessageHandler.EVENT);
      MessageHandler.addListener(new FOPMessageListener(observer));
      driver.setRenderer(Driver.RENDER_PDF);
      driver.setErrorDump(getFopLogLevel() == FOPLogger.LEVEL_DEBUG);
      driver.run();
      fileStream.flush();
      fileStream.close();
    } catch (Exception e) {
      observer.caughtException(e);
    }
    observer.processorFinished();
  }
  
  public void processorFinished()
  {
    if (closeOnFinish) {
      try {
        outStream.close();
      }
      catch (IOException e) {}
    }
  }
  
  /**
   * <code>debuggerStopped</code> is called by the debugger's trace
   * listener instance when it stops the XSLT processing.
   */
  public void debuggerStopped(String filename,
                              int line,
                              int column,
			      int count,
                              String message)
  {
    // Check the source frame stack and the style frame stack for
    // modifications
    if ((lastSourceFrames == null && sourceFrames != null)
        || !lastSourceFrames.equals(sourceFrames)) {
      lastSourceFrames = (Stack)sourceFrames.clone();
      observer.sourceStackChanged();
    }

    if ((lastStyleFrames == null && styleFrames != null)
        || !styleFrames.equals(lastStyleFrames)) {
      lastStyleFrames = (Stack)styleFrames.clone();
      observer.styleStackChanged();
      if (lastStyleFrames.size() > 0) {
        // Check whether the local variables have changed
        ArrayList newLocalVariables
          = ((StyleFrame)lastStyleFrames.pop()).getLocalVariables();
        if ((lastLocalVariables == null && newLocalVariables != null)
            || !lastLocalVariables.equals(newLocalVariables)) {
          lastLocalVariables = (ArrayList)newLocalVariables.clone();
          observer.localVariablesChanged(lastLocalVariables);
        }
      }
    }

    ArrayList newGlobalVariables = getDebugger().getGlobalVariables();
    if ((lastGlobalVariables == null && newGlobalVariables != null)
        || (newGlobalVariables != null
            && !newGlobalVariables.equals(lastGlobalVariables))) {
      lastGlobalVariables = newGlobalVariables;
      observer.globalVariablesChanged(lastGlobalVariables);      
    }

    debugger.debuggerStopped(filename, line, column, count, message);
  }
  
  /**
   * <code>getDebugger</code> returns the debugger instance.
   *
   * @return an <code>AbstractXSLTDebugger</code> value
   */
  public AbstractXSLTDebugger getDebugger()
  {
    return debugger;
  }

  /**
   * <code>setObserver</code> sets the observer instance to receive
   * notifications when interesting things happen.
   *
   * @param observer an <code>Observer</code> value
   */
  public void setObserver (Observer observer)
  {
    this.observer = observer;
  }
  
  /**
   * <code>getObserver</code> returns the Observer instance.
   *
   * @return an <code>Observer</code> value
   */
  public Observer getObserver()
  {
    return observer;
  }

  public void setOutputStream(OutputStream stream, boolean closeOnFinish)
  {
    outStream = stream;
    this.closeOnFinish = closeOnFinish;
  }

  public OutputStream getOutputStream()
  {
    return outStream;
  }

  public void setMessageStream(OutputStream stream)
  {
    messageStream = stream;
  }

  public OutputStream getMessageStream()
  {
    return messageStream;
  }

  public void setXMLFilename(String filename)
  {
    xmlFilename = filename;
  }

  public void setXSLTStylesheet(String filename)
  {
    xslFilename = filename;
  }

  public void setFopLogLevel(int logLevel) {
    this.fopLogLevel = logLevel;
  }

  public int getFopLogLevel() {
    return this.fopLogLevel;
  }
}
