/*
    Controller.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March  7, 2001

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

package xslt.debugger.cmdline;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.NoSuchMethodException;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Stack;
import java.util.Vector;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Breakpoint;
import xslt.debugger.Manager;
import xslt.debugger.Observer;
import xslt.debugger.SourceFrame;
import xslt.debugger.StyleFrame;
import xslt.debugger.Type;
import xslt.debugger.Utils;
import xslt.debugger.Value;
import xslt.debugger.Variable;
import xslt.debugger.FOPLogger;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;

/**
 * This is the public class used by the command line interface to
 * interact with the XSLT debugger.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 * @see xslt.debugger.Manager
 * @see xslt.debugger.AbstractXSLTDebugger
 */
public class Controller
{
  static final String breakpointUsage = "Usage: b filename line";
  static final String deleteBreakpointUsage = "Usage: d <breakpoint number>";
  static final String disableBreakpointUsage
    = "Usage: dis (<breakpoint number> | filename lineno)";
  static final String enableBreakpointUsage
    = "Usage: ena (<breakpoint number> | filename lineno)";
  static final String debugUsage = "Usage: debug -xml filename [-a | -xsl filename] [-o <output filename>] [-p n params]";
  static final String runUsage = "Usage: run -xml filename [-a | -xsl filename] [-o <output filename>] [-p n params]";
  static final String toPDFUsage = "Usage: toPDF -errorlevel -xml filename -o <output filename>";
  static final String setSourceFrameUsage = "Usage: sf <framenumber>";
  static final String setStyleFrameUsage = "Usage: xf <framenumber>";
  static final String printLocalVariableUsage = "Usage: pl <name>";
  static final String printGlobalVariableUsage = "Usage: pg <name>";
  static final String setParameterUsage = "Usage: set <parameter> <value>\n"
    + "\tPossible parameters are:\n"
    + "\t\tprocessor = (saxon|xalan)";
  static final String showParametersUsage = "Usage: show\n"
    + "    or show <parameter>";

  Manager manager = new Manager();
  AbstractXSLTDebugger debugger;
  HashMap commands = new HashMap();
  int currentSourceFrame = -1;
  int currentStyleFrame = -1;
  Observer observer = null;
  HashMap parameters = new HashMap();
  OutputStream outputStream = System.out;

  public Controller()
  {
    commands.put("h", getMethod("help"));
    commands.put("help", getMethod("help"));
    commands.put("b", getMethod("setBreakpoint"));
    commands.put("d", getMethod("deleteBreakpoint"));
    commands.put("dis", getMethod("disableBreakpoint"));
    commands.put("ena", getMethod("enableBreakpoint"));
    commands.put("lb", getMethod("listBreakpoints"));
    commands.put("debug", getMethod("debugXSLTProcessor"));
    commands.put("run", getMethod("runXSLTProcessor"));
    commands.put("toPDF", getMethod("convertToPDF"));
    commands.put("s", getMethod("doStep"));
    commands.put("n", getMethod("doNext"));
    commands.put("f", getMethod("doFinish"));
    commands.put("c", getMethod("continueProcessing"));
    commands.put("lv", getMethod("showLocalVariables"));
    commands.put("gv", getMethod("showGlobalVariables"));
    commands.put("pl", getMethod("printLocalVariable"));
    commands.put("pg", getMethod("printGlobalVariable"));
    commands.put("sf", getMethod("setCurrentSourceFrame"));
    commands.put("xf", getMethod("setCurrentStyleFrame"));
    commands.put("sbt", getMethod("showSourceFrames"));
    commands.put("xbt", getMethod("showStyleFrames"));
    commands.put("stop", getMethod("stopXSLTProcessing"));
    commands.put("q", getMethod("quit"));
    commands.put("set", getMethod("setParameter"));
    commands.put("show", getMethod("showParameters"));
  }

  public void createXSLTDebugger()
  {
    String processorName = (String)parameters.get("processor");
    manager.setXSLTProcessorType(processorName);
    debugger = manager.getDebugger();
  }

  public void help(Vector args)
  {
    System.out.println("Known commands:\n\n"
                       + "help or h\t shows this message\n"
                       + "b\t set breakpoint\n"
                       + "d\t delete breakpoint\n"
                       + "dis\t disable breakpoint\n"
                       + "ena\t enable breakpoint\n"
                       + "lb\t list breakpoints\n"
                       + "debug\t run the XSLT debugger\n"
                       + "run\t run the XSLT processor without debugging\n"
                       + "toPDF\t convert an XML FO document to PDF\n"
                       + "s\t step\n"
                       + "n\t next\n"
                       + "c\t continue\n"
                       + "lv\t show local variables\n"
                       + "gv\t show global variables\n"
                       + "pl\t print value of local variable\n"
                       + "pg\t print value of global variable\n"
                       + "sf\t set current source frame\n"
                       + "xf\t set current style frame\n"
                       + "sbt\t show source stack frames\n"
                       + "xbt\t show XSLT stack frames\n"
                       + "stop\t stop (useful with long processings)\n"
                       + "set\t set parameters (see help set)\n"
                       + "show\t show the values of settable parameters\n"
                       + "q\t quit\n");
  }

  public static void main(String[] args)
  {
    boolean useEmacs = false;
    String processor = "Saxon";

    for (int i = 0; i < args.length; i++) {
      String arg = (String)args[i];
      if (arg.equals("-emacs"))
        useEmacs = true;
      else if (arg.equalsIgnoreCase("-saxon"))
        processor = "Saxon";
      else if (arg.equalsIgnoreCase("-xalan"))
        processor = "Xalan";
    }

    Controller controller = new Controller();

    controller.parameters.put("processor", processor);

    Observer observer = null;
    if (useEmacs)
      observer = new EmacsObserver(controller);
    else
      observer = new CmdLineObserver(controller);

    controller.setObserver(observer);

    controller.createXSLTDebugger();
    observer.debuggerProcessStarted();
    controller.mainLoop();
  }

  public Method getMethod(String mthName)
  {
    Class[] args = { Vector.class };
    
    try {
      return this.getClass().getMethod(mthName, args);
    }
    catch (NoSuchMethodException e) {
      System.out.println("No such method: " + mthName + ": " + e);
    }
    return null;
  }

  /**
   * <code>setBreakpoint</code> sets a breakpoint at a filename and
   * line number indicated as arguments. This method is invoked
   * whenever the user runs the 'b' command.
   *
   * @param args a <code>Vector</code> value containing the arguments
   * of the command
   * @exception NumberFormatException if an error occurs processing
   * the line number, which should be a positive integer.
   */
  public void setBreakpoint(Vector args)
    throws NumberFormatException, IOException
  {
    String filename;
    int line;
    
    if (args.size() != 3) {
      System.out.println(breakpointUsage);
      return;
    }

    filename = getAbsoluteFilename((String)args.get(1), true);
    line = getPositiveInteger((String)args.get(2), breakpointUsage, true);

    manager.setBreakpoint(filename, line);
    if (observer != null)
      observer.breakpointSetAt(filename, line);
  }

  /**
   * Remove breakpoint at filename and line indicated as arguments.
   *
   * @param args a <code>Vector</code> value containing the arguments
   * of the command.
   * @exception NumberFormatException if an error occurs
   * @exception IOException if an error occurs
   */
  public void deleteBreakpoint(Vector args)
    throws NumberFormatException, IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(deleteBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "d <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     deleteBreakpointUsage, false);
    if (index >= 0) {
      Breakpoint bkp = getBreakpointNo(index);
      if (bkp == null)
        return;
      filename = bkp.getFilename();
      line = bkp.getLine();
    }
    else {
      filename = getAbsoluteFilename((String)args.get(1), true);
      line = getPositiveInteger((String)args.get(2),
                                deleteBreakpointUsage, true);
    }

    manager.removeBreakpoint(filename, line);
    if (observer != null)
      observer.breakpointDeletedAt(filename, line);
  }

  /**
   * Disables the breakpoint indicated by the number passed as
   * argument. The number is an index in the list of breakpoints as
   * returned by listBreakpoints.
   *
   * @param args a <code>Vector</code> value
   */
  public void disableBreakpoint(Vector args)
    throws IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(disableBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "dis <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     disableBreakpointUsage, false);
    Breakpoint bkp = null;
    
    if (index >= 0)
      bkp = getBreakpointNo(index);
    else {
      filename = getAbsoluteFilename((String)args.get(1), true);
      line = getPositiveInteger((String)args.get(2),
                                disableBreakpointUsage, true);
      bkp = manager.getBreakpointAt(filename, line);
    }

    if (bkp == null)
      return;

    bkp.setEnabled(false);
    if (observer != null)
      observer.breakpointDisabledAt(bkp.getFilename(), bkp.getLine());
  }

  /**
   * Enables the breakpoint indicated by the number passed as
   * argument. The number is an index in the list of breakpoints as
   * returned by listBreakpoints.
   *
   * @param args a <code>Vector</code> value
   */
  public void enableBreakpoint(Vector args)
    throws IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(enableBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "ena <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     enableBreakpointUsage, false);
    Breakpoint bkp = null;
    
    if (index >= 0)
      bkp = getBreakpointNo(index);
    else {
      filename = getAbsoluteFilename((String)args.get(1), true);
      line = getPositiveInteger((String)args.get(2),
                                enableBreakpointUsage, true);
      bkp = manager.getBreakpointAt(filename, line);
    }

    if (bkp == null)
      return;

    bkp.setEnabled(true);
    if (observer != null)
      observer.breakpointEnabledAt(bkp.getFilename(), bkp.getLine());
  }

  /**
   * Common method for removing, enabling and disabling
   * breakpoints. Given an index in the list of breakpoints as
   * returned by listBreakpoints, returns the Breakpoint instance.
   *
   * @param index an <code>int</code> value
   * @return a <code>Breakpoint</code> value
   */
  protected Breakpoint getBreakpointNo(int index)
  {
    Collection breakpoints = manager.getBreakpoints();
    
    if (index >= breakpoints.size()) {
      System.out.println("Breakpoint number greater than number of "
                         + "breakpoints: " + breakpoints.size());
      return null;
    }

    Object[] bkpArray = breakpoints.toArray();
    Breakpoint bkp = (Breakpoint)bkpArray[index];
    return bkp;
  }

  /**
   * Generates a list of breakpoints to the output. If the breakpoint
   * is disabled a 'D' character indicates it.
   *
   * @param args a <code>Vector</code> value
   */
  public void listBreakpoints(Vector args)
  {
    Collection breakpoints = manager.getBreakpoints();
    Iterator iter = breakpoints.iterator();
    int i = 0;

    while (iter.hasNext()) {
      Breakpoint bkp = (Breakpoint)iter.next();
      String filename = bkp.getFilename();
      int line = bkp.getLine();
      Object[] msgArgs = {
        new Integer(i),
        bkp.isEnabled() ? " " : "D",
        filename,
        new Integer(line)
      };
      String fmt = MessageFormat.format("{0,number,##} {1} {2} {3,number}",
                                        msgArgs);
      System.out.println(fmt);
      i++;
    }
  }

  protected boolean parseArguments(Vector args, String helpString)
    throws Exception
  {
    boolean foundXML = false;
    // By default use the associated stylesheet
    manager.setXSLTStylesheet(null);

    // Set outputStream as the default for output
    manager.setOutputStream(outputStream, false);

    int i = 1;
    int size = args.size();
    while (i < size) {
      String arg = (String)args.get(i);
      if (arg.equals("-xsl")) {
        if (i + 1 >= size) {
          System.out.println("Required XSLT stylesheet argument is missing!");
          System.out.println(helpString);
          return false;
        }
        manager.setXSLTStylesheet(getAbsoluteFilename((String)args.get(i + 1),
                                                      true));
        i += 2;
      }
      else if (arg.equals("-xml")) {
        foundXML = true;
        if (i + 1 >= size) {
          System.out.println("Required XML file name argument is missing!");
          System.out.println(helpString);
          return false;
        }
        manager.setXMLFilename(getAbsoluteFilename((String)args.get(i + 1),
                                                   true));
        i += 2;
      }
      else if (arg.equals("-o")) {
        if (i + 1 >= size) {
          System.out.println("Required output filename argument is missing!");
          System.out.println(helpString);
          return false;
        }
          
        String outputFilename = (String)args.get(i + 1);
        File file = new File(outputFilename);
        boolean created = file.createNewFile();
        file.deleteOnExit();
        FileOutputStream fileStream = new FileOutputStream(file);
        manager.setOutputStream(fileStream, true);
        i += 2;
      }
      else if (arg.equals("-a")) {
        manager.setXSLTStylesheet(null);
        i++;
      }
      else if (arg.equals("-p")) {
        if (i + 1 >= size) {
          System.out.println("Required parameter argument is missing!");
          System.out.println(helpString);
          return false;
        }
	int numberOfParams = Integer.parseInt((String)args.get(i+1));
	if (i + 1 + numberOfParams >= size) {
          System.out.println("Required parameter argument is missing!");
          System.out.println(helpString);
          return false;
        }
	Hashtable transformParameters = new Hashtable();
	for (int p = 0; p < numberOfParams; p++) {
	    String parameter = (String)args.get(i+2+p);
	    int sep = parameter.indexOf('=');
	    transformParameters.put(parameter.substring(0, sep), parameter.substring(sep+1));
	}
	manager.setTransformParameters(transformParameters);
        i += 2 + numberOfParams;
      }
      else {
        System.out.println("Unknown argument: " + arg + "\n" + helpString);
        System.out.println(helpString);
        return false;
      }
    }

    if (!foundXML) {
      System.out.println(helpString);
      return false;
    }
    
    if (debugger.isStarted()) {
      System.out.print("XSLT processor already running, aborting.");
      debugger.stopProcessing();
    }
    return true;
  }
  
  /**
   * <code>debugXSLTProcessor</code> starts the execution of the XSLT
   * processor on the XML filename specified as argument. The
   * processor will stop when it hits a breakpoint, so make sure you
   * setup breakpoints before invoking it. If the processing takes a
   * long time you can stop it using the stopXSLTProcessing function
   * (the 'stop' command).
   *
   * @param args a <code>Vector</code> value containing the argument
   * of the command.
   */
  public void debugXSLTProcessor(Vector args)
    throws Exception
  {
    if (!parseArguments(args, debugUsage))
      return;

    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    manager.setOutputStream(outputStream, false);
    String currentProcessorName = (String)parameters.get("processor");
    if (!currentProcessorName.equalsIgnoreCase(debugger.getProcessorName()))
      createXSLTDebugger();

    manager.startDebugger();
  }

  public void runXSLTProcessor(Vector args)
    throws Exception
  {
    if (!parseArguments(args, runUsage))
      return;

    String currentProcessorName = (String)parameters.get("processor");
    if (!currentProcessorName.equalsIgnoreCase(debugger.getProcessorName()))
      createXSLTDebugger();

    manager.startProcessor();
  }

  public void convertToPDF(Vector args)
    throws Exception
  {
    boolean foundXML = false;
    boolean foundOutputFile = false;
    int i = 1;
    int size = args.size();
    String outputFilename = null;
    
    while (i < size) {
      String arg = (String)args.get(i);
      if (arg.equals("-xml")) {
        foundXML = true;
        if (i + 1 >= size) {
          System.out.println("Required XML file name argument is missing!");
          System.out.println(toPDFUsage);
          return;
        }
        manager.setXMLFilename(getAbsoluteFilename((String)args.get(i + 1),
                                                   true));
        i += 2;
      }
      else if (arg.equals("-o")) {
        if (i + 1 >= size) {
          System.out.println("Required output filename argument is missing!");
          System.out.println(toPDFUsage);
          return;
        }

        foundOutputFile = true;
        outputFilename = (String)args.get(i + 1);
        i += 2;
      }
      else if (arg.equals("-info")) {
	manager.setFopLogLevel(FOPLogger.LEVEL_INFO);
	i += 1;
      }
      else if (arg.equals("-warn")) {
	manager.setFopLogLevel(FOPLogger.LEVEL_WARN);
	i += 1;
      }
      else if (arg.equals("-debug")) {
	manager.setFopLogLevel(FOPLogger.LEVEL_DEBUG);
	i += 1;
      }
      else if (arg.equals("-error")) {
	manager.setFopLogLevel(FOPLogger.LEVEL_ERROR);
	i += 1;
      }
      else if (arg.equals("-fatal")) {
	manager.setFopLogLevel(FOPLogger.LEVEL_FATAL);
	i += 1;
      }
      else if (arg.equals("-disabled")){
    	manager.setFopLogLevel(FOPLogger.LEVEL_DISABLED);
	i += 1;
      }
      else {
        System.out.println("Unknown argument: " + arg + "\n" + toPDFUsage);
        return;
      }
    }

    if (!foundXML) {
      System.out.println("No XML document to process was specified!");
      System.out.println(toPDFUsage);
      return;
    }
    if (!foundOutputFile) {
      System.out.println("No output file specified!");
      System.out.println(toPDFUsage);
      return;
    }
    manager.convertToPDF(outputFilename);
  }
  
  public void doStep(Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doStep();
  }

  public void doNext(Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doNext();
  }

  public void doFinish(Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doFinish();
  }

  public void continueProcessing (Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doContinue();
  }

  /**
   * Sets the current source frame. This could modify the style frame
   * pointer.
   *
   * @param args a <code>Vector</code> value
   */
  public void setCurrentSourceFrame(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(setSourceFrameUsage);
      return;
    }

    Stack sourceFrames = manager.getSourceFrames();
    int frameNo = getPositiveOrZeroInteger((String)args.get(1),
                                           setSourceFrameUsage, true);
    if (frameNo >= sourceFrames.size()) {
      System.out.println("Requested frame number bigger than number of "
                         + "frames: " + sourceFrames.size());
      return;
    }

    currentSourceFrame = frameNo;
    SourceFrame sourceFrame
      = (SourceFrame)sourceFrames.get(currentSourceFrame);
    StyleFrame styleFrame = sourceFrame.getStyleFrame();
    Stack styleFrames = manager.getStyleFrames();
    currentStyleFrame = styleFrames.indexOf(styleFrame);
    if (observer != null)
      observer.stackFramesChanged(currentSourceFrame, currentStyleFrame);
  }

  public void setCurrentStyleFrame(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(setStyleFrameUsage);
      return;
    }

    Stack styleFrames = manager.getStyleFrames();
    int frameNo = getPositiveOrZeroInteger((String)args.get(1),
                                           setStyleFrameUsage, true);
    if (frameNo >= styleFrames.size()) {
      System.out.println("Requested frame number bigger than number of "
                         + "frames: " + styleFrames.size());
      return;
    }

    currentStyleFrame = frameNo;
    StyleFrame styleFrame
      = (StyleFrame)styleFrames.get(currentStyleFrame);
    SourceFrame sourceFrame = styleFrame.getSourceFrame();
    Stack sourceFrames = manager.getSourceFrames();
    currentSourceFrame = sourceFrames.indexOf(sourceFrame);
    if (observer != null)
      observer.stackFramesChanged(currentSourceFrame, currentStyleFrame);
  }
  
  public void showSourceFrames(Vector args)
  {
    Stack sourceFrames = manager.getSourceFrames();

    if (currentSourceFrame == -1)
      currentSourceFrame = sourceFrames.size() - 1;
    
    for (int i = 0; i < sourceFrames.size(); i++) {
      SourceFrame sourceFrame = (SourceFrame)sourceFrames.get(i);
      Object[] msgArgs = {
        new Integer(i),
        currentSourceFrame == i ? "*" : " ",
        sourceFrame.getName()
      };
      String line = MessageFormat.format("{0,number,##} {1} {2}", msgArgs);
      System.out.println(line);
    }
  }

  public void showStyleFrames(Vector args)
  {
    Stack styleFrames = manager.getStyleFrames();

    if (currentStyleFrame == -1)
      currentStyleFrame = styleFrames.size() - 1;
    
    for (int i = 0; i < styleFrames.size(); i++) {
      StyleFrame styleFrame = (StyleFrame)styleFrames.get(i);
      Object[] msgArgs = {
        new Integer(i),
        currentStyleFrame == i ? "*" : " ",
        styleFrame.getName() 
      };
      String line = MessageFormat.format("{0,number,##} {1} {2}", msgArgs);
      System.out.println(line);
    }
  }

  /**
   * Shows the names of the local variables in the current style
   * frame. To view the value of a local variable use the 'pl'
   * command, which is bound to printLocalVariable.
   *
   * @param args a <code>Vector</code> value
   */
  public void showLocalVariables(Vector args)
  {
    StyleFrame styleFrame = manager.peekStyleFrame(currentStyleFrame);
    ArrayList variables = styleFrame.getLocalVariables();
    showVariables(variables);
  }

  /**
   * Display the global variables. To view the value of a global
   * variable use the 'pg' command, bound to the printGlobalVariable
   * method.
   *
   * @param args a <code>Vector</code> value
   */
  public void showGlobalVariables(Vector args)
  {
    ArrayList variables = debugger.getGlobalVariables();
    showVariables(variables);
  }

  /**
   * Common method used by both showLocalVariables and
   * showGlobalVariables methods to display the name of their
   * respective variables.
   *
   * @param variables an <code>ArrayList</code> value
   */
  protected void showVariables(ArrayList variables)
  {
    if (variables == null)
      return;
    
    for (int i = 0; i < variables.size(); i++) {
      Variable var = (Variable)variables.get(i);
      String name = var.getName();
      System.out.println(name);
    }
  }

  /**
   * Prints the value of a local variable.
   *
   * @param args a <code>Vector</code> value
   */
  public void printLocalVariable(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(printLocalVariableUsage);
      return;
    }

    String name = (String)args.get(1);

    StyleFrame styleFrame = manager.peekStyleFrame(currentStyleFrame);
    if (styleFrame == null) {
      System.out.println("No such variable: " + name);
      return;
    }

    Value value = null;
    try {
      value = styleFrame.getValueOfLocalVariable(name);
    }
    catch (NoSuchFieldException e) {
      System.out.println("No such variable: " + name);
      return;
    }

    if (value == null)
      System.out.println(name + ": null");
    else {
      String stringValue = value.getValue();
      Type type = value.getType();
      String typeName = (type == null ? "unknown" : type.getTypeName());
      System.out.println(name + ": " + typeName + "\n" + stringValue);
    }
  }
  
  /**
   * Prints the value of a global variable.
   *
   * @param args a <code>Vector</code> value
   */
  public void printGlobalVariable(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(printGlobalVariableUsage);
      return;
    }

    String name = (String)args.get(1);

    Value value = null;
    try {
      value = debugger.getValueOfGlobalVariable(name);
    }
    catch (NoSuchFieldException e) {
      System.out.println("No such variable: " + name);
      return;
    }

    if (value == null)
      System.out.println(name + ": null");
    else {
      String stringValue = value.getValue();
      Type type = value.getType();
      String typeName = (type == null ? "unknown" : type.getTypeName());
      System.out.println(name + ": " + typeName + "\n" + stringValue);
    }
  }
  
  public void quit(Vector args)
  {
    System.out.println("Quit");
    System.exit(0);
  }

  public void setParameter(Vector args)
  {
    if (args.size() != 3) {
      System.out.println(setParameterUsage);
      return;
    }

    parameters.put(args.get(1), args.get(2));
  }

  public void showParameters(Vector args)
  {
    if (args.size() > 2) {
      System.out.println(showParametersUsage);
      return;
    }

    if (args.size() == 1) {
      // This is a request to show all the parameters
      Iterator iter = parameters.keySet().iterator();
      while (iter.hasNext()) {
        Object param = iter.next();
        Object value = parameters.get(param);
        System.out.println(param + ":\t" + value);
      }
    }
    else {
      // Request to show a specific parameter
      Object value = parameters.get(args.get(1));
      System.out.println(args.get(1) + ":\t" + value);
    }
  }

  public void stopXSLTProcessing(Vector args)
    throws InterruptedException
  {
    debugger.stopProcessing();
  }

  protected String getAbsoluteFilename(String filename, boolean checkExistance)
    throws IOException
  {
    File file = new File(filename);
    String canonicalName;
    
    try {
      canonicalName = file.getCanonicalPath();
      if (manager.runningWindows)
	canonicalName = canonicalName.replace('\\', '/');
    }
    catch (IOException e) {
      System.out.println("Error obtaining the absolute filename: " + e);
      throw e;
    }

    if (checkExistance && !file.exists()) {
      System.out.println("Filename doesn't exists: " + canonicalName);
      throw new IOException();
    }

    return "file:" + canonicalName;
    //return canonicalName;
  }

  public String getParameter(String param)
  {
    return (String)parameters.get(param);
  }

  protected int getPositiveInteger(String arg, String usage, boolean showError)
    throws NumberFormatException
  {
    int value = getPositiveOrZeroInteger(arg, usage, showError);
    if (value == 0 && showError) {
      System.out.println("Line number must be integer greater than 0, got "
                         + arg + ".");
      System.out.println(usage);
      throw new NumberFormatException();
    }
    return value;
  }

  protected int getPositiveOrZeroInteger(String arg, String usage,
                                         boolean showError)
    throws NumberFormatException
  {
    int value = -1;
    
    try {
      value = Integer.parseInt(arg);
      if (value < 0)
        throw new NumberFormatException();
    }
    catch (NumberFormatException e) {
      if (showError) {
        System.out.println("Line number must be integer greater or equal to 0,"
                           + " got " + arg + ".");
        System.out.println(usage);
        throw e;
      }
    }

    return value;
  }

  public Vector split(String line)
    throws Exception
  {
    int length = line.length();
    StringBuffer tmp = new StringBuffer(length);
    Vector arguments = new Vector();
    boolean insideString = false;
    char ch;
    int i = 0;

    while (i < length) {
      // Skip over any blank characters 
      for (; i < length && ((ch = line.charAt(i)) == ' ' || ch == '\t'); i++)
        ; // do nothing

      if (i < length && (ch = line.charAt(i)) == '"') {
        insideString = true;
        i++;
      }

      while (i < length
             && (insideString
                 || ((ch = line.charAt(i)) != ' ' && ch != '\t'))) {
        if ((ch = line.charAt(i)) == '\\') {
          if (i + 1 >= length)
            throw new Exception("Escape character not followed by character.");
          tmp.append(line.charAt(i + 1));
          i += 2;
          continue;
        }

        if (ch == '"') {
          if (!insideString)
            throw new Exception("Unescaped quote character in argument!");
          insideString = false;
          i++;
          break;
        }

        tmp.append(ch);
        i++;
      }

      if (tmp.length() != 0) {
        arguments.add(tmp.toString());
        tmp.delete(0, tmp.length());
      }
    }

    if (insideString)
      throw new Exception("Quoted string not terminated!");

    return arguments;
  }

  public void mainLoop()
  {
    InputStreamReader sin = new InputStreamReader(System.in);
    BufferedReader stdin = new BufferedReader(sin);

    while (true) {
      String line;

      // Read a command from the input
      System.out.print("xslt> ");
      System.out.flush();
      try {
        line = stdin.readLine();
      }
      catch (IOException e) {
        return;
      }

      if (line == null) {
        System.out.println("\nQuit");
        System.exit(0);
      }
      if (line.equals(""))
        continue;

      Vector args;
      try {
        args = split(line);
      }
      catch (Exception ex) {
        System.out.println("Parse error: " + ex.toString());
        continue;
      }

      if (args.size() == 0)
        continue;
        
      String cmd = (String)args.get(0);
      Method method = (Method)commands.get(cmd);
      if (method == null) {
        System.out.println("No such command '" + cmd + "'.");
        continue;
      }

      Object[] mthArguments = { args };
      try {
        method.invoke(this, mthArguments);
      }
      catch (NumberFormatException e) {
        // Message should have been provided
      }
      catch (Exception e) {
        System.out.println("Error invoking method '"
                           + method.getName() + "': " + e);
        observer.processorFinished();
      }
    }
  }

  public void setObserver(Observer observer)
  {
    this.observer = observer;
    manager.setObserver(this.observer);
  }

  public AbstractXSLTDebugger getDebugger()
  {
    return debugger;
  }

  public Manager getManager()
  {
    return manager;
  }

  public void setOutputStream(OutputStream stream)
  {
    outputStream = stream;
  }
}
