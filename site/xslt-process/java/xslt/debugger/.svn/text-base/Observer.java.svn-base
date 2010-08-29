/*
    Observer.java

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
import java.util.ArrayList;

/**
 * An <code>Observer</code> could be registered with the
 * <code>Manager</code> to be notified of important changes in the
 * state of the debugger.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 */
public interface Observer
{
  /**
   * Called immediately after the debugger process is created. Use
   * this method to setup any global variables you might need for the
   * lifetime of the debugger process. This method is called only once
   * during the life of the debugger process.
   */
  public void debuggerProcessStarted();

  /**
   * Called to notify the debugger is stopped, at
   * <code>filename</code> in line <code>line</code> and column
   * <code>column</code>. It has stopped at this <code>line</code>
   * for the <code>count</code>th consecutive time.
   *
   * The <code>message</code> contains a string starting either with
   * "entering: " or with "leaving: ", depending on whether the XSLT
   * processor in entering or leaving a particular element. Following
   * this, the display name of the element being visited is appended.
   *
   * @param filename a <code>String</code> value
   * @param lineno an <code>int</code> value
   * @param column an <code>int</code> value
   * @param count an <code>int</code> value
   * @param message a <code>String</code> value
   */
  public void debuggerStopped(String filename,
                              int lineno,
                              int column,
			      int count,
                              String message);

  /**
   * Called to inform that a breakpoint was set in
   * <code>filename</code> at <code>line</code>.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointSetAt(String filename, int line);

  /**
   * Called to inform that a breakpoint was deleted in
   * <code>filename</code> at <code>line</code>.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointDeletedAt(String filename, int line);

  /**
   * Called to inform that the breakpoint in <code>filename</code> at
   * <code>line</code> was enabled.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointEnabledAt(String filename, int line);

  /**
   * Called to inform that the breakpoint in <code>filename</code> at
   * <code>line</code> was disabled.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointDisabledAt(String filename, int line);

  /**
   * Called to notify that the source frames stack has changed. This
   * method is called only if the source frames stack has changed when
   * the debugger stops. It is not called during the normal execution
   * of the XSLT processor.
   */
  public void sourceStackChanged();

  /**
   * Called to notify that the style frames stack has changed. This
   * method is called only if the style frames stack has changed when
   * the debugger stops. It is not called during the normal execution
   * of the XSLT processor.
   */
  public void styleStackChanged();

  /**
   * <code>stackFramesChanged</code> is called when either the source
   * frame or the style frame changes.
   *
   * @param sourceFrameNo an <code>int</code> value, the index in the
   * source frame stack
   * @param styleFrameNo an <code>int</code> value, the index in the
   * style frame stack
   */
  public void stackFramesChanged(int sourceFrameNo, int styleFrameNo);

  /**
   * Called to inform that the global variables in the last XSLT
   * template have changed.
   *
   * @param globalVariables an <code>ArrayList</code> containing a list
   * of {@link Variable} instances.
   */
  public void globalVariablesChanged(ArrayList globalVariables);

  /**
   * Called to inform that the local variables in the last XSLT
   * template have changed.
   *
   * @param localVariables an <code>ArrayList</code> containing a list
   * of {@link Variable} instances.
   */
  public void localVariablesChanged(ArrayList localVariables);

  /**
   * Notifies that the XSLT processing is going to start now.
   */
  public void processorStarted();
  
  /**
   * Notifies that the XSLT processing has just finished.
   */
  public void processorFinished();

  /**
   * Notifies that an exception occurred while running the XSLT
   * debugger process.
   *
   * @param e an <code>Exception</code> value
   */
  public void caughtException(Exception e);

  /**
   * Called to provide information to the observer.
   *
   * @param info a <code>String</code> value
   */
  public void displayInfo(String info);
}
