/*
    CmdLineObserver.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
    Date: March 29, 2001

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

import xslt.debugger.Observer;
import java.util.ArrayList;

public class CmdLineObserver implements Observer
{
  Controller controller;
  
  public CmdLineObserver(Controller controller)
  {
    this.controller = controller;
  }

  public void debuggerProcessStarted()
  {
  }

  /**
   * Invoked by the debugger to print a descriptive message showing
   * where the debugger stopped.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   * @param column an <code>int</code> value
   * @param message a <code>String</code> value, additional
   * informative message
   */
  public void debuggerStopped(String filename,
                              int line,
                              int column,
			      int count,
                              String message)
  {
    System.out.println("Debugger stopped at: " + filename
                       + " " + line
                       + ":" + column
		       + "#" + count
                       + " " + message);
  }

  public void breakpointSetAt(String filename, int line)
  {
    System.out.println("Set breakpoint at " + filename + " " + line);
  }
  
  public void breakpointDeletedAt(String filename, int line)
  {
    System.out.println("Deleted breakpoint at " + filename + " " + line);
  }
  
  public void breakpointEnabledAt(String filename, int line)
  {
    System.out.println("Enabled breakpoint at " + filename + " " + line);
  }
  
  public void breakpointDisabledAt(String filename, int line)
  {
    System.out.println("Disabled breakpoint at " + filename + " " + line);
  }

  /**
   *
   */
  public void sourceStackChanged()
  {
    // No implementation
  }

  /**
   *
   */
  public void styleStackChanged()
  {
    // No implementation
  }

  public void stackFramesChanged(int sourceFrameNo, int styleFrameNo)
  {
    // No implementation
  }

  public void globalVariablesChanged(ArrayList globalVariables)
  {
    // No implementation
  }

  public void localVariablesChanged(ArrayList localVariables)
  {
    // No implementation
  }

  public void processorStarted()
  {
  }
  
  public void processorFinished()
  {
  }

  public void caughtException(Exception e)
  {
    e.printStackTrace();
  }

  public void displayInfo(String info)
  {
    System.out.println(info);
  }
}
