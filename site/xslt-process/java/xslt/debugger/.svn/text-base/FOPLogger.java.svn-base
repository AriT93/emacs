/**
 * FOPLogger.java
 *
 *
 * Created: Sun Jul 14 21:08:58 2002
 *
 *  @author: <a href="mailto:A.M.Addyman@salford.ac.uk">Tony Addyman</a>
 * @version
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

import org.apache.avalon.framework.logger.Logger;

public class FOPLogger implements Logger
{

  public static final int LEVEL_DEBUG = 0;
  public static final int LEVEL_INFO = 1;
  public static final int LEVEL_WARN = 2;
  public static final int LEVEL_ERROR = 3;
  public static final int LEVEL_FATAL = 4;
  public static final int LEVEL_DISABLED = 5;

  Observer observer;
  StringBuffer line = new StringBuffer(80);
  int logLevel = LEVEL_INFO;

  public FOPLogger(Observer observer, int logLevel)
  {
    this.observer = observer;
    if (logLevel >= LEVEL_DEBUG && logLevel <= LEVEL_DISABLED) {
      this.logLevel = logLevel;
    }
  }

  public  void debug( String message )
  {
    if (logLevel <= LEVEL_DEBUG) {
      processMessage("Debug: " + message);
    }
  }

  public void debug( String message, Throwable throwable )
  {
    String exMsg = throwable.getMessage();
    String sep = "";
    if (message != null && exMsg != null) sep = " ";
    debug(message + sep + exMsg);
  }

  public boolean isDebugEnabled()
  {
    return logLevel <= LEVEL_DEBUG;
  }

  public void info( String message )
  {
    if (logLevel <= LEVEL_INFO) {
      processMessage("Info: " + message);
    }
  }

  public void info( String message, Throwable throwable )
  {
    String exMsg = throwable.getMessage();
    String sep = "";
    if (message != null && exMsg != null) sep = " ";
    info(message + sep + exMsg);
  }

  public boolean isInfoEnabled()
  {
    return logLevel <= LEVEL_INFO;
  }

  public void warn( String message )
  {
    if (logLevel <= LEVEL_WARN) {
      processMessage("Warn: " + message);
    }
  }

  public void warn( String message, Throwable throwable )
  {
    String exMsg = throwable.getMessage();
    String sep = "";
    if (message != null && exMsg != null) sep = " ";
    warn(message + sep + exMsg);
  }

  public boolean isWarnEnabled()
  {
    return logLevel <= LEVEL_WARN;
  }

  public void error( String message )
  {
    if (logLevel <= LEVEL_ERROR) {
      processMessage("Error: " + message);
    }
  }

  public void error( String message, Throwable throwable )
  {
    String exMsg = throwable.getMessage();
    String sep = "";
    if (message != null && exMsg != null) sep = " ";
    error(message + sep + exMsg);
  }

  public boolean isErrorEnabled()
  {
    return logLevel <= LEVEL_ERROR;
  }

  public void fatalError( String message )
  {
    if (logLevel <= LEVEL_FATAL) {
      processMessage("Fatal error: " + message);
    }
  }

  public void fatalError( String message, Throwable throwable )
  {
    String exMsg = throwable.getMessage();
    String sep = "";
    if (message != null && exMsg != null) sep = " ";
    fatalError(message + sep + exMsg);
  }

  public boolean isFatalErrorEnabled()
  {
    return logLevel <= LEVEL_FATAL;
  }

  public Logger getChildLogger( String name )
  {
    return null;
  }


// processMessage derived from the method in FOPMessageListener
  
  public void processMessage(String message)
  {
    int fromIndex = 0;
    int endIndex;
    
    while ((endIndex = message.indexOf("\n", fromIndex)) != -1) {
      line.append(message.substring(fromIndex, endIndex));
      observer.displayInfo(line.toString());
      line.delete(0, line.length());
      fromIndex = endIndex + 1;
    }

    line.append(message.substring(fromIndex));
    observer.displayInfo(line.toString());
    line.delete(0, line.length());
  }
 
}// FOPLogger
