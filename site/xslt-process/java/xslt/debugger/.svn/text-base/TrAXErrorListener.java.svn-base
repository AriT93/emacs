/*
    TrAXErrorListener.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: May 17, 2001

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

package xslt.debugger;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.TransformerException;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerConfigurationException;

/**
 * Implementation of the
 * <code>java.xml.transform.ErrorListener</code> interface in the
 * TrAX interface, and is used to report error to an
 * <code>Observer</code> instance.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 */
public class TrAXErrorListener implements ErrorListener
{
  protected Manager manager;
  
  public TrAXErrorListener(Manager manager)
  {
    this.manager = manager;
  }
    
  public void error(TransformerException ex)
    throws TransformerException
  {
    reportError("Error", ex);
  }

  public void warning(TransformerException ex)
    throws TransformerException
  {
    reportError("Warning", ex);
  }

  public void fatalError(TransformerException ex)
    throws TransformerException
  {
    reportError("Fatal error", ex);
  }

  /**
   * <code>reportError</code> reformats the error to include line and
   * column numbers, and reports the error to the observer. Returns a
   * new exception instance that includes this information. This gives
   * an opportunity to the specific XSLT processor to override the
   * handler of the error.
   *
   * @param type a <code>String</code> value
   * @param ex a <code>TransformerException</code> value
   * @exception TransformerException if an error occurs
   */
  public TransformerException reportError(String type, TransformerException ex)
    throws TransformerException
  {
    SourceLocator locator = ex.getLocator();
    if (locator != null) {
      String message = Utils.uriNoProtocol(locator.getSystemId()) + ":"
        + locator.getLineNumber() + ":";

      if (locator.getColumnNumber() != -1)
        message = message + locator.getColumnNumber() + ":";
      message = message + " " + type + ": " + ex.getMessage() + "\n";
      
      ex = new TransformerConfigurationException(message);
    }
      
    manager.getObserver().caughtException(ex);
    return ex;
  }
}
