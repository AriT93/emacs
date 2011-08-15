/*
    SAXParserErrorHandler.java

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
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;
import org.xml.sax.SAXException;

/**
 * <code>SAXParserErrorHandler</code> is an implementation of the
 * <code>org.xml.sax.ErrorHandler</code> interface, and is used to
 * report error to an <code>Observer</code> instance.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 */
public class SAXParserErrorHandler implements ErrorHandler
{
  protected Manager manager;
  
  public SAXParserErrorHandler(Manager manager)
  {
    this.manager = manager;
  }
  
  public void error(SAXParseException ex)
    throws SAXException
  {
    reportError("Error", ex);
  }

  public void warning(SAXParseException ex)
    throws SAXException
  {
    reportError("Warning", ex);
  }

  public void fatalError(SAXParseException ex)
    throws SAXException
  {
    reportError("Fatal error", ex);
  }

  public SAXException reportError(String type, SAXParseException ex)
    throws SAXException
  {
    String message = Utils.uriNoProtocol(ex.getSystemId()) + ":"
      + ex.getLineNumber() + ":";

    if (ex.getColumnNumber() != -1)
      message += ex.getColumnNumber() + ":";

    message += " " + type + ": " + ex.getMessage() + "\n";
    SAXException newEx = new SAXException(message, ex);
    manager.getObserver().caughtException(newEx);
    return newEx;
  }
}
