/*
    SaxonSAXParserErrorHandler.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: June  8, 2001

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

package xslt.debugger.saxon;

import xslt.debugger.SAXParserErrorHandler;
import xslt.debugger.Manager;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class SaxonSAXParserErrorHandler extends SAXParserErrorHandler
{
  public SaxonSAXParserErrorHandler(Manager manager)
  {
    super(manager);
  }
  
  public SAXException reportError(String type, SAXParseException ex)
    throws SAXException
  {
    SAXException exception = super.reportError(type, ex);
    throw exception;
  }
}
