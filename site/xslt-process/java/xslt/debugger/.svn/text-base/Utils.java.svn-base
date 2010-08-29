/*
    Utils.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 16, 2001

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

import java.util.Vector;
import java.util.StringTokenizer;

public class Utils
{
  public static Vector split(String string, String separator)
  {
    if (string == null || string.equals(""))
      return null;
    
    Vector array = new Vector(2);
    StringTokenizer st = new StringTokenizer(string, separator);

    while (st.hasMoreTokens()) {
      String element = st.nextToken();
      array.add(element);
    }
    return array;
  }

  public static String uriNoProtocol(String uri)
  {
    if (uri.startsWith("file:"))
      return uri.substring(5);
    return uri;
  }

  /**
   * Escapes characters in <code>message</code> as follows:
   *
   * <ul>
   *  <li><pre>\  -> \\</pre></li>
   *  <li><pre>"  -> \d</pre></li>
   *  <li><pre>^  -> \b</pre></li>
   *  <li><pre>$  -> \e</pre></li>
   * </ul>
   *
   * @param message a <code>String</code> value
   */
  public static String escape(String message)
  {
    if (message == null)
      return message;
    
    // Assume about 1% of the characters need to be escaped; probably
    // a safe bet in most cases. Otherwise the buffer is increased
    // automatically by the StringBuffer.
    StringBuffer escaped = new StringBuffer((int)(message.length() * 1.01));

    for (int i = 0, length = message.length(); i < length; i++) {
      char ch = message.charAt(i);
      switch (ch) {
      case '\\':
        escaped.append("\\\\\\\\");
        break;
      case '"':
        escaped.append("\\\\d");
        break;
      case '^':
        escaped.append("\\\\b");
        break;
      case '$':
        escaped.append("\\\\e");
        break;
      default:
        escaped.append(ch);
        break;
      }
    }

    return escaped.toString();
  }
}
