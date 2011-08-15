/*
    Breakpoint.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

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

public class Breakpoint
{
  String filename;
  int line;
  Manager manager;
  boolean enabled = true;

  public Breakpoint (String filename, int line, Manager manager)
  {
    this.filename = filename;
    this.line = line;
    this.manager = manager;
  }

  public void setEnabled(boolean flag)
  {
    enabled = flag;
  }

  public boolean isEnabled()
  {
    return enabled;
  }

  public int hashCode()
  {
    return line;
  }

  public boolean equals(Object obj)
  {
    if (!(obj instanceof Breakpoint))
      return false;

    Breakpoint other = (Breakpoint)obj;
    return line == other.line
      && filename.equals(other.filename)
      && manager == other.manager;
  }

  /**
   * Get the value of filename.
   * @return value of filename.
   */
  public String getFilename()
  {
    return filename;
  }
  
  /**
   * Get the value of line.
   * @return value of line.
   */
  public int getLine()
  {
    return line;
  }
}
