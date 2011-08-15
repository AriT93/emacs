/*
    Variable.java

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

public class Variable
{
  protected String name;
  protected Value value;
  protected StyleFrame frame;

  public Variable (String name, StyleFrame frame)
  {
    this.name = name;
    this.frame = frame;
  }

  /**
   * Set the value of this variable
   * @param value a <code>Value</code> value
   */
  public void setValue(Value value)
  {
    this.value = value;
  }

  /**
   * Get the value of the variable
   * @return an <code>Object</code> value
   */
  public Value getValue()
  {
    return value;
  }

  /**
   * Get the name of the variable.
   * @return value of name.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Get the style frame this variable belongs to
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame getFrame()
  {
    return frame;
  }
}
