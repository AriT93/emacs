/*
    FOPMessageListener.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: August 16, 2001

 */

package xslt.debugger;

import org.apache.fop.messaging.MessageListener;
import org.apache.fop.messaging.MessageEvent;

public class FOPMessageListener implements MessageListener
{
  Observer observer;
  StringBuffer line = new StringBuffer(80);

  public FOPMessageListener(Observer observer)
  {
    this.observer = observer;
  }
  
  public void processMessage(MessageEvent event)
  {
    //    if (event.getMessageType() != MessageEvent.LOG && event.getMessageType() != MessageEvent.ERROR)
    //      return;

    String message = event.getMessage();
    // Check for a \n character in the message. If there's no such
    // character, append it to `line'. Otherwise appends to the line
    // buffer, calls the observer, and clears the line buffer.
    int fromIndex = 0;
    int endIndex;
    
    while ((endIndex = message.indexOf("\n", fromIndex)) != -1) {
      line.append(message.substring(fromIndex, endIndex));
      observer.displayInfo("Message: " + line.toString());
      line.delete(0, line.length());
      fromIndex = endIndex + 1;
    }

    line.append(message.substring(fromIndex));
    observer.displayInfo("Message: " + line.toString());
    line.delete(0, line.length());
  }
}
