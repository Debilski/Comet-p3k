package code
package comet

import net.liftweb._
import http._
import util._
import Helpers._

import js._
import json._
import JsonAST._

trait Command
trait JsCommand extends Command with JsCmd

case class North(i: Int) extends JsCommand {
  def toJsCmd = """if (agents["""+i+"""]) agents["""+i+"""].up();"""
}
case class South(i: Int) extends JsCommand {
  def toJsCmd = """if (agents["""+i+"""]) agents["""+i+"""].down();"""
}
case class East(i: Int) extends JsCommand {
  def toJsCmd = """if (agents["""+i+"""]) agents["""+i+"""].right();"""
}
case class West(i: Int) extends JsCommand {
  def toJsCmd = """if (agents["""+i+"""]) agents["""+i+"""].left();"""
}


import net.liftweb.http._
import net.liftweb.http.rest._

object MyRest extends RestHelper {
  val restPacman = new Pacman()

  serve {
    case JsonGet("api" :: AsInt(i) :: "north" :: _, _) => PacmanServer ! North(i); JString("Success")
    case JsonGet("api" :: AsInt(i) :: "south" :: _, _) => PacmanServer ! South(i); JString("Success")
    case JsonGet("api" :: AsInt(i) :: "east" :: _, _) => PacmanServer ! East(i); JString("Success")
    case JsonGet("api" :: AsInt(i) :: "west" :: _, _) => PacmanServer ! West(i); JString("Success")
  }  
}


/**
 * The screen real estate on the browser will be represented
 * by this component.  When the component changes on the server
 * the changes are automatically reflected in the browser.
 */
class Pacman extends CometActor with CometListener {
  private var msgs: Vector[JsCommand] = Vector() // private state
  /**
   * When the component is instantiated, register as
   * a listener with the ChatServer
   */

  def registerWith = PacmanServer
  /**
   * The CometActor is an Actor, so it processes messages.
   * In this case, we're listening for Vector[String],
   * and when we get one, update our private state
   * and reRender() the component.  reRender() will
   * cause changes to be sent to the browser.
   */
  
  override def lowPriority = {
    case v: JsCommand => msgs = Vector(v); reRender()
    case v: Vector[JsCommand] => msgs = v; reRender()
  }
  /**
   * Put the messages in the li elements and clear
   * any elements that have the clearable class.
   */
  def render = (JsCmds.Noop /: msgs)(_ & _)
}


import actor._
/**
 * A singleton that provides chat features to all clients.
 * It's an Actor so it's thread-safe because only one
 * message will be processed at once.
 */
object PacmanServer extends LiftActor with ListenerManager {
  private var msgs: Vector[JsCommand] = Vector() // private state

  /**
   * When we update the listeners, what message do we send?
   * We send the msgs, which is an immutable data structure,
   * so it can be shared with lots of threads without any
   * danger or locking.
   */
  def createUpdate = msgs

  /**
   * process messages that are sent to the Actor.  In
   * this case, we're looking for Strings that are sent
   * to the ChatServer.  We append them to our Vector of
   * messages, and then update all the listeners.
   */
  override def lowPriority = {
    case s: JsCommand => msgs = Vector(s); updateListeners()
  }
}


