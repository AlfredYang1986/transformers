package controllers

import play.api._
import play.api.mvc._

object RegisterController extends Controller {
    /**
     * certificate
     */
    def register = Action {
        Ok(views.html.register_index())
    }
     
    /**
     * certificate Driver
     */
    def registerDriver = Action {
        Ok(views.html.registerDriver("test"))
    }
        
    /**
     * certificate Compelete
     */
    def registerComplete = Action {
        Ok(views.html.registerComplete("test"))
    }
}