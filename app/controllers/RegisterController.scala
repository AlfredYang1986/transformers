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
    def certificateDriver = Action {
        Ok(views.html.certificateDriver("test"))
    }
        
    /**
     * certificate Compelete
     */
    def certificateComplete = Action {
        Ok(views.html.certificateComplete("test"))
    }
}