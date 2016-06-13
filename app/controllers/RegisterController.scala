package controllers

import play.api._
import play.api.mvc._

import module.common.xml.xmlOpt

object RegisterController extends Controller {
    /**
     * certificate
     */
    def register = Action {
        Ok(views.html.register_index(xmlOpt.allCities))
    }
     
    /**
     * certificate Driver
     */
    def registerDriver = Action {
        Ok(views.html.registerDriver(xmlOpt.allCities))
    }
        
    /**
     * certificate Compelete
     */
    def registerComplete = Action {
        Ok(views.html.registerComplete("test"))
    }
}