package controllers

import play.api._
import play.api.mvc._

object DriverController extends Controller {
  
    /**
     * Driver Login Page
     */
    def driverLoginIndex = Action {
        Ok(views.html.driverLoginIndex("Your new application is ready."))
    }

    /**
     * Driver Account Password
     */
    def driverLoginAccountPsw = Action {
    	Ok(views.html.driverLoginAccountPsw("Your new application is ready."))
    }


    
}