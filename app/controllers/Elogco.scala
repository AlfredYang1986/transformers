package controllers

import play.api._
import play.api.mvc._

object Elogco extends Controller {
  
    /**
     * home 
     */
    def index = Action {
        Ok(views.html.index("Your new application is ready."))
    }
    
    /**
     * company
     */
    def company = Action {
        Ok(views.html.company("Your new application is ready."))
    }
    
    /**
     * business
     */
    def business = Action {
        Ok(views.html.business("Your new application is ready."))
    }
    
    /**
     * recruit
     */
    def recruit = Action {
        Ok(views.html.recruit("Your new application is ready."))
    }
    
    /**
     * help
     */
    def help = Action {
        Ok(views.html.help("Your new application is ready."))
    }

    /**
    * forget password
    */
    def forgetpsw = Action {
        Ok(views.html.forgetpsw("Your new application is ready."))
    }

    /**
    * mobile login
    */
    def mobileLogin = Action {
        Ok(views.html.mobileLogin("Your new application is ready."))
    }


}
