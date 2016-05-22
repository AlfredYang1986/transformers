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
     * certificate
     */
    def certificate = Action {
        Ok(views.html.certificate_index("test"))
    }
    
     /**
     * certificate company
     */
    def certificateCompany = Action {
        Ok(views.html.certificateCompany("test"))
    }
    
     /**
     * certificate industry
     */
    def certificateIndustry = Action {
        Ok(views.html.certificateIndustry("test"))
    }

     /**
     * certificate Special Way
     */
    def certificateSpecialWay = Action {
        Ok(views.html.certificateSpecialWay("test"))
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