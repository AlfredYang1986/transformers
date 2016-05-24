package controllers

import play.api._
import play.api.mvc._

object AdminController extends Controller {
    def adminLogin = Action {
        Ok(views.html.admin_login())
    }  
  
  
    def index = Action {
        Ok(views.html.admin_index())
    }
    
    def passCompanyPage = Action {
        Ok(views.html.admin_company_pass())
    }
    
    def passIndustryPage = Action {
        Ok(views.html.admin_industry_pass())
    }
    
    def passSpecialwayPage = Action {
        Ok(views.html.admin_specialway_pass())
    }
    
    def passDriverPage = Action {
        Ok(views.html.admin_driver_pass())
    }
    
    def faildCompanyPage = Action {
        Ok(views.html.admin_company_faild())
    }
    
    def faildIndustryPage = Action {
        Ok(views.html.admin_industry_faild())
    }
    
    def faildSpecialwayPage = Action {
        Ok(views.html.admin_specialway_faild())
    }
    
    def faildDriverPage = Action {
        Ok(views.html.admin_driver_faild())
    }
   
    def certificatingCompanyPage = Action {
        Ok(views.html.admin_company_certificating())
    }
    
    def certificatingIndustryPage = Action {
        Ok(views.html.admin_industry_certificating())
    }
    
    def certificatingSpecialwayPage = Action {
        Ok(views.html.admin_specialway_certificating())
    }
    
    def certificatingDriverPage = Action {
        Ok(views.html.admin_driver_certificating())
    }
}