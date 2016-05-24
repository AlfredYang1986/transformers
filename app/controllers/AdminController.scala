package controllers

import play.api._
import play.api.mvc._

object AdminController extends Controller {
    def index = Action {
        Ok(views.html.admin_index())
    }
    
    def passCompanyPage = Action {
        Ok(views.html.admin_company_pass())
    }
    
    def passIndustryPage = Action {
        Ok(views.html.admin_industry_pass())
    }
    
    def passSpicalwayPage = Action {
        Ok(views.html.admin_spicalway_pass())
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
    
    def faildSpicalwayPage = Action {
        Ok(views.html.admin_spicalway_faild())
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
    
    def certificatingSpicalwayPage = Action {
        Ok(views.html.admin_spicalway_certificating())
    }
    
    def certificatingDriverPage = Action {
        Ok(views.html.admin_driver_certificating())
    }
}