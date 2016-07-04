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
     * Driver Account Followed Company
     */
    def driverLoginAccountFollowedCompany = Action {
        Ok(views.html.driverLoginAccountFollowedCompany("Your new application is ready."))
    }
    
    /**
     * Driver Account Normal Information
     */
    def driverLoginAccountNormalInfo = Action {
        Ok(views.html.driverLoginAccountNormalInfo("Your new application is ready."))
    }
    
    /**
     * Driver Account Change Password
     */
    def driverLoginAccountPsw = Action {
        Ok(views.html.driverLoginAccountPsw("Your new application is ready."))
    }
    
    /**
     * Driver Account Validated Information
     */
    def driverLoginAccountValidateInfo = Action {
        Ok(views.html.driverLoginAccountValidateInfo("Your new application is ready."))
    }

    /**
     * Driver to view the other information about recruuiment
     */
    def driverLoginRecruitment = Action {
        Ok(views.html.driverLoginRecruitment("Your new application is ready."))
    }
    
    /**
     * Driver to search the products based on the company
     */
    def driverLoginSearchCompany = Action {
        Ok(views.html.driverLoginSearchCompany("Your new application is ready."))
    }
    
    /**
     * Driver to search the products based on the department
     */
    def driverLoginSearchDepartment = Action {
        Ok(views.html.driverLoginSearchDepartment("Your new application is ready."))
    }

    /**
     * Driver to search the products based on the special way
     */
    def driverLoginSearchSpecialWay = Action {
        Ok(views.html.driverLoginSearchSpecialWay("Your new application is ready."))
    }
}