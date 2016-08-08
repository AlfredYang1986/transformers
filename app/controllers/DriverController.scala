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

    /**
     * Driver Account Normal Information (the driver could modify)
     */
    def driverLoginAccountNormalInfo = Action {
        Ok(views.html.driverLoginAccountNormalInfo("Your new application is ready."))
    }

    /**
     * Driver Account Validate Information
     */
    def driverLoginAccountValidateInfo = Action {
        Ok(views.html.driverLoginAccountValidateInfo("Your new application is ready."))
    }

    /**
     * Driver Recruitment
     */
    def driverLoginRecruitment = Action {
        Ok(views.html.driverLoginRecruitment("Your new application is ready."))
    }

    /**
     * Driver Followed Company
     */
    def driverLoginAccountFollowedCompany = Action {
        Ok(views.html.driverLoginAccountFollowedCompany("Your new application is ready."))
    }

    /**
     * Driver Search Company
     */
    def driverLoginSearchCompany = Action {
        Ok(views.html.driverLoginSearchCompany("Your new application is ready."))
    }

    /**
     * Driver Search Department
     */
    def driverLoginSearchDepartment = Action {
        Ok(views.html.driverLoginSearchDepartment("Your new application is ready."))
    }


    /**
     * Driver Search Special Way
     */
    def driverLoginSearchSpecialWay = Action {
        Ok(views.html.driverLoginSearchSpecialWay("Your new application is ready."))
    }

}