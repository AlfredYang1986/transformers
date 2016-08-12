package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.driverOpt.driverSearchModule
import module.auth.AuthModule
import module.auth.authTypes

object DriverController extends Controller {
  
    /**
     * Driver Login Page
     */
    def driverLoginIndex(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                val com_lst = (driverSearchModule.queryCompany(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.driverLoginIndex(com_lst))
            }
            else Redirect("/index")
        }
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
    def driverLoginSearchCompany(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginSearchCompany())
            }
            else Redirect("/index")
        }
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

    def driverSearchCompany = Action (request => requestArgs(request)(driverSearchModule.queryCompany))
}