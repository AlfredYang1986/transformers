package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.driverOpt.{ driverSearchModule, driverFollowModule }
import module.companyOpt.companyProductModule
import module.auth.AuthModule
import module.auth.AuthModule
import module.auth.authTypes

import module.companyOpt.companyInfoModule
import module.system.config.ConfigModule
import module.common.xml.xmlOpt

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
            val driver = AuthModule.queryInstanceWithToken(token)
            
            val open_id = (driver \ "open_id").asOpt[String].get
            val name = (driver \ "driver_name").asOpt[String].get

            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                val com_lst = (driverSearchModule.queryCompany(toJson("")) \ "result").asOpt[List[JsValue]].get
                val following_lst = (driverFollowModule.queryDriverFollowingLst(toJson(Map("driver_open_id" -> open_id))) \ "result").asOpt[List[String]].get
                Ok(views.html.driverLoginIndex(com_lst)(token)(open_id)(name)(following_lst))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Account Password
     */
    def driverLoginAccountPsw(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val driver = AuthModule.queryInstanceWithToken(token)
            
            val open_id = (driver \ "open_id").asOpt[String].get
            val name = (driver \ "driver_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginAccountPsw(token)(open_id)(name))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Account Normal Information (the driver could modify)
     */
    def driverLoginAccountNormalInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val driver = AuthModule.queryInstanceWithToken(token)
        
            val open_id = (driver \ "open_id").asOpt[String].get
            val name = (driver \ "driver_name").asOpt[String].get
            
            val vc = ConfigModule.configAllVehicles
            
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginAccountNormalInfo(token)(open_id)(name)(driver)(xmlOpt.allCities)(vc))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Account Validate Information
     */
    def driverLoginAccountValidateInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val driver = AuthModule.queryInstanceWithToken(token)
        
            val open_id = (driver \ "open_id").asOpt[String].get
            val name = (driver \ "driver_name").asOpt[String].get
            
            val vc = ConfigModule.configAllVehicles
            
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginAccountValidateInfo(token)(open_id)(name)(driver))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Recruitment
     */
    def driverLoginRecruitment(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val driver = AuthModule.queryInstanceWithToken(token)
            
            val open_id = (driver \ "open_id").asOpt[String].get
            val name = (driver \ "driver_name").asOpt[String].get
           
            val info_lst = (companyInfoModule.queryInfo(toJson("")) \ "result").asOpt[List[JsValue]].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginRecruitment(token)(open_id)(name)(info_lst))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Followed Company
     */
    def driverLoginAccountFollowedCompany(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        val user = AuthModule.queryUserWithToken(token)
        val driver = AuthModule.queryInstanceWithToken(token)
            
        val open_id = (driver \ "open_id").asOpt[String].get
        val name = (driver \ "driver_name").asOpt[String].get
        
        val following_lst = (driverFollowModule.queryDriverFollowingLst(toJson(Map("driver_open_id" -> open_id))) \ "result").asOpt[List[String]].get
        val cp = AuthModule.queryMultipleProfiles(following_lst)
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                Ok(views.html.driverLoginAccountFollowedCompany(token)(open_id)(name)(cp)(following_lst))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Search Company
     */
    def driverLoginSearchCompany(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        val user = AuthModule.queryUserWithToken(token)
        val driver = AuthModule.queryInstanceWithToken(token)
            
        val open_id = (driver \ "open_id").asOpt[String].get
        val name = (driver \ "driver_name").asOpt[String].get
        
        val following_lst = (driverFollowModule.queryDriverFollowingLst(toJson(Map("driver_open_id" -> open_id))) \ "result").asOpt[List[String]].get
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                val com_lst = (driverSearchModule.queryCompany(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.driverLoginSearchCompany(token)(open_id)(name)(following_lst)(com_lst))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Search Department
     */
    def driverLoginSearchDepartment(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        val user = AuthModule.queryUserWithToken(token)
        val driver = AuthModule.queryInstanceWithToken(token)
            
        val open_id = (driver \ "open_id").asOpt[String].get
        val name = (driver \ "driver_name").asOpt[String].get
        
        val vc = ConfigModule.configAllVehicles
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                val product_lst = (companyProductModule.queryProduct(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.driverLoginSearchDepartment(token)(open_id)(name)(xmlOpt.allCities)(vc)(product_lst))
            }
            else Redirect("/index")
        }
    }

    /**
     * Driver Search Special Way
     */
    def driverLoginSearchSpecialWay(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "driver_name").asOpt[String].get
            
            val vc = ConfigModule.configAllVehicles
            
            if ((user \ "auth").asOpt[Int].get > authTypes.driverBase.t) {
                val com_lst = (driverSearchModule.queryCompany(toJson(Map("type" -> 3))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.driverLoginSearchSpecialWay(token)(open_id)(name)(xmlOpt.allCities)(vc)(com_lst))
            }
            else Redirect("/index")
        }
    }

    def driverSearchCompany = Action (request => requestArgs(request)(driverSearchModule.queryCompany))
    def driverPushCollection = Action (request => requestArgs(request)(driverFollowModule.driverFollowCompany))
    def driverPopCollection = Action (request => requestArgs(request)(driverFollowModule.driverUnFollowCompany))
    def driverQueryCollections = Action (request => requestArgs(request)(driverFollowModule.queryDriverFollowingLst)) 

    def driverSearchCompanyHtml = Action { request => 
        try {
  			    request.body.asJson.map { x => 
                val result = (driverSearchModule.queryCompany(x) \ "result").asOpt[List[JsValue]].get 
  			        val following_lst = (x \ "open_id").asOpt[String].map { open_id => 
    			          (driverFollowModule.queryDriverFollowingLst(toJson(Map("driver_open_id" -> open_id))) \ "result").asOpt[List[String]].get
  			        }.getOrElse(Nil)
                Ok(views.html.driver_company_search_result(result)(following_lst))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  		   
    }
}