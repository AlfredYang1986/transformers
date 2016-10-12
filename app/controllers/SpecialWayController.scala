package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.companyOpt.{ companySearchModule, companyInfoModule, companyProductModule, companyConfigModule } 
import module.driverOpt.{ driverSearchModule, driverFollowModule }
import module.common.xml.xmlOpt
import module.auth.AuthModule
import module.auth.authTypes
import module.system.config.ConfigModule
import module.platformLines.PlatformLinesModule

object SpecialWayController extends Controller {
  
    /**
     * Special Way Login Page
     */
    def swLoginIndex(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val com_lst = (driverSearchModule.queryCompany(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginIndex(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((driverSearchModule.queryCompany(
                        toJson("")) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginAccountExtra(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginAccountExtra(token)
                    (open_id)((company \ "company_name").asOpt[String].get)(auth)
                    (AuthModule.queryUserLstWithOpenID(open_id)))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginAccountNormalInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginAccountNormalInfo(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    (company)
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginAccountPeople(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginAccountPeople(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil)))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginAccountPsw(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginAccountPsw(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth))
            }
            else Redirect("/index")
        }
    }
    
    /**
     * Special Way Login Page
     */
    def swLoginAccountValidateInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginAccountValidateInfo(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    (company))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginCompanyList(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val product_lst = (companyProductModule.queryProduct(toJson(Map("status" -> toJson(0)))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginCompanyList(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles)
                    ((companyProductModule.queryProduct(
                        toJson(Map("status" -> toJson(0)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginCompleteInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginCompleteInfo(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(1)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginCompleteProduct(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginCompleteProduct(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigProductNameQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].
                        map (x => x).getOrElse(Nil))
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].
                        map (x => x).getOrElse(Nil))
                    ((companyProductModule.queryProduct(
                        toJson(Map("open_id" -> toJson(open_id), "status" ->toJson(1)))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginDriverList(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginDriverList(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companySearchModule.queryDrivers(
                        toJson("")) \ "result").
                        asOpt[List[JsValue]].get)
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginRecruitment(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginRecruitment(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("status" -> toJson(0)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSendInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.swLoginSendInfo(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSendProduct(t : String) = Action { request =>
      
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginSendProduct(xmlOpt.allCities)(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigProductNameQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[String]].map (x => x).getOrElse(Nil))
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    (ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSentInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginSentInfo(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(0)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSentProduct(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val open_id = (company \ "open_id").asOpt[String].get
                Ok(views.html.swLoginSentProduct(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigProductNameQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[String]].map (x => x).getOrElse(Nil))
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    ((companyProductModule.queryProduct(
                        toJson(Map("open_id" -> toJson(open_id), "status" ->toJson(0) ))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSpecialWayList(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.speicalwayBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val com_lst = (driverSearchModule.queryCompany(toJson(Map("type" -> 3))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginSpecialWayList(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles)
                    ((driverSearchModule.queryCompany(
                        toJson(Map("type" -> 3))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginRewards(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                val result = (PlatformLinesModule.platformLineQuery(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginRewards(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((PlatformLinesModule.platformLineQuery(
                        toJson("")) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }
    
    def swSearchCompanyHtml = Action { request => 
        try {
  			    request.body.asJson.map { x => 
                val result = (driverSearchModule.queryCompany(x) \ "result").asOpt[List[JsValue]].get 
                Ok(views.html.specialway_company_search_result(result))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  		   
    }
}