package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.companyOpt.{ companySearchModule, companyInfoModule, companyProductModule, companyConfigModule } 
import module.driverOpt.driverSearchModule
import module.common.xml.xmlOpt
import module.auth.AuthModule
import module.auth.authTypes
import module.system.config.ConfigModule
import module.platformLines.PlatformLinesModule

object CompanyIndustryController extends Controller {
  
    /**
     * Company Login Page
     */
    def ciLoginIndex(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.companyBase.t) {
              val company = AuthModule.queryInstanceWithToken(token)
              Ok(views.html.ciLoginIndex(
                     (companySearchModule.queryDrivers(toJson("")) \ "result").asOpt[List[JsValue]].get)
                     (token)((company \ "open_id").asOpt[String].get)
                     ((company \ "company_name").asOpt[String].get)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountExtra(t : String) = Action { request =>
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
                val user_lst = AuthModule.queryUserLstWithOpenID(open_id)
                Ok(views.html.ciLoginAccountExtra(token)(open_id)
                       ((company \ "company_name").asOpt[String].get)(auth)
                       (AuthModule.queryUserLstWithOpenID(open_id)))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountNormalInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginAccountNormalInfo(token)
                      ((company \ "open_id").asOpt[String].get)
                      ((company \ "company_name").asOpt[String].get)(company)
                      (xmlOpt.allCities)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountPeople(t : String) = Action { request =>
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
                Ok(views.html.ciLoginAccountPeople(token)
                    (open_id)((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil)))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountProductName(t : String) = Action { request =>
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
                Ok(views.html.ciLoginAccountProductName(token)
                      (open_id)((company \ "company_name").asOpt[String].get)(auth)
                      ((companyConfigModule.companyConfigProductNameQuery(
                          toJson(Map("open_id" -> open_id))) \ "result").
                          asOpt[List[String]].map (x => x).getOrElse(Nil)))
            }
            else Redirect("/index")
        }
    }


    /**
     * Company Login Page
     */
    def ciLoginAccountPsw(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginAccountPsw(token)
                      ((company \ "open_id").asOpt[String].get)
                      ((company \ "company_name").asOpt[String].get)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginAccountValidateInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginAccountValidateInfo(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)
                    (company)(xmlOpt.allCities)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginCompanyList(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginCompanyList(token)
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
     * Company Login Page
     */
    def ciLoginCompleteInfo(t : String) = Action { request =>
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
                Ok(views.html.ciLoginCompleteInfo(token)
                    (open_id)((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("open_id" -> toJson(open_id), 
                                   "status" -> toJson(1)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginCompleteProduct(t : String) = Action { request => 
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
                Ok(views.html.ciLoginCompleteProduct(token)
                    (open_id)((company \ "company_name").asOpt[String].get)(auth)
                    ((companyConfigModule.companyConfigProductNameQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[String]].map (x => x).getOrElse(Nil))
                    ((companyConfigModule.companyConfigContactQuery(
                        toJson(Map("open_id" -> open_id))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    ((companyProductModule.queryProduct(
                        toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(1)))) \ "result").
                        asOpt[List[JsValue]].map (x => x).getOrElse(Nil))
                    (xmlOpt.allCities)(ConfigModule.configAllVehicles))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginDriverList(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        val user = AuthModule.queryUserWithToken(token)
        val auth = (user \ "auth").asOpt[Int].get

        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginDriverList(token)
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
     * Company Login Page
     */
    def ciLoginRecruitment(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
            
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginRecruitment(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("status" -> 0))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginSendInfo(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginSendInfo(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginSendProduct(t : String) = Action { request =>
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
                Ok(views.html.ciLoginSendProduct(xmlOpt.allCities)(token)(open_id)
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
     * Company Login Page
     */
    def ciLoginSentInfo(t : String) = Action { request =>
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
                Ok(views.html.ciLoginSentInfo(token)(open_id)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((companyInfoModule.queryInfo(
                        toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(0)))) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginSentProduct(t : String) = Action { request =>
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
                Ok(views.html.ciLoginSentProduct(token)(open_id)
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
     * Company Login Page
     */
    def ciLoginSpecialWayList(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get

            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginSpecialWayList(token)
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
     * Company Login Page
     */
    def ciLoginRewards(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            val auth = (user \ "auth").asOpt[Int].get
           
            if (auth > authTypes.companyBase.t) {
                val company = AuthModule.queryInstanceWithToken(token)
                Ok(views.html.ciLoginRewards(token)
                    ((company \ "open_id").asOpt[String].get)
                    ((company \ "company_name").asOpt[String].get)(auth)
                    ((PlatformLinesModule.platformLineQuery(
                        toJson("")) \ "result").
                        asOpt[List[JsValue]].get))
            }
            else Redirect("/index")
        }
    }

    def companySearchDriver = Action (request => requestArgs(request)(companySearchModule.queryDrivers))
    def companyInfoPush = Action (request => requestArgs(request)(companyInfoModule.pushInfo))
    def companyInfoUpdate = Action (request => requestArgs(request)(companyInfoModule.updateInfo))
    def companyInfoPop = Action (request => requestArgs(request)(companyInfoModule.popInfo))
    def companyInfoQuery = Action (request => requestArgs(request)(companyInfoModule.queryInfo))
    def companyProductPush = Action (request => requestArgs(request)(companyProductModule.pushProduct))
    def companyProductUpdate = Action (request => requestArgs(request)(companyProductModule.updateProduct))
    def companyProductPop = Action (request => requestArgs(request)(companyProductModule.popProduct))
    def companyProductQuery = Action (request => requestArgs(request)(companyProductModule.queryProduct))
    def companyProductNamePush = Action (request => requestArgs(request)(companyConfigModule.companyConfigProductNamePush))
    def companyProductNamePop = Action (request => requestArgs(request)(companyConfigModule.companyConfigProductNamePop))
    def companyProductNameUpdate = Action (request => requestArgs(request)(companyConfigModule.companyConfigProductNameUpdate))
//    def companyProductNameQuery = Action (request => requestArgs(request)(companyConfigModule.companyConfigProductNameQuery))
    def companyContactPush = Action (request => requestArgs(request)(companyConfigModule.companyConfigContactPush))
    def companyContactPop = Action (request => requestArgs(request)(companyConfigModule.companyConfigContactPop))
    def companyContactUpdate = Action (request => requestArgs(request)(companyConfigModule.companyConfigContactUpdate))
//    def companyContactQuery = Action (request => requestArgs(request)(companyConfigModule.companyConfigProductNameUpdate))

    def companyProductQueryHtml = Action { request => 
        try {
  			    request.body.asJson.map { x => 
                val result = (companyProductModule.queryProduct(x) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.company_product_search_result(result))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  		   
    }
    
    def companySWQueryHtml = Action { request => 
        try {
  			    request.body.asJson.map { x => 
                val result = (driverSearchModule.queryCompany(x) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.specialway_product_search_result(result))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  		   
    }

    def companySearchDriverHtml = Action { request => // requestArgs(request)(companySearchModule.queryDrivers))
        try {
  			    request.body.asJson.map { x => 
                val result = (companySearchModule.queryDrivers(x) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.company_driver_search_result(result))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  		   
    }

    def companyAppendSentProductHtml = Action { request =>
        try {
  			    request.body.asJson.map { x =>
  			        val open_id = (x \ "open_id").asOpt[String].get
            
                val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
                val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
            
                val products = (companyProductModule.queryProduct(x) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
                val vc = ConfigModule.configAllVehicles
                val result = (driverSearchModule.queryCompany(x) \ "result").asOpt[List[JsValue]].get
                
                Ok(views.html.company_sent_products(pdns)(contacts)(products)(xmlOpt.allCities)(vc))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  
    }
    
    def companyAppendSentInfoHtml = Action { request =>
        try {
  			    request.body.asJson.map { x =>
                val infos = (companyInfoModule.queryInfo(x) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.company_sent_info(infos))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  
    }
    
    def companyAppendOtherInfoHtml = Action { request =>
        try {
  			    request.body.asJson.map { x =>
                val infos = (companyInfoModule.queryInfo(x) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.company_other_info_result(infos))
      			}.getOrElse (BadRequest("Bad Request for input"))
  	   	} catch {
  		   	case _ : Exception => BadRequest("Bad Request for input")
  		  }  
    }
}