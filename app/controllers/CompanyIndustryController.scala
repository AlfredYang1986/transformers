package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import controllers.common.requestArgsQuery._

import module.companyOpt.{ companySearchModule, companyInfoModule, companyProductModule, companyConfigModule } 
import module.common.xml.xmlOpt
import module.auth.AuthModule
import module.auth.authTypes
import module.system.config.ConfigModule

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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
              val dir_lst = (companySearchModule.queryDrivers(toJson("")) \ "result").asOpt[List[JsValue]].get
              Ok(views.html.ciLoginIndex(dir_lst)(token)(open_id)(name))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            val user_lst = AuthModule.queryUserLstWithOpenID(open_id)
            println(user_lst)
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountExtra(token)(open_id)(name)(user_lst))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountNormalInfo(token)(open_id)(name)(company)(xmlOpt.allCities))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountPeople(token)(open_id)(name)(contacts))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountProductName(token)(open_id)(name)(pdns))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountPsw(token)(open_id)(name))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginAccountValidateInfo(token)(open_id)(name)(company)(xmlOpt.allCities))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginCompanyList = Action {
        Ok(views.html.ciLoginCompanyList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginCompleteInfo = Action {
        Ok(views.html.ciLoginCompleteInfo("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginCompleteProduct = Action {
        Ok(views.html.ciLoginCompleteProduct("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginDriverList(t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = AuthModule.queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginDriverList(xmlOpt.allCities))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginRecruitment = Action {
        Ok(views.html.ciLoginRecruitment("Your new application is ready."))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginSendInfo(token)(open_id)(name))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get

            val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
            val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
           
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginSendProduct(xmlOpt.allCities)(token)(open_id)(name)(pdns)(contacts))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            val infos = (companyInfoModule.queryInfo(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginSentInfo(token)(open_id)(name)(infos))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
            val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
            
            val products = (companyProductModule.queryProduct(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
            val vc = ConfigModule.configAllVehicles
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.ciLoginSentProduct(token)(open_id)(name)(pdns)(contacts)(products)(xmlOpt.allCities)(vc))
            }
            else Redirect("/index")
        }
    }

    /**
     * Company Login Page
     */
    def ciLoginSpecialWayList = Action {
        Ok(views.html.ciLoginSpecialWayList("Your new application is ready."))
    }

    /**
     * Company Login Page
     */
    def ciLoginRewards = Action {
        Ok(views.html.ciLoginRewards("Your new application is ready."))
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
}