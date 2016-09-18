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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                val com_lst = (driverSearchModule.queryCompany(toJson("")) \ "result").asOpt[List[JsValue]].get
                val following_lst = (driverFollowModule.queryDriverFollowingLst(toJson(Map("driver_open_id" -> open_id))) \ "result").asOpt[List[String]].get
                Ok(views.html.swLoginIndex(token)(open_id)(name)(com_lst)(following_lst))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get

            val user_lst = AuthModule.queryUserLstWithOpenID(open_id)
           
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                Ok(views.html.swLoginAccountExtra(token)(open_id)(name)(user_lst))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get

            val vc = ConfigModule.configAllVehicles
           
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                Ok(views.html.swLoginAccountNormalInfo(token)(open_id)(name)(company)(xmlOpt.allCities)(vc))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
                Ok(views.html.swLoginAccountPeople(token)(open_id)(name)(contacts))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                Ok(views.html.ciLoginAccountPsw(token)(open_id)(name))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                Ok(views.html.swLoginAccountValidateInfo(token)(open_id)(name)(company))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            val vc = ConfigModule.configAllVehicles
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                val product_lst = (companyProductModule.queryProduct(toJson("")) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginCompanyList(token)(open_id)(name)(xmlOpt.allCities)(vc)(product_lst))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            val vc = ConfigModule.configAllVehicles
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                val infos = (companyInfoModule.queryInfo(toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(1)))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginCompleteInfo(token)(open_id)(name)(infos))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginCompleteProduct = Action {
        Ok(views.html.swLoginCompleteProduct("Your new application is ready."))
    }

    /**
     * Special Way Login Page
     */
    def swLoginDriverList(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        val user = AuthModule.queryUserWithToken(token)
        val company = AuthModule.queryInstanceWithToken(token)

        val open_id = (company \ "open_id").asOpt[String].get
        val name = (company \ "company_name").asOpt[String].get
        val vc = ConfigModule.configAllVehicles
        val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
        val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val dir_lst = (companySearchModule.queryDrivers(toJson("")) \ "result").asOpt[List[JsValue]].get
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
                Ok(views.html.swLoginDriverList(token)(open_id)(name)(dir_lst)(xmlOpt.allCities)(vc))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginRecruitment = Action {
        Ok(views.html.swLoginRecruitment("Your new application is ready."))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
           
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                Ok(views.html.swLoginSendInfo(token)(open_id)(name))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get

            val pdns = (companyConfigModule.companyConfigProductNameQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[String]].map (x => x).getOrElse(Nil)
            val contacts = (companyConfigModule.companyConfigContactQuery(toJson(Map("open_id" -> open_id))) \ "result").asOpt[List[JsValue]].map (x => x).getOrElse(Nil)
           
            val vc = ConfigModule.configAllVehicles
            
            if ((user \ "auth").asOpt[Int].get > authTypes.speicalwayBase.t) {
//                Ok(views.html.swLoginSendProduct(xmlOpt.allCities)(token)(open_id)(name)(pdns)(contacts)(vc))
              Ok("请先登陆在进行有效操作")
//                Ok(views.html.swLoginSendProduct(xmlOpt.allCities)(token)(open_id)(name)(pdns)(contacts)(vc))
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
            val company = AuthModule.queryInstanceWithToken(token)

            val open_id = (company \ "open_id").asOpt[String].get
            val name = (company \ "company_name").asOpt[String].get
            
            if ((user \ "auth").asOpt[Int].get > authTypes.companyBase.t) {
                val infos = (companyInfoModule.queryInfo(toJson(Map("open_id" -> toJson(open_id), "status" -> toJson(0)))) \ "result").asOpt[List[JsValue]].get
                Ok(views.html.swLoginSentInfo(token)(open_id)(name)(infos))
            }
            else Redirect("/index")
        }
    }

    /**
     * Special Way Login Page
     */
    def swLoginSentProduct = Action {
        Ok(views.html.swLoginSentProduct("Your new application is ready."))
    }

    /**
     * Special Way Login Page
     */
    def swLoginSpecialWayList = Action {
        Ok(views.html.swLoginSpecialWayList("Your new application is ready."))
    }

    /**
     * Special Way Login Page
     */
    def swLoginRewards = Action {
        Ok(views.html.swLoginRewards("Your new application is ready."))
    }
}