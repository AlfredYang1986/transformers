package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import module.auth.AuthModule.{ queryUserWithToken, queryProfileCondition }
import module.auth.AuthModule
import module.auth.authTypes
import module.auth.registerTypes
import module.applications.applicationStatus
import module.applications.applicationTypes
import module.applications.AppModule
import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.auth.AuthModule

object AdminController extends Controller {
    def adminLogin = Action {
        Ok(views.html.admin_login())
    }  
  
    def index(t : String) = Action { request =>
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            val user = queryUserWithToken(token)
            if ((user \ "auth").asOpt[Int].get > authTypes.adminBase.t) Ok(views.html.admin_index())
            else Redirect("/admin/login")
        }
    }
    
    def profiles(k : String, s : String, p : String, t : String) = Action { request => 
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            try {
                val user = queryUserWithToken(token)
                val kt = k match {
                  case applicationTypes.company_registration.des => applicationTypes.company_registration.t
                  case applicationTypes.driver_registration.des => applicationTypes.driver_registration.t
                  case applicationTypes.company_update.des => applicationTypes.company_update.t
                  case applicationTypes.driver_update.des => applicationTypes.driver_update.t
                  case _ => throw new Exception("error input")
                }
                
                val st = s match {
                  case applicationStatus.pushed.des => applicationStatus.pushed.t
                  case applicationStatus.approved.des => applicationStatus.approved.t
                  case applicationStatus.rejected.des => applicationStatus.rejected.t
                  case _ => throw new Exception("error input")
                }
                
                val pt = p.toInt
                
                val data = toJson(Map("apply_type" -> toJson(kt), "apply_status" -> toJson(st)))
                val reVal = AppModule.queryApplications(data)
                
                val result = (reVal \ "status").asOpt[String].get match {
                  case "ok" => (reVal \ "result").asOpt[List[JsValue]].get
                  case "error" => throw new Exception((reVal \ "error" \ "message").asOpt[String].get)
                }
                
                if ((user \ "auth").asOpt[Int].get > authTypes.adminBase.t) Ok(views.html.admin_profiles(token)(result)(kt))
                else Ok("只有管理员才有这个操作权限")
            
            } catch {
              case ex : Exception => { ex.getStackTrace foreach (x => println(x)); Ok(ex.getMessage) }
//              case ex : Exception => Ok(ex.getMessage)
            }
        }    
    }
    
    def adminProfileDetail(a : String, t : String) = Action { request =>  
        var token = t
        if(token == "") token = request.cookies.get("token").map (x => x.value).getOrElse("")
        else Unit
        
        if (token == "") Ok("请先登陆在进行有效操作")
        else {
            try {
                val user = queryUserWithToken(token)
                
                val (content, apply_id, date, open_id, status, company_name) = (AppModule.queryApplicationDetail(toJson(Map("apply_id" -> a))) \ "result").asOpt[JsValue] match {
                  case Some(x) => 
                       (Json.parse((x \ "content").asOpt[String].get),
                        a, (x \ "date").asOpt[JsValue].get,
                        (x \ "open_id").asOpt[String].get,
                        (x \ "status").asOpt[Int].get,
                        (x \ "company_name").asOpt[String].get)
                  case None => throw new Exception("wrong input")
                }
             
                val previous = if (open_id == "") None
                               else Some(AuthModule.queryProfile(open_id)) 
                                 
                if ((user \ "auth").asOpt[Int].get > authTypes.adminBase.t) previous match {
                  case None => Ok(views.html.admin_profile_register_detail(token)(apply_id)(content)(date)(status))
                  case Some(x) => Ok("未实现")
                }
                else Ok("只有管理员才有这个操作权限")
            
            } catch {
//              case ex : Exception => { ex.getStackTrace foreach (x => println(x)); Ok(ex.getMessage) }
              case ex : Exception => Ok(ex.getMessage)
            }
        }        
    }

    def adminSendCar = Action {
        Ok(views.html.adminSendCar())
    }

    def adminHaveSentCar = Action {
        Ok(views.html.adminHaveSentCar())
    }

    def adminCarHistory = Action {
        Ok(views.html.adminCarHistory())
    }

    def adminSetting = Action {
        Ok(views.html.adminSetting())
    }
}