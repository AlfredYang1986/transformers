package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import module.auth.AuthModule.{ queryUserWithToken, queryProfileCondition }
import module.auth.authTypes
import module.auth.registerTypes
import module.auth.authStatus

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
                  case registerTypes.company.des => registerTypes.company.t
                  case registerTypes.driver.des => registerTypes.driver.t
                  case registerTypes.industry.des => registerTypes.industry.t
                  case registerTypes.specialway.des => registerTypes.specialway.t
                  case _ => throw new Exception("error input")
                }
                
                val st = s match {
                  case authStatus.approved.des => authStatus.approved.t
                  case authStatus.progress.des => authStatus.progress.t
                  case authStatus.rejected.des => authStatus.rejected.t
                  case _ => throw new Exception("error input")
                }
                
                val pt = p.toInt
                
                val data = toJson(Map("skip" -> toJson(pt * 10), "take" -> toJson(10), "type" -> toJson(kt), "auth_status" -> toJson(st)))
                val reVal = queryProfileCondition("", "", data)
                
                val result = (reVal \ "status").asOpt[String].get match {
                  case "ok" => (reVal \ "result").asOpt[List[JsValue]].get
                  case "error" => { println(1233); throw new Exception((reVal \ "error" \ "message").asOpt[String].get)}
                }

                if ((user \ "auth").asOpt[Int].get > authTypes.adminBase.t) Ok(views.html.admin_profiles((user \ "token").asOpt[String].get)(result)(kt))
//              else Redirect("/admin/login")
                else Ok("只有管理员才有这个操作权限")
            
            } catch {
              case ex : Exception => { ex.getStackTrace foreach (x => println(x)); Ok(ex.getMessage) }
            }
            
        }    
    }
}