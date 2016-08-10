package module.companyOpt

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity
import module.sms.smsModule

import java.util.Date

import module.auth.registerTypes
import module.auth.AuthModule

object companySearchModule {
    def queryDrivers(data : JsValue) : JsValue = {
        
        def basicCondition = "type" $eq registerTypes.driver.t
     
        def conditionsAcc(o : DBObject, key : String, value : Any) : DBObject = {
            key match {
              case "address" => $and(o, "address" $eq value.asInstanceOf[String])
            }
        }

        def conditions : DBObject = {
            var reVal : DBObject = basicCondition
            (data \ "address").asOpt[String].map (x => 
                reVal = conditionsAcc(reVal, "address", x)
            ).getOrElse(Unit)
            
            reVal
        }
        
        def orderCol = "date"
        
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
        
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "user_profile" where conditions select (AuthModule.detailResult(_))).toList)))
    }
}