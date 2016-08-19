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
import java.util.Calendar

import module.auth.registerTypes
import module.auth.AuthModule
import module.sercurity.Sercurity

object companySearchModule {
    def queryDrivers(data : JsValue) : JsValue = {
        
        def basicCondition = "type" $eq registerTypes.driver.t
    
        def cityConditions : Option[DBObject] = 
            try {
                val origin_province = (data \ "origin_province").asOpt[String].map (x => x).getOrElse(throw new Exception)
                val origin_city = (data \ "origin_city").asOpt[String].map (x => x).getOrElse(throw new Exception)
                val destination_province = (data \ "destination_province").asOpt[String].map (x => x).getOrElse(throw new Exception)
                val destination_city = (data \ "destination_city").asOpt[String].map (x => x).getOrElse(throw new Exception)
                Some("driver_lines" $elemMatch ($and($and("destination_province" $eq destination_province, "destination_city" $eq destination_city), $and("origin_province" $eq origin_province, "origin_city" $eq origin_city))))
      
            } catch {
              case ex : Exception => None
            }
        
        def vehicleConditions : Option[DBObject] = 
            try {
                val vehicle = (data \ "vehicle").asOpt[List[String]].map (x => x).getOrElse(throw new Exception)
                Some(DBObject("vehicle" -> vehicle))
              
            } catch {
              case ex : Exception => None
            }
        
        def vehicleLengthConditions : Option[DBObject] = 
            try {
                val length = (data \ "vehicle_length").asOpt[List[Float]].map (x => x).getOrElse(throw new Exception)
                var reVal : DBObject = null
                length map { x =>
                  if (reVal == null) reVal = DBObject("vehicle_length" -> x)
                  else reVal = $or(reVal, DBObject("vehicle_length" -> x))
                }
                if (reVal != null) Some(reVal)
                else None
              
            } catch {
              case ex : Exception => None
            }
        
        def conditionsAcc(o : DBObject, a : Option[DBObject]) : DBObject = a match {
              case Some(x) => $and(o, x)
              case None => o
            }
        
        def orderCol = "date"
        
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
        val conditions = conditionsAcc(conditionsAcc(conditionsAcc(basicCondition, cityConditions), vehicleConditions), vehicleLengthConditions)
        
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            (from db() in "user_profile" where conditions select (AuthModule.detailResult(_))).toList)))
    }
    

    

}