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
   
        def floatdismiss = 0.0001
        def conditionsAcc(o : DBObject, key : String, value : Any) : DBObject = key match {
              case "address" => $and(o, "address" $eq value.asInstanceOf[String])
              case "type" => $and(o, "type" $eq value.asInstanceOf[Int])
              case "line" => {
                  val origin_province = (value.asInstanceOf[JsValue] \ "origin_province").asOpt[String].get
                  val origin_city = (value.asInstanceOf[JsValue] \ "origin_city").asOpt[String].get
                  val destination_province = (value.asInstanceOf[JsValue] \ "destination_province").asOpt[String].get
                  val destination_city = (value.asInstanceOf[JsValue] \ "destination_city").asOpt[String].get
                  
                  $and(o, ("driver_lines" $elemMatch 
                          $and(("origin_province" $eq origin_province) :: 
                               ("origin_city" $eq origin_city) :: 
                               ("destination_province" $eq destination_province) ::
                               ("destination_city" $eq destination_city) :: Nil))
                      )
              }
              case "vehicle" => {
                  val lst = value.asInstanceOf[List[String]]
                  val con = lst map { str =>
                      "vehicle" $eq str
                  }
                  $and (o, $or(con))
              }
              case "vehicle_length" => {
                  val lst = value.asInstanceOf[List[Float]]
                  val con = lst map { f =>
                      $and("vehicle_length" $gte (f - floatdismiss),
                           "vehicle_length" $lte (f + floatdismiss))
                  }
                  $and (o, $or(con))
              }
            }
      
        def conditions : DBObject = {
            var reVal : DBObject = basicCondition
            (data \ "address").asOpt[String].map (x => 
                reVal = conditionsAcc(reVal, "address", x)
            ).getOrElse(Unit)
           
            (data \ "type").asOpt[Int].map (x => 
                reVal = conditionsAcc(reVal, "type", x)
            ).getOrElse(Unit)
           
            (data \ "line").asOpt[JsValue].map { x =>
                reVal = conditionsAcc(reVal, "line", x)
            }.getOrElse(Unit)
            
            (data \ "vehicle").asOpt[List[String]].map { x =>
                reVal = conditionsAcc(reVal, "vehicle", x)
            }.getOrElse(Unit)
           
            (data \ "vehicle_length").asOpt[List[Float]].map { x =>
                reVal = conditionsAcc(reVal, "vehicle_length", x)
            }
            
            reVal
        }

        def orderCol = "date"
        
        val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
        val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
       
        toJson(Map("status" -> toJson("ok"), "result" -> toJson(
            ((from db() in "user_profile" where conditions).selectSkipTop(skip)(take)(orderCol) 
                (AuthModule.detailResult(_))).toList)))
    }
    

    

}