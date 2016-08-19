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
import module.companyOpt.companyConfigModule

object companyProductModule {
    def pushProduct(data : JsValue) : JsValue = 
        try {
            val builder = MongoDBObject.newBuilder
            
            (data \ "origin").asOpt[JsValue].map { x =>
                val origin_builder = MongoDBObject.newBuilder
                origin_builder += "province" -> (x \ "origin_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "city" -> (x \ "origin_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "district" -> (x \ "origin_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                origin_builder += "address" -> (x \ "origin_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                builder += "origin" -> origin_builder.result
            }.getOrElse(throw new Exception("wrong input"))
            
            (data \ "destination").asOpt[JsValue].map { x =>
                val destination_builder = MongoDBObject.newBuilder
                destination_builder += "province" -> (x \ "destination_province").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "city" -> (x \ "destination_city").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "district" -> (x \ "destination_district").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                destination_builder += "address" -> (x \ "destination_address").asOpt[String].map (y => y).getOrElse(throw new Exception("wrong input"))
                builder += "destination" -> destination_builder.result
            }.getOrElse(throw new Exception("wrong input"))
           
            builder += "vehicle" -> (data \ "vehicle").asOpt[List[String]].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "vehicle_length" -> (data \ "vehicle_length").asOpt[List[Float]].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "weight" -> (data \ "weight").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "volume" -> (data \ "volume").asOpt[Float].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            builder += "date_requirement" -> (data \ "date_requirement").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "notes" -> (data \ "notes").asOpt[String].map (x => x).getOrElse("")
            
            val seed = "alfred yang"
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            builder += "product_id" -> Sercurity.md5Hash(seed + Sercurity.getTimeSpanWithMillSeconds)
            builder += "open_id" -> open_id

            val product_name = (data \ "product_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val product_name_save = (data \ "product_name_save").asOpt[Int].map (x => x).getOrElse(0)
            if (product_name_save == 1 && !product_name.equals("")) {
                companyConfigModule.companyConfigProductNamePush(toJson(Map("open_id" -> open_id, "pdn" -> product_name)))
            }
            
            val contact_name = (data \ "contact_name").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_phone = (data \ "contact_phone").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val contact_save = (data \ "contact_save").asOpt[Int].map (x => x).getOrElse(0)
            if (contact_save == 1 && !contact_name.equals("") && !contact_name.equals("")) {
                companyConfigModule.companyConfigContactPush(toJson(Map("open_id" -> open_id, "contact_name" -> contact_name, "contact_phone" -> contact_phone)))
            }
            
            _data_connection.getCollection("products") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def popProduct(data : JsValue) : JsValue = {
        null
    }
    
    def updateProduct(data : JsValue) : JsValue = {
        null
    }
    
    def queryProduct(data : JsValue) : JsValue = {
        null
    }
    
    def product2JsValue(x : MongoDBObject) : JsValue = {
        null 
    }  
}