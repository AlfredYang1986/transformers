package module.system.config

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object ConfigModule {
    def configVehicleInit = { 
        val builder = MongoDBObject.newBuilder
        builder += "index" -> 1
        builder += "vehicle" -> ("平板车" :: "高栏车" :: "厢式车" :: "半挂车" :: "高低板" :: "起重车" :: "牵引车" :: "仓棚车" :: "罐式车" :: "特种车" :: "危险品车" :: "保温冷藏" :: "集装箱车" :: Nil)
        builder += "vehicle_length" -> (4.2 :: 4.5 :: 5.0 :: 6.2 :: 6.9 :: 7.2 :: 7.7 :: 7.8 :: 8.2 :: 8.6 :: 8.7 :: 9.6 :: 11.7 :: 12.5 :: 13.0 :: 13.5 :: 14.0 :: 16.0 :: 17.0 :: 17.5 :: 18.0 :: Nil) 
        _data_connection.getCollection("system_config") += builder.result
    }
    
    def configAllVehicles : JsValue = {
        val lst = (from db() in "system_config" where ("index" -> 1) select (x => x)).toList
        lst match {
          case head :: Nil => {
              toJson(Map("vehicle" -> toJson(head.getAs[MongoDBList]("vehicle").get.toList.asInstanceOf[List[String]]),
                         "vehicle_length" -> toJson(head.getAs[MongoDBList]("vehicle_length").get.toList.asInstanceOf[List[Double]])))
          }
          case _ => ???
        }
    }
}