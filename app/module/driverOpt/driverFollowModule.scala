package module.driverOpt

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

object driverFollowModule {
    def driverFollowCompany(data : JsValue) : JsValue = {
        try {
            val driver_open_id = (data \ "driver_open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val target_open_id = (data \ "target_open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
           
            (from db() in "driver_company" where ("driver_open_id" -> driver_open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val following_lst = head.getAs[MongoDBList]("followings").get.toList.asInstanceOf[List[String]]
                  head += "followings" -> (target_open_id :: following_lst).distinct
                  println((target_open_id :: following_lst).distinct)
                  
                  _data_connection.getCollection("driver_company").update(DBObject("driver_open_id" -> driver_open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("follow success")))
              }
              case Nil => {
                  val builder = MongoDBObject.newBuilder
                  builder += "driver_open_id" -> driver_open_id
                  builder += "followings" -> (target_open_id :: Nil)
                  println(target_open_id :: Nil)
                  
                  _data_connection.getCollection("driver_company") += builder.result
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("follow success")))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def driverUnFollowCompany(data : JsValue) : JsValue = {
        try {
            val driver_open_id = (data \ "driver_open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            val target_open_id = (data \ "target_open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "driver_company" where ("driver_open_id" -> driver_open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val following_lst = head.getAs[MongoDBList]("followings").get.toList.asInstanceOf[List[String]]
                  head += "followings" -> following_lst.filterNot (x => target_open_id.equals(x))
                  
                  _data_connection.getCollection("driver_company").update(DBObject("driver_open_id" -> driver_open_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("follow success")))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryDriverFollowingLst(data : JsValue) :JsValue = {
        try {
            val driver_open_id = (data \ "driver_open_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "driver_company" where ("driver_open_id" -> driver_open_id) select (x => x)).toList match {
              case head :: Nil => {
                  val following_lst = head.getAs[MongoDBList]("followings").get.toList.asInstanceOf[List[String]]
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(following_lst)))
              }
              case Nil => toJson(Map("status" -> toJson("ok"), "result" -> toJson(List[String]())))
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => throw new Exception(ex.getMessage)
        }
    }
    
    def queryCompanyFollowedLst(data : JsValue) : JsValue = {
        null
    }
}