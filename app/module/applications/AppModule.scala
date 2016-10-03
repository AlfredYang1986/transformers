package module.applications

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
import module.auth.AuthModule

import java.util.Date

object applicationStatus {  // auth  indicate weather account is approved 
    case object pushed extends applicationStatusDefines(0, "progressing")
    case object approved extends applicationStatusDefines(1, "approved")
    case object rejected extends applicationStatusDefines(2, "rejected")
}

sealed abstract class applicationStatusDefines(val t : Int, val des : String)

object applicationTypes {  // auth  indicate weather account is approved 
    case object company_registration extends applicationTypeDefines(0, "cr")
    case object driver_registration extends applicationTypeDefines(1, "dr")
    case object company_update extends applicationTypeDefines(2, "cu")
    case object driver_update extends applicationTypeDefines(2, "du")
}

sealed abstract class applicationTypeDefines(val t : Int, val des : String)

object AppModule {
  
    def DB2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("apply_id" -> toJson(x.getAs[String]("apply_id").get),
                   "open_id" -> toJson(x.getAs[String]("open_id").get),
                   "company_name" -> toJson(x.getAs[String]("company_name").get),
                   "status" -> toJson(x.getAs[Number]("status").get.intValue),
                   "date" -> toJson(x.getAs[Number]("date").get.floatValue)))
                   
    def Detail2JsValue(x : MongoDBObject) : JsValue = 
        toJson(Map("apply_id" -> toJson(x.getAs[String]("apply_id").get),
                   "open_id" -> toJson(x.getAs[String]("open_id").get),
                   "company_name" -> toJson(x.getAs[String]("company_name").get),
                   "content" -> toJson(toJson(x.getAs[String]("content").get)),
                   "status" -> toJson(x.getAs[Number]("status").get.intValue),
                   "date" -> toJson(x.getAs[Number]("date").get.floatValue)))
  
    def pushApplication(data : JsValue) : JsValue = { 
        try {
            val apply_id = Sercurity.md5Hash(data.toString + Sercurity.getTimeSpanWithMillSeconds)
            val open_id = (data \ "open_id").asOpt[String].map (x => x).getOrElse("")
            val company_name = (data \ "company_name").asOpt[String].map (x => x).getOrElse("")
            val content = (data \ "content").asOpt[JsValue].map (x => x.toString).getOrElse(throw new Exception("wrong input"))
            val apply_type = (data \ "apply_type").asOpt[Int].map (x => x).getOrElse(throw new Exception("wrong input"))

            val builder = MongoDBObject.newBuilder
            builder += "apply_id" -> apply_id
            builder += "open_id" -> open_id
            builder += "content" -> content
            builder += "company_name" -> company_name
            builder += "date" -> new Date().getTime
            builder += "status" -> applicationStatus.pushed.t
            builder += "apply_type" -> apply_type
            
            _data_connection.getCollection("applications") += builder.result
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(builder.result))))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def popApplication(data : JsValue) : JsValue = {
        try {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
            
            (from db() in "applications" where ("apply_id" -> apply_id) select (x => x)).toList match {
              case head :: Nil => {
                  _data_connection.getCollection("applications") -= head
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(DB2JsValue(head))))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def approveApplication(data : JsValue) : JsValue = {
        try {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
         
            (from db() in "applications" where ("apply_id" -> apply_id) select (x => x)).toList match {
              case head :: Nil => { 
                  head += "status" -> applicationStatus.approved.t.asInstanceOf[Number]
                 
                  val content = toJson(head.getAs[String]("content").get)
                  val result = head.getAs[Number]("apply_type").get.intValue match {
                    case 0 => (AuthModule.register(content) \ "status").asOpt[String].get
                    case 1 => (AuthModule.driverRegister(content) \ "status").asOpt[String].get
                    case 2 => (AuthModule.updateProfile(content) \ "status").asOpt[String].get
                    case 3 => (AuthModule.updateDriverProfile(content) \ "status").asOpt[String].get
                  }
                  
                  if (result.equals("ok")) {
                      // TODO: send sms to applyer
                      _data_connection.getCollection("applications").update(DBObject("apply_id" -> apply_id), head)
                      toJson(Map("status" -> toJson("ok"), "result" -> toJson(Detail2JsValue(head))))
                  } else {
                    throw new Exception("wrong input")
                  }
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def rejectApplication(data : JsValue) : JsValue = {
        try {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
         
            (from db() in "applications" where ("apply_id" -> apply_id) select (x => x)).toList match {
              case head :: Nil => { 
                  head += "status" -> applicationStatus.rejected.t.asInstanceOf[Number]
                  
                  // TODO: send message to phone
                  
                  _data_connection.getCollection("applications").update(DBObject("apply_id" -> apply_id), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(Detail2JsValue(head))))
              }
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryApplicationDetail(data : JsValue) : JsValue = {
        try {
            val apply_id = (data \ "apply_id").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
         
            (from db() in "applications" where ("apply_id" -> apply_id) select (x => x)).toList match {
              case head :: Nil => 
                toJson(Map("status" -> toJson("ok"), "result" -> toJson(Detail2JsValue(head))))
              case _ => throw new Exception("not existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def queryApplications(data : JsValue) : JsValue = {
        try {
            def orderCol = "date"
            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(20)
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
            val app_status = (data \ "app_status").asOpt[Int].map (x => "status" -> x).getOrElse(DBObject)
            val app_type = (data \ "apply_type").asOpt[Int].map (x => "apply_type" -> x).getOrElse(DBObject)
           
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                (from db() in "application" where (app_status, app_type)).selectSkipTop(skip)(take)(orderCol)(DB2JsValue(_)).toList)))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
}